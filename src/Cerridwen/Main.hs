{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
module Main (main) where
import qualified Control.Exception as X
import Control.Monad
import Data.List
import Data.Maybe
import Data.Word
import Options.Applicative
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map as Map
import qualified Control.Lens as LE
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ElfEdit as Elf
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import qualified Data.Parameterized.Classes as PC
import           Data.Parameterized.Some ( Some(..) )
import           System.Directory
import           System.FilePath.Posix
import qualified System.FilePath.Glob as SFG
import qualified Data.Macaw.Memory as MM
import qualified Data.Macaw.Discovery as MD
import qualified Data.Macaw.X86 as MX
import Utils
import CLI
import StrandX86
import Hashing () -- New instances of Hashable
import Corpus


---------------
-- Cerridwen --
---------------

-- Inspired by Symbolic.Testing/runDiscovery
cerrDiscover:: Elf.ElfHeaderInfo 64 -> IO (MM.Memory 64, [Some (MD.DiscoveryFunInfo MX.X86_64)])
cerrDiscover ehi = do
  let toEntryPoints = toAddrSymMap -- (MM.Memory w -> [MEL.MemSymbol w] -> Map.Map (MM.MemSegmentOff w) BS.ByteString), default could be toAddrSymMap
  let archInfo = MX.x86_64_linux_info -- MAI.ArchitechtureInfo
  (mem, nameAddrList) <- loadELF ehi
  let addrSymMap = toEntryPoints mem nameAddrList
  let ds0 = MD.emptyDiscoveryState mem addrSymMap archInfo
  fns <- T.forM (Map.keys addrSymMap) $ \entryPoint -> do
    let (_ds1, dfi) = MD.analyzeFunction entryPoint MD.UserRequest ds0
    return dfi
  return (mem, fns)

-- cerrDecode: From a FilePath to a 64 bit Elf
cerrDecode:: FilePath -> IO (Maybe (Elf.ElfHeaderInfo 64))
cerrDecode exePath = do
  bytes <- BS.readFile exePath
  case Elf.decodeElfHeaderInfo bytes of
    Left (_, msg) -> do
      print ("Error parsing ELF header from file '" ++ show exePath ++ "': " ++ msg)
      return Nothing
    Right (Elf.SomeElf ehi) -> do
      case Elf.headerClass (Elf.header ehi) of
        Elf.ELFCLASS32 -> do
          print "32 bit x86 binaries are not supported"
          return Nothing
        Elf.ELFCLASS64 ->
          return (Just ehi)         

-- Returns a set of the hash of the strands found
cerrHashStrands:: FilePath -> IO FreqMap
cerrHashStrands exePath = do
  mehi <- cerrDecode exePath
  case mehi of
    Nothing ->
      return emptyFreq
    Just ehi -> do
      (mem, funInfos) <- cerrDiscover ehi
      strandsBB <-  mapM (cerrFunToStrands mem) funInfos
      return (unionsFreq strandsBB)
  where cerrFunToStrands :: MM.Memory 64 -> Some (MD.DiscoveryFunInfo MX.X86_64) -> IO FreqMap
        cerrFunToStrands _mem (Some dfi) = do
          let pBM = LE.view MD.parsedBlocks dfi          
          let pBs = Map.elems pBM
          strands <- mapM (\pB -> do
                              let pBStmts  = (MD.pblockStmts pB) -- [(MC.Stmt arch ids)]                              
                              let hss = (map (\s -> (PC.hash s, s)) pBStmts) -- [(Int, MC.Stmt arch ids)]
                              let strands = findStrandsMain MX.x86_64_linux_info hss -- find strands from block
                              let strandbodies :: [HStrand] = map fst strands -- drop the statements for now
                              let strandhashes :: [Int] = map PC.hash strandbodies
                              return (fromListFreq strandhashes)) pBs
          return (unionsFreq strands)



-- cerrCachedHashStrands: Return a set of the hash of the strands found, potentially cached
cerrCachedHashStrands :: (FilePath -> FilePath) -> FilePath -> IO FreqMap
cerrCachedHashStrands cachedM exePath = do
  let cachedExePath = cachedM exePath
  hC <- X.catch (cerrLoadCorpus cachedExePath) (\ (_ :: X.SomeException) -> return (emptyCorpus False))
  if (frequency hC == emptyFreq)
    then do 
      hI <- cerrHashStrands exePath
      cerrSaveCorpus False True (Corpus False (IntMap.size hI) hI) cachedExePath
      return hI
    else return (frequency hC)

-- Filter a freqmap to only contain "significant" strands (i.e. that appears less than BOUND in CORPUS)
cerrBoundFreq:: Word16 -> Corpus -> FreqMap -> FreqMap
cerrBoundFreq bound corpus freq =
  let fqC = (frequency corpus)
  in IntMap.filterWithKey (\k _ -> maybe True (\f -> f <= bound) (IntMap.lookup k fqC)) freq



-- Find the minimum frequency in corpus required (bound) to capture threshold % of the strands
cerrFindBound :: Int -> Corpus -> [Int] -> IO Word16
cerrFindBound threshold corpus keys =
  let cFreq = frequency corpus
      sKF = map (\k -> (k, getFreqW16 cFreq k)) keys
      sKeys = sortOn snd sKF
      index = (((length keys) * threshold) `div` 100) - 1
      bound = snd (sKeys !! index)
  in do
    return bound

-- if t is in range, then find bound using corpus, otherwise
-- use b as bound if in range, and filter freqmap using bound
cerrBoundHash :: Int -> Int -> Corpus -> FreqMap -> IO (FreqMap, Bool)
cerrBoundHash t b hC hI = do
  bound <- if (1 <= t) && (t <= 100)
           then cerrFindBound t hC (IntMap.keys hI)
           else return (fromIntegral b)
--  print $ "With threshold "++ (show t) ++ " found bound " ++ (show bound)  
  let bounded = (1 <= bound) && (bound <= (maxBound::Freq))
  if bounded
    then return (cerrBoundFreq bound hC hI, bounded)
    else return (hI, bounded)

-- Discover every procedure in the corpus, and create a Corpus structure
cerrMakeCorpus :: Int -> String -> IO Corpus
cerrMakeCorpus n corpusPattern = do
  corpus <- SFG.glob corpusPattern
  hashes <- mapM cerrHashStrands corpus
  let fq = unionsFreq hashes
  let count = IntMap.size fq -- count is the number of unique strand in corpus
  return $ cerrOptNCorpus n (Corpus False count fq)

-- Create an unoptimize Corpus representing the strands found in a single executable    
cerrMakeSummary :: FilePath -> IO Corpus
cerrMakeSummary fp = do
  print $ "-- Processing " ++ fp
  hashes <- cerrHashStrands fp
  print $ " ---- cerrHashStrands DONE"
  return (Corpus False (IntMap.size hashes) hashes)

-- Append onto a raw summary on a raw corpus
cerrAppendSummary :: FilePath -> Corpus -> IO ()  
cerrAppendSummary f c = do
  print $ "-- Saving summary with " ++ (show (IntMap.size (frequency c))) ++ " stands"
  cerrSaveCorpus True True c f


-- Create a raw corpus using a single raw summary
cerrSaveSummary :: FilePath -> Corpus -> IO ()  
cerrSaveSummary f c = do
  print $ "-- Saving summary with " ++ (show (IntMap.size (frequency c))) ++ " stands"
  cerrSaveCorpus False True c f
  

-- gitzSimilarity q t P
-- Compute the Gitz similarity of a query q and target t w.r.t corpus P
gitzSimilarity :: FreqMap -> FreqMap -> Corpus -> IO Float
gitzSimilarity hQ hT (Corpus _ s fq) = do
  let sharedStrands = IntMap.keys $ intersectionFreq hT hQ
  let sizeCorpus = fromIntegral s
  let simStrands = map (\(hS) -> sizeCorpus / (cerrFreq hS fq)) sharedStrands
  let simScore = sum simStrands
  return simScore
    where cerrFreq h cfq = fromIntegral (getFreq cfq h) -- f(s) in gitz

-- cerrSimpleSimilarity
-- Unweighted similarity: matched strands count for 1 if they are not present more than N times in the corpus
cerrSimpleSimilarity :: Word16 -> FreqMap -> FreqMap -> Corpus -> IO Float
cerrSimpleSimilarity n hQ hT (Corpus _ _ fq) = do
  let sharedStrands = IntMap.keys $ intersectionFreq hT hQ
  let simStrands = map (\(hS) -> cerrSimpleFreq hS fq) sharedStrands
  let simScore = sum simStrands
  return simScore
    where cerrSimpleFreq h cfq = if (n <= fromIntegral (getFreq cfq h))
                                    then 0
                                    else 1

-- Assumes one of hQ or hT has already been filtered for significant strand
cerrCountSimilarity :: FreqMap -> FreqMap -> IO Float
cerrCountSimilarity hQ hT  = 
  return (fromIntegral (IntMap.size (intersectionFreq hT hQ)))


-- If 0 < Int <= Freq.maxbound, compute cerrSimpleSimilarity, otherwise compute gitzSimilarity
cerrSimilarity :: Bool -> FreqMap -> FreqMap ->  Corpus -> IO Float
cerrSimilarity True = (\ hQ hT _ -> cerrCountSimilarity hQ hT)
cerrSimilarity False = gitzSimilarity

-- Target, Query, Similarity, Condition \n
cerrFormatEval :: FilePath -> String -> (String, Float) -> String
cerrFormatEval t c (q, s) = t ++ ", " ++ q ++ ", " ++ (show s) ++ ", " ++ c ++ "\n"

cerrSaveEval :: FilePath -> String -> [(String, Float)] -> [(String, Float)] -> IO ()
cerrSaveEval f t ps ns = do
  putStrLn "-- Saving similarity scores as CSV"
  let posS = F.foldMap (cerrFormatEval t "1") ps
  let negS = F.foldMap (cerrFormatEval t "0") ns
  writeFile f $ posS ++ negS
  putStrLn "-- Done saving similarity scores"


cerrBatchSim :: Bool -> (FilePath -> FilePath) -> Bool -> FreqMap -> [FilePath] -> Corpus -> IO [(FilePath, Float)]
cerrBatchSim cached cachedM b hT qs hC = do
  let hashF = if cached
              then (cerrCachedHashStrands cachedM)
              else cerrHashStrands
  hQ <- mapM hashF qs
  let simFun = cerrSimilarity b
  simQ <- mapM (\h -> simFun hT h hC) hQ
  return (zip qs simQ)
  

cerrAccuracy :: FilePath -> Int -> FreqMap -> [Float] -> [Float] -> IO ()
cerrAccuracy aFP a hT sP sN = do
  let sizeT = IntMap.size hT
  let bound = (fromIntegral (sizeT * a))/100
  let aP = length sP
  let aN = length sN
  let tP = length (filter (\ x -> bound <= x) sP)
  let tN = length (filter (\ x -> x < bound) sN)
  let fP = aP - tP
  let fN = aN - tN
  let acc = (fromIntegral (tP + tN)) / (fromIntegral (aP + aN))
  let precision = (fromIntegral tP) / (fromIntegral (tP + fN))
  let recall = (fromIntegral tP) / (fromIntegral aP)
  if null aFP
    then do
      putStrLn "-- Minimum number of significant strands for match --"
      print bound
      putStrLn "-- Number of True positive -- "
      print tP
      putStrLn "-- Number of True negative -- "
      print tN
      putStrLn "-- Accuracy -- "
      print acc
      putStrLn "-- Precision -- "
      print precision
      putStrLn "-- Recall -- "
      print recall
    else appendFile aFP $ (show a) ++ ", " ++ (show bound) ++ ", " ++ (show tP) ++ ", " ++ (show fP) ++ ", " ++ (show tN) ++ ", " ++ (show fN)  ++ "\n"
  


----------------
-- Handle CLI --
----------------
  
runGenCorpus :: GenCorpusOptions -> IO ()
runGenCorpus options = 
  if (gLargeCorpus options)
    then runLGenCorpus
    else runGenCorpus'
  where runLGenCorpus = do
          let cLoc = gCorpusFp options
          if (gAppendCorpus options)
            then return ()
            else BS.writeFile cLoc $ BL.toStrict $ BSB.toLazyByteString (boolBS True) -- create/overwrite the corpus file
          corpus <- SFG.glob (gCorpus options)
          mapM (cerrMakeSummary >=> cerrAppendSummary cLoc) corpus >> putStrLn "-- Corpus saved!"
        runGenCorpus' = do
          let bOpt = gOptBound options
          let rOpt = if (bOpt == 0)
                       then gRawCorpus options
                       else False     
          aOpt <- if rOpt
                    then return (gAppendCorpus options)
                    else return False
          print ("-- Generating a corpus, optimized to bound " ++ (show bOpt))
          hC <- cerrMakeCorpus bOpt (gCorpus options)
          print $ "-- Corpus created"
          cerrSaveCorpus aOpt rOpt hC (gCorpusFp options)


runMergeCorpus :: MergeCorpusOptions -> IO ()
runMergeCorpus options = do
  let oOpt = mOptBound options
  let rOpt = if (oOpt == 0)
               then mRawCorpus options
               else False
  let aOpt = if rOpt
               then mAppendCorpus options
               else False
  putStrLn "-- Merging corpora"
  let hCFp = (mCorpora options)
  hC <- mapM cerrLoadCorpus hCFp
  putStrLn "-- All corpus loaded"
  let hCM = fromMaybe (emptyCorpus False) (cerrCombineCorpora hC)
  let hCM'= cerrOptNCorpus oOpt hCM
  putStrLn "-- Corpora merged"
  cerrSaveCorpus aOpt rOpt hCM' (mCorpusFp options)


runHash :: HashOptions -> IO ()
runHash options = do
  let cOpt = hCorpusFp options
  let tOpt = hThreshold options
  let bOpt = hBound options
  hI <- cerrHashStrands (hInputFp options)
  -- if a cOpt OR a tOpt is given, load cOpt and filter hI according to it
  (hI', bounded) <- if (bOpt == 0) && (tOpt == 0)
         then return (hI, False)
         else do  hC <- X.catch (putStrLn "-- Loading corpus" >> cerrLoadCorpus cOpt) (\ (_ :: X.SomeException) -> (print "Failed to load corpus") >> return (emptyCorpus False))
                  cerrBoundHash tOpt bOpt hC hI
  if null (hOCorpusFp options)
    then return ()
    else cerrSaveSummary (hOCorpusFp options) (Corpus bounded (IntMap.size hI) hI')
  cerrPrintCorpus (hOutputFp options) (Corpus False 0 hI')

runSim :: SimOptions -> IO ()
runSim options = do
  let sOpt = sSaveCorpus options
  let cOpt = sCorpus options
  let oOpt = sOptBound options
  let tOpt = sThreshold options
  let bOpt = sBound options
  let rOpt = if (oOpt == 0)
               then sRawCorpus options
               else False
  putStrLn "-- Similarity between target and query w.r.t. summary"
  hC <- if null cOpt
          then X.catch (putStrLn "-- Loading corpus" >> cerrLoadCorpus (sCorpusFp options)) (\ (_ :: X.SomeException) -> (print "Failed to load corpus") >> return (emptyCorpus False))
          else cerrMakeCorpus oOpt cOpt
  putStrLn "-- Corpus loaded!"
  print (size hC)
  if sOpt 
    then if null cOpt
           then putStrLn "-- -s used without --corpus, ignored"
           else do cerrSaveCorpus False rOpt hC (sCorpusFp options)
                   putStrLn "-- Corpus saved!"
    else return ()
  hT <- cerrHashStrands (sTargetFp options)
  -- if threshold is in range, find the bound for target according to corpus
  (hT', bounded) <- cerrBoundHash tOpt bOpt hC hT
  putStrLn "-- Target strands (hashed):"
  print (keyHash hT)
  putStrLn "-- Significant target strands (hashed):"
  print (keyHash hT')
  hQ <- cerrHashStrands (sQueryFp options)
  putStrLn "-- Query strands (hashed):"
  print (keyHash hQ)
  putStrLn "-- Similarity w.r.t corpus:"
  simqt <- cerrSimilarity bounded hT' hQ hC
  print simqt


runEvalSim :: EvalSimOptions -> IO ()
runEvalSim options = do
  hC <- X.catch (putStrLn "-- Loading corpus" >> cerrLoadCorpus (eCorpusFp options)) (\ (_ :: X.SomeException) -> (print "Failed to load corpus") >> return (emptyCorpus False))
  let cOpt = eCached options
  let cDir = "./_cached/"
  let cMap = (\s -> cDir ++ (takeBaseName s) ++ ".cached")
  let fpT = eTargetFp options
  fpP0 <- SFG.glob (ePositive options)
  fpN0 <- SFG.glob (eNegative options)
  let fpP = delete fpT fpP0 -- always remove fpT from positives
  let fpN = if (eExcludePositive options)
             then fpN0 \\ (fpT:fpP)
             else fpN0 

  hT <- if cOpt
          then do createDirectoryIfMissing True cDir
                  cerrCachedHashStrands cMap fpT
          else cerrHashStrands fpT
  let tOpt = eThreshold options               
  let bOpt = eBound options
  (hT', bounded) <- cerrBoundHash tOpt bOpt hC hT
  fpsP <- cerrBatchSim cOpt cMap bounded hT' fpP hC
  fpsN <- cerrBatchSim cOpt cMap bounded hT' fpN hC
  let aOpt = eAcc options
  if (aOpt  == 0 )
    then return ()
    else cerrAccuracy (eASaveFp options) aOpt hT' (map snd fpsP) (map snd fpsN)
  let fpSave = eSaveFp options
  if null fpSave
    then do putStrLn "-- Eval sim!"
            print fpT
            putStrLn "-- Positive pairs score:"
            mapM_ print fpsP
            putStrLn "-- Negative pairs score:"
            mapM_ print fpsN
    else cerrSaveEval fpSave fpT fpsP fpsN
  
runPrintSummary :: PrintSummaryOptions -> IO ()
runPrintSummary options = do
  hC <- X.catch (putStrLn "-- Loading corpus" >> cerrLoadCorpus (pCorpusFp options)) (\ (_ :: X.SomeException) -> (print "Failed to load corpus") >> return (emptyCorpus False))
  cerrPrintCorpus (pOutputFp options) hC
  putStrLn "-- Done printing corpus"  

  
main :: IO ()
main = do
  cmd <- execParser parseCommand
  case cmd of
    Sim o -> runSim o
    EvalSim o -> runEvalSim o
    Hash o -> runHash o
    GenCorpus o -> runGenCorpus o
    MergeCorpus o -> runMergeCorpus o
    PrintSummary o -> runPrintSummary o
      

