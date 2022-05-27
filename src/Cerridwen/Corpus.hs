---------------
-- Corpus    --
---------------
-- The Corpus is represented as precomputed information collected offline over a binary procedures
-- 1) The number of binary procedures in the corpus
-- 2) A map between hashes of strands and its frequency (number of binary procedures in which it occurs) in the corpus

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Corpus where
import Data.List
import Data.Word
import qualified Data.IntMap.Strict as IntMap
import           Data.Binary.Get (Get)
import qualified Data.Binary.Get as Get
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.Foldable as F
import Utils

-- Frequency map
type Freq = Word16

type FreqMap = IntMap.IntMap Freq

emptyFreq :: FreqMap
emptyFreq = IntMap.empty

maxFreq :: Freq
maxFreq = maxBound :: Word16

-- Bounded (at maxFreq) addition 
plusFreq :: Freq -> Freq -> Freq
plusFreq f1 f2 = let t = f1 + f2 in
  if (t < f1 || t < f2)
    then maxFreq
    else t
    


updateFreq :: FreqMap -> Int -> FreqMap
updateFreq m h = IntMap.insertWith plusFreq h 1 m

unionFreq :: FreqMap -> FreqMap -> FreqMap
unionFreq = IntMap.unionWith plusFreq

unionsFreq :: Foldable f => f FreqMap -> FreqMap
unionsFreq = IntMap.unionsWith plusFreq

intersectionFreq :: FreqMap -> FreqMap -> FreqMap
intersectionFreq = IntMap.intersectionWith min

fromListFreq :: [Int] -> FreqMap
fromListFreq = foldl' updateFreq emptyFreq

fromAssocFreq :: [(Int, Freq)] -> FreqMap
fromAssocFreq = IntMap.fromListWith plusFreq

keyHash :: FreqMap -> [Word64]
keyHash freq = map hashW64 (IntMap.keys freq)



-- Expected behavior of getting a Freq out of a FreqMap:
-- Default : 1
-- If maxFreq, return maxBound::Int
-- o.w. return found value
getFreq :: FreqMap -> Int -> Int
getFreq fqm k =
  let fq = (IntMap.findWithDefault 1 k fqm) in
    if fq == maxFreq
      then maxBound
      else fromIntegral fq

getFreqW16 :: FreqMap -> Int -> Word16
getFreqW16 fqm k = IntMap.findWithDefault 1 k fqm

-- Used in Corpus to serialize the frequency
freqBE :: Freq -> BSB.Builder
freqBE i = BSB.word16BE i

getFreqBE :: Get Freq
getFreqBE = do
  i <- Get.getWord16be
  return i


-- Corpus datatype

data Corpus = Corpus { isOpt :: Bool, -- has the corpus been compressed. Otherwise, it should be the case that size = |frequency| (and that you can merge with other uncompressed corpora)
                       size :: Int, -- Number of unique strand in the corpus. 
                       frequency :: FreqMap -- Frequency of strands
                     }

emptyCorpus :: Bool -> Corpus
emptyCorpus opt = Corpus opt 0 emptyFreq


-- Serializing Corpus
-- raw corpus are serialized as a list of strand's hash, where each strands may appear multiple times
-- opt corpus are serialized as (1) the number of unique strands in the corpus (2) pairs of strands and frequency for each strands appearing more than one time. We can assume strands in the pair are unique

decodeFreqCorpus :: Get Corpus
decodeFreqCorpus  = do
  opt <- getBoolBS
  sizeI <- getIntBE
  fq <- decodeFreqCorpus' IntMap.empty
  return (Corpus opt sizeI fq)
  where decodeFreqCorpus' curr = do
          empty <- Get.isEmpty
          if empty
            then return curr
            else do k <- getIntBE
                    v <- getFreqBE
                    decodeFreqCorpus' (IntMap.insert k v curr) 

decodeRawCorpus :: Get Corpus
decodeRawCorpus = do
  fq <- decodeRawCorpus' IntMap.empty
  let sizeI = IntMap.size fq
  return (Corpus True sizeI fq)
  where decodeRawCorpus' curr = do
          empty <- Get.isEmpty
          if empty
            then return curr
            else do k <- getIntBE
                    decodeRawCorpus' $ updateFreq curr k
          
decodeCorpus :: Get Corpus
decodeCorpus = do
  raw <- getBoolBS
  if raw
    then decodeRawCorpus
    else decodeFreqCorpus


encodeFreqCorpus :: Corpus -> BSB.Builder
encodeFreqCorpus c = 
  let opt = isOpt c
      sbytes = size c
      fq = frequency c
      fqbuilder = IntMap.foldMapWithKey encodeCell fq
  in (boolBS opt) <> (intBE sbytes) <> fqbuilder
  where encodeCell ik iv = (intBE ik) <> (freqBE iv)          

encodeRawCorpus :: Corpus -> BSB.Builder
encodeRawCorpus c = IntMap.foldMapWithKey encodeRawCell (frequency c)
  where encodeRawCell ik iv = (mconcat (replicate (fromIntegral iv) (intBE ik)))


encodeCorpus :: Bool -> Corpus -> BSB.Builder
encodeCorpus raw c = boolBS raw <> if raw
                                     then encodeRawCorpus c
                                     else encodeFreqCorpus c




-- cerrSaveCorpus append? raw? Corpus CorpusLoc
cerrSaveCorpus :: Bool -> Bool -> Corpus -> FilePath -> IO ()
cerrSaveCorpus a r c f = do
  let builder = if a
                  then encodeRawCorpus c
                  else encodeCorpus r c
  let bytes = BL.toStrict $ BSB.toLazyByteString builder
  print $ "-- Corpus saved with " ++ (show (size c)) ++ " strands, optimized? " ++ (show (isOpt c))
  if a
    then BS.appendFile f bytes
    else BS.writeFile f bytes

cerrLoadCorpus :: FilePath -> IO Corpus
cerrLoadCorpus f = do
  bytes <- BS.readFile f  
  let corpus = strictRunGet decodeCorpus bytes
--  print $ "-- Corpus loaded with " ++ (show (size corpus)) ++ " strands , optimized? " ++ (show (isOpt corpus))
  return corpus


cerrShowCorpus :: Corpus -> String
cerrShowCorpus c = do
  let freq = frequency c
  let freqAssoc = IntMap.assocs freq
  foldMap formatCorpusEntry freqAssoc
  where formatCorpusEntry (h, f) = id ((show (hashW64 h)) ++ ", " ++ (show f) ++ "\n")
  
-- Print Corpus as CSV in f is not empty, otherwise display on stdout
cerrPrintCorpus :: FilePath -> Corpus -> IO ()
cerrPrintCorpus f c = do
  let s = cerrShowCorpus c
  if null f
    then putStr s
    else writeFile f s

-- Utilities for Corpus --

cerrOptCorpus :: Corpus -> Corpus
cerrOptCorpus (Corpus False sizeI fq) = cerrOptNCorpus 1 (Corpus False sizeI fq)
cerrOptCorpus c = c

cerrOptNCorpus :: Int -> Corpus -> Corpus
cerrOptNCorpus n (Corpus opt sizeI fq) =
  if 1 <= n && n <= (fromIntegral (maxBound::Freq))
    then Corpus True sizeI (IntMap.filter (> (fromIntegral n)) fq)
    else Corpus opt sizeI fq

cerrFakeOptCorpus :: Corpus -> Corpus
cerrFakeOptCorpus (Corpus _ sizeI fq) = (Corpus True sizeI fq)

-- Combine two (uncompressed) corpora
cerrCombineCorpus :: Corpus -> Corpus -> Maybe Corpus
cerrCombineCorpus c1 c2 =
  if not ((isOpt c1) || (isOpt c2))
  then let fq = unionFreq (frequency c1) (frequency c2)
           sizeI = IntMap.size fq
       in Just (Corpus False sizeI fq)    
  else Nothing

-- Combine a list of (uncompressed) corpora
-- Base case allows for using merge to optimize an already optimized corpus
cerrCombineCorpora :: [Corpus] -> Maybe Corpus
cerrCombineCorpora [c] = Just c
cerrCombineCorpora cs =
  F.foldl' cerrCombineMaybeCorpus (Just (emptyCorpus False)) cs 
  where cerrCombineMaybeCorpus Nothing _ = Nothing
        cerrCombineMaybeCorpus (Just c1) c2 = cerrCombineCorpus c1 c2
        
