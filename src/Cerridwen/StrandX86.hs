{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}

module StrandX86 where
import qualified Data.Text as Text
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import qualified Data.Foldable as F
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map as Map
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Macaw.CFG as MC
import qualified Data.Macaw.X86 as MX
import qualified RefDef.Assignables as RDA
import qualified RefDef.RegX86 as RDR
import Hashing ()

type HStrand = [Int]
type Strand ids = [(MC.Stmt MX.X86_64 ids)]


type LocSet ids = Set.Set (Either (Some (MC.AssignId ids)) (Some (MC.ArchReg MX.X86_64)))

a2lSet :: Set.Set (Some (MC.AssignId ids)) -> LocSet ids
a2lSet a  = Set.disjointUnion a Set.empty

r2lSet :: Set.Set (Some (MC.ArchReg MX.X86_64)) -> LocSet ids
r2lSet = Set.disjointUnion Set.empty  


-- findStrands varRefed varDefed ls
-- varRefed is a map from Stmt hash to the set of location they refer to
-- varDefed is a map from Stmt hash to the set of location they define
-- ls is an ordered sequence of Stmt hash
-- Return is a list of a pair of
--- Strands, which are lists of Stmt hash
--- Inputs, which are the LocSet referred to but not defined in the Strand
findStrands :: IntMap.IntMap (LocSet ids) -> IntMap.IntMap (LocSet ids) -> Seq.Seq Int -> [(HStrand, LocSet ids)]
findStrands varRefed varDefed lso =
  let lso' = Seq.reverse lso -- for foldl
      unused = Set.fromDistinctAscList [0..((Seq.length lso)-1)]
  in whileStrands unused lso' [] 
  where -- whileStrands :: Seq.Seq Int (index of unused) -> Seq.Seq.Int (hash of stmt) -> [(HStrand, LocSet ids)] -> [(HStrand, LocSet ids)]
        whileStrands unused ls strs =
          if (Set.null unused) then
            strs
          else 
            let  (maxUnused, restUnused) = Set.deleteFindMax unused
            in case (Seq.lookup ((length ls) - (maxUnused+1)) ls) of
                 Nothing -> []
                 Just strandHead ->
                   let ls' = Seq.drop ((length ls) - maxUnused) ls -- drop strandHand and stmt after strandHand                     
                       varRef = IntMap.findWithDefault mempty strandHead varRefed 
                       varDef = IntMap.findWithDefault mempty strandHead varDefed
                       (newStrand, newUnused, newRef, newDef) = Seq.foldlWithIndex (forStrand (length ls')) ([strandHead], restUnused, varRef, varDef) ls'
                       inputs = newRef Set.\\ newDef
                   in whileStrands newUnused ls' ((newStrand, inputs):strs)
        forStrand lenls (str, unused, varRef, varDef) idx currStmt =
          let needed = (IntMap.findWithDefault mempty currStmt varDefed) `Set.intersection` varRef
          in if null needed then (str, unused, varRef, varDef)
             else let str' = currStmt:str
                      varRef' = varRef `Set.union` (IntMap.findWithDefault mempty currStmt varRefed)
                      varDef' = varDef `Set.union` needed
                      unused' = Set.delete (lenls - (idx + 1)) unused
                  in (str', unused', varRef', varDef')

findStrandsR :: IntMap.IntMap (LocSet ids) -> IntMap.IntMap (LocSet ids) -> Seq.Seq Int -> [(HStrand, LocSet ids)]
findStrandsR varRefed varDefed lso =
  let unused = Set.fromDistinctAscList [0..((Seq.length lso)-1)]
  in whileStrands unused lso [] 
  where -- whileStrands :: Seq.Seq Int (index of unused) -> Seq.Seq.Int (hash of stmt) -> [(HStrand, LocSet ids)] -> [(HStrand, LocSet ids)]
        whileStrands unused ls strs =
          if (Set.null unused) then
            strs
          else 
            let  (maxUnused, restUnused) = Set.deleteFindMax unused
            in case (Seq.lookup maxUnused ls) of
                 Nothing -> []
                 Just strandHead ->
                   let ls' = Seq.take maxUnused ls                       
                       varRef = IntMap.findWithDefault mempty strandHead varRefed 
                       varDef = IntMap.findWithDefault mempty strandHead varDefed
                       (newStrand, newUnused, newRef, newDef) = Seq.foldrWithIndex forStrand ([strandHead], restUnused, varRef, varDef) ls'
                       inputs = newRef Set.\\ newDef
                   in whileStrands newUnused ls' ((newStrand, inputs):strs)
        forStrand idx currStmt (str, unused, varRef, varDef) =
          let needed = (IntMap.findWithDefault mempty currStmt varDefed) `Set.intersection` varRef          
          in if null needed then (str, unused, varRef, varDef)
             else let str' = currStmt:str
                      varRef' = varRef `Set.union` (IntMap.findWithDefault mempty currStmt varRefed)
                      varDef' = varDef `Set.union` needed
                      unused' = Set.delete idx unused
                  in (str', unused', varRef', varDef')


stmtVarRef :: MX.ArchitectureInfo MX.X86_64 -> MC.Stmt MX.X86_64 ids -> LocSet ids
stmtVarRef ainfo s = Set.disjointUnion (RDA.ref ainfo s) (RDR.ref ainfo s)

stmtVarDef :: MC.Stmt MX.X86_64 ids -> LocSet ids
stmtVarDef s = Set.disjointUnion (RDA.def s) (RDR.def s)

computeRefDef :: MX.ArchitectureInfo MX.X86_64 -> [(Int, MC.Stmt MX.X86_64 ids)] -> (IntMap.IntMap (LocSet ids), IntMap.IntMap (LocSet ids))
computeRefDef ainfo hsbb = F.foldl' addStmtRefDef (mempty, mempty) hsbb
  where addStmtRefDef (vR, vD) (hs, s) = (IntMap.insert hs (stmtVarRef ainfo s) vR, IntMap.insert hs (stmtVarDef s) vD)
  

-- input and output of findStrandsMain
displayStrands :: [(Int, MC.Stmt MX.X86_64 ids)] -> [(HStrand, LocSet ids)] -> IO ()
displayStrands hs strands = do
  putStrLn "-- Basic block:"
  putStrLn "======================"
  mapM_ print (map snd hs)
  putStrLn "======================"
  let stmtM = Map.fromList hs
  let pstrands = map (\ (hstr, _input) -> 
                        let str = map (\h -> Map.findWithDefault (MC.Comment (Text.pack "Error in findStrandsMain"))  h stmtM) hstr
                        in str) strands 
  putStrLn $ "Results in " ++ (show (length strands)) ++ " strand(s):"
  putStrLn "----------------------"
  mapM_ print pstrands
  putStrLn "----------------------"



  
-- Input: hsbb is basic block consisting of a list of pairs of statement and their hash
-- Return: A list of strand x Input extracted from the basic block
-- To avoid quadratic blowup in computation of strands, break basic blocks that are bigger than some constant OR drop very big blocks.
findStrandsMain :: MX.ArchitectureInfo MX.X86_64 -> [(Int, MC.Stmt MX.X86_64 ids)] -> [(HStrand, LocSet ids)]
findStrandsMain ainfo hsbb =
  -- compute Ref/Def maps for bb
  let (varRefed, varDefed) = computeRefDef ainfo hsbb
  -- project the sequence of hashes
      hs =  Seq.fromList (map fst hsbb)
  -- Split basic blocks into chunks of at most 1000 instructions
--      hss = Seq.chunksOf 1000 hs
  -- Compute the strands for each chunks
--      sss = fmap (findStrandsR varRefed varDefed) hss
  in if (Seq.length hs) > 1000
        then []
        else findStrandsR varRefed varDefed hs -- F.fold sss
