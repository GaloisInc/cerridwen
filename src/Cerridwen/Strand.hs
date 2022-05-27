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

module Strand where
import qualified Data.Text as Text
import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import qualified Data.Foldable as F
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map as Map
import qualified Data.Parameterized.Map as PM
import qualified Data.Parameterized.TraversableF as PF
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Macaw.CFG as MC
import qualified Data.Macaw.X86 as MX
import RefDef ()
import qualified RefDef.Assignables as RDA
import Hashing ()

type HStrand = [Int]
type Strand arch ids = [(MC.Stmt arch ids)]




type LocSet arch ids = Set.Set (Some (MC.AssignId ids))

a2lSet :: Set.Set (Some (MC.AssignId ids)) -> LocSet arch ids
a2lSet = id


-- findStrands varRefed varDefed ls
-- varRefed is a map from Stmt hash to the set of location they refer to
-- varDefed is a map from Stmt hash to the set of location they define
-- ls is an ordered sequence of Stmt hash
-- Return is a list of a pair of
--- Strands, which are lists of Stmt hash
--- Inputs, which are the LocSet referred to but not defined in the Strand
findStrands :: IntMap.IntMap (LocSet arch ids) -> IntMap.IntMap (LocSet arch ids) -> Seq.Seq Int -> [(HStrand, LocSet arch ids)]
findStrands varRefed varDefed lso =
  let lso' = Seq.reverse lso -- for foldl
      unused = Set.fromDistinctAscList [0..((Seq.length lso)-1)]
  in whileStrands unused lso' [] 
  where -- whileStrands :: Seq.Seq Int (index of unused) -> Seq.Seq.Int (hash of stmt) -> [(HStrand, LocSet arch ids)] -> [(HStrand, LocSet arch ids)]
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

findStrandsR :: IntMap.IntMap (LocSet arch ids) -> IntMap.IntMap (LocSet arch ids) -> Seq.Seq Int -> [(HStrand, LocSet arch ids)]
findStrandsR varRefed varDefed lso =
  let unused = Set.fromDistinctAscList [0..((Seq.length lso)-1)]
  in whileStrands unused lso [] 
  where -- whileStrands :: Seq.Seq Int (index of unused) -> Seq.Seq.Int (hash of stmt) -> [(HStrand, LocSet arch ids)] -> [(HStrand, LocSet arch ids)]
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


stmtVarRef :: (PM.OrdF (MC.ArchReg arch), MC.ArchConstraints arch) => MX.ArchitectureInfo arch -> MC.Stmt arch ids -> LocSet arch ids
stmtVarRef ainfo s = RDA.ref ainfo s

stmtVarDef :: (PM.OrdF (MC.ArchReg arch), MC.ArchConstraints arch) => MC.Stmt arch ids -> LocSet arch ids
stmtVarDef s = RDA.def s

computeRefDef :: (PM.OrdF (MC.ArchReg arch), MC.ArchConstraints arch) => MX.ArchitectureInfo arch -> [(Int, MC.Stmt arch ids)] -> (IntMap.IntMap (LocSet arch ids), IntMap.IntMap (LocSet arch ids))
computeRefDef ainfo hsbb = F.foldl' addStmtRefDef (mempty, mempty) hsbb
  where addStmtRefDef (vR, vD) (hs, s) = (IntMap.insert hs (stmtVarRef ainfo s) vR, IntMap.insert hs (stmtVarDef s) vD)
  

-- input and output of findStrandsMain
displayStrands :: (MC.IPAlignment arch, MC.PrettyF (MC.ArchTermStmt arch), PF.FoldableF (MC.ArchStmt arch), MC.IsArchStmt (MC.ArchStmt arch), MC.IsArchFn (MC.ArchFn arch), MC.FoldableFC (MC.ArchFn arch), MC.RegisterInfo (MC.ArchReg arch)) => [(Int, MC.Stmt arch ids)] -> [(HStrand, LocSet arch ids)] -> IO ()
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
findStrandsMain :: (PM.OrdF (MC.ArchReg arch), MC.ArchConstraints arch) => MX.ArchitectureInfo arch -> [(Int, MC.Stmt arch ids)] -> [(HStrand, LocSet arch ids)]
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

