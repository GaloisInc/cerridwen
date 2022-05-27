{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module RefDef.RegX86 where
import qualified Data.Set as Set
import           Data.Parameterized.Some ( Some(..), viewSome )
import qualified Data.Macaw.CFG as MC
import qualified Data.Parameterized.Map as PM
import           Data.Parameterized.TraversableF
import qualified Data.Macaw.X86 as MX
--import qualified Data.Macaw.X86.X86Reg as X86R

refAssignRhs :: MC.AssignRhs MX.X86_64 (MC.Value MX.X86_64 ids) tp -> Set.Set (Some (MC.ArchReg MX.X86_64))
refAssignRhs = \case
    MC.EvalApp (app::MC.App f tp) -> MC.foldMapFC refV app
    MC.EvalArchFn (archfn::MC.ArchFn MX.X86_64 f tp) _rep -> MC.foldMapFC refV archfn
    MC.SetUndefined _rep -> Set.empty
    MC.ReadMem _bv _rep -> Set.empty
    MC.CondReadMem _rep _b _bv _tp -> Set.empty


refV :: MC.Value MX.X86_64 ids tp -> Set.Set (Some (MC.ArchReg MX.X86_64))
refV = \case
    MC.Initial reg -> Set.singleton (Some reg)
    MC.AssignedValue (MC.Assignment _ rhs) -> refAssignRhs rhs
    MC.CValue _cval -> Set.empty



ref :: MX.ArchitectureInfo MX.X86_64 -> MC.Stmt MX.X86_64 ids -> Set.Set (Some (MC.ArchReg MX.X86_64))
ref _ = \case
  MC.ArchState (_addr::MC.ArchMemAddr MX.X86_64) (assn::PM.MapF (MC.ArchReg MX.X86_64) (MC.Value MX.X86_64 is)) ->
    Set.foldr (\ a s -> Set.union s (viewSome refV a)) Set.empty (Set.fromList $ PM.elems assn)
    
  MC.ExecArchStmt (stm::MC.ArchStmt MX.X86_64 (MC.Value MX.X86_64 ids)) -> foldMapF refV stm
  MC.AssignStmt (MC.Assignment _ rhs) -> refAssignRhs rhs
  MC.WriteMem _a _mrep rhs -> refV rhs
  MC.CondWriteMem _c _a _mrep rhs -> refV rhs -- Not looking into condition for now
  _ -> Set.empty 


defX86Stmt :: MX.X86Stmt (MC.Value MX.X86_64 ids) -> Set.Set (Some (MC.ArchReg MX.X86_64))
defX86Stmt = \case
    MX.StoreX87Control addr -> refV addr 
    MX.RepMovs _bc dest _src _cnt _dir -> refV dest
    MX.RepStos _bc dest _val _cnt _dir -> refV dest
    MX.EMMS -> Set.empty
    MX.SetSegmentSelector _sg _v -> Set.empty -- ignore Segment register


def :: MC.Stmt MX.X86_64 ids -> Set.Set (Some (MC.ArchReg MX.X86_64))
def = \case
  MC.ArchState _addr assn -> Set.fromList (PM.keys assn)
  MC.ExecArchStmt (stm :: MC.ArchStmt MX.X86_64 (MC.Value MX.X86_64 ids)) -> defX86Stmt stm
  MC.AssignStmt _a -> Set.empty
  MC.WriteMem _a _mrep _rhs -> Set.empty 
  MC.CondWriteMem _c _a _mrep _rhs -> Set.empty 
  _ -> Set.empty
