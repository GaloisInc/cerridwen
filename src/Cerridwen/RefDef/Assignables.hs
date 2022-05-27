{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module RefDef.Assignables where
import qualified Data.Set as Set
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Parameterized.Map as PM
import qualified Data.Macaw.CFG as MC
import qualified Data.Macaw.CFG.DemandSet as MDS
import qualified Data.Macaw.X86 as MX
import qualified Data.Macaw.X86.ArchTypes as MXA


ref :: PM.OrdF (MC.ArchReg arch) => MX.ArchitectureInfo arch -> MC.Stmt arch ids -> Set.Set (Some (MC.AssignId ids))
ref ainfo s = MDS.runDemandComp (MX.archDemandContext ainfo) (MDS.addStmtDemands s)

def :: PM.OrdF (MC.ArchReg arch) => MC.Stmt arch ids -> Set.Set (Some (MC.AssignId ids))
def = \case
  MC.AssignStmt a -> Set.singleton (Some $ MC.assignId a)
  _ -> Set.empty
 
