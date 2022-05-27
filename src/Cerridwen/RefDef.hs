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

module RefDef where
import qualified Data.Set as Set
import           Data.Parameterized.Some ( Some(..) )
import qualified Data.Macaw.CFG as MC
import qualified Data.Parameterized.Map as PM
import           Data.Parameterized.TraversableF
import qualified Data.Macaw.X86 as MX


class (PM.OrdF (MC.ArchReg arch), MC.ArchConstraints arch) => RedDef arch where
  ref :: forall ids. MC.Stmt arch ids -> Set.Set (Some (MC.Value arch ids))
  def :: forall ids. MC.Stmt arch ids -> Set.Set (Some (MC.Value arch ids))

