{-# LANGUAGE FlexibleContexts #-}
-- | Perform copy propagation.  This is done by invoking the
-- simplifier with no rules, so hoisting and dead-code elimination may
-- also take place.
module Futhark.Transform.CopyPropagate
       (copyPropagateInBindings)
       where

import Futhark.MonadFreshNames
import Futhark.Representation.AST
import Futhark.Optimise.Simplifier.Simple
import Futhark.Optimise.Simplifier.Engine (MonadEngine)
import Futhark.Optimise.Simplifier
  (simplifyBindingsWithRules, noExtraHoistBlockers)

copyPropagateInBindings :: (MonadFreshNames m,
                            HasTypeEnv m,
                            MonadEngine (SimpleM lore)) =>
                           SimpleOps (SimpleM lore)
                        -> [Binding lore]
                        -> m [Binding lore]
copyPropagateInBindings simpl =
  simplifyBindingsWithRules simpl ([], []) noExtraHoistBlockers
