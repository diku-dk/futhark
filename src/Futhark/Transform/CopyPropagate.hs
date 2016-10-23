{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
-- | Perform copy propagation.  This is done by invoking the
-- simplifier with no rules, so hoisting and dead-code elimination may
-- also take place.
module Futhark.Transform.CopyPropagate
       (copyPropagateInStms)
       where

import Data.Monoid

import Prelude

import Futhark.MonadFreshNames
import Futhark.Representation.AST
import Futhark.Optimise.Simplifier.Engine
import Futhark.Optimise.Simplifier (simplifyStmsWithRules)

copyPropagateInStms :: (MonadFreshNames m, SimplifiableLore lore, HasScope lore m) =>
                       SimpleOps lore
                    -> [Stm lore]
                    -> m [Stm lore]
copyPropagateInStms simpl =
  simplifyStmsWithRules simpl mempty noExtraHoistBlockers
