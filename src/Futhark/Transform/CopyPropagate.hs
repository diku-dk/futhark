{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
-- | Perform copy propagation.  This is done by invoking the
-- simplifier with no rules, so hoisting and dead-code elimination may
-- also take place.
module Futhark.Transform.CopyPropagate
       ( copyPropagateInStms
       , copyPropagateInFun)
       where

import Futhark.MonadFreshNames
import Futhark.Representation.AST
import Futhark.Optimise.Simplify

-- | Run copy propagation on some statements.
copyPropagateInStms :: (MonadFreshNames m, SimplifiableLore lore, HasScope lore m) =>
                       SimpleOps lore
                    -> Stms lore
                    -> m (Stms lore)
copyPropagateInStms simpl = simplifyStms simpl mempty noExtraHoistBlockers

-- | Run copy propagation on a function.
copyPropagateInFun :: (MonadFreshNames m, SimplifiableLore lore) =>
                       SimpleOps lore
                    -> FunDef lore
                    -> m (FunDef lore)
copyPropagateInFun simpl = simplifyFun simpl mempty noExtraHoistBlockers
