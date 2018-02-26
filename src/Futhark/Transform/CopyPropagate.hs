{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
-- | Perform copy propagation.  This is done by invoking the
-- simplifier with no rules, so hoisting and dead-code elimination may
-- also take place.
module Futhark.Transform.CopyPropagate
       (copyPropagateInStms)
       where

import Futhark.MonadFreshNames
import Futhark.Representation.AST
import Futhark.Optimise.Simplify

-- | Run copy propagation.
copyPropagateInStms :: (MonadFreshNames m, SimplifiableLore lore, HasScope lore m) =>
                       SimpleOps lore
                    -> Stms lore
                    -> m (Stms lore)
copyPropagateInStms simpl = simplifyStms simpl mempty noExtraHoistBlockers
