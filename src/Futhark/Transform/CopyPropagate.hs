{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
-- | Perform copy propagation.  This is done by invoking the
-- simplifier with no rules, so hoisting and dead-code elimination may
-- also take place.
module Futhark.Transform.CopyPropagate
       ( copyPropagateInProg
       , copyPropagateInStms
       , copyPropagateInFun
       )
where

import Futhark.Pass
import Futhark.MonadFreshNames
import Futhark.Representation.AST
import Futhark.Optimise.Simplify
import qualified Futhark.Analysis.SymbolTable as ST
import Futhark.Optimise.Simplify.Lore (Wise)

-- | Run copy propagation on an entire program.
copyPropagateInProg :: SimplifiableLore lore =>
                       SimpleOps lore
                    -> Prog lore
                    -> PassM (Prog lore)
copyPropagateInProg simpl = simplifyProg simpl mempty neverHoist

-- | Run copy propagation on some statements.
copyPropagateInStms :: (MonadFreshNames m, SimplifiableLore lore) =>
                       SimpleOps lore
                    -> Scope lore
                    -> Stms lore
                    -> m (ST.SymbolTable (Wise lore), Stms lore)
copyPropagateInStms simpl = simplifyStms simpl mempty neverHoist

-- | Run copy propagation on a function.
copyPropagateInFun :: (MonadFreshNames m, SimplifiableLore lore) =>
                       SimpleOps lore
                   -> ST.SymbolTable (Wise lore)
                   -> FunDef lore
                   -> m (FunDef lore)
copyPropagateInFun simpl = simplifyFun simpl mempty neverHoist
