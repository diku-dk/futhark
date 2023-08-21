-- | Perform copy propagation.  This is done by invoking the
-- simplifier with no rules, so hoisting and dead-code elimination may
-- also take place.
module Futhark.Transform.CopyPropagate
  ( copyPropagateInProg,
    copyPropagateInStms,
    copyPropagateInFun,
  )
where

import Futhark.Analysis.SymbolTable qualified as ST
import Futhark.IR
import Futhark.MonadFreshNames
import Futhark.Optimise.Simplify
import Futhark.Optimise.Simplify.Rep (Wise)
import Futhark.Pass

-- | Run copy propagation on an entire program.
copyPropagateInProg ::
  (SimplifiableRep rep) =>
  SimpleOps rep ->
  Prog rep ->
  PassM (Prog rep)
copyPropagateInProg simpl = simplifyProg simpl mempty neverHoist

-- | Run copy propagation on some statements.
copyPropagateInStms ::
  (MonadFreshNames m, SimplifiableRep rep) =>
  SimpleOps rep ->
  Scope rep ->
  Stms rep ->
  m (Stms rep)
copyPropagateInStms simpl = simplifyStms simpl mempty neverHoist

-- | Run copy propagation on a function.
copyPropagateInFun ::
  (MonadFreshNames m, SimplifiableRep rep) =>
  SimpleOps rep ->
  ST.SymbolTable (Wise rep) ->
  FunDef rep ->
  m (FunDef rep)
copyPropagateInFun simpl = simplifyFun simpl mempty neverHoist
