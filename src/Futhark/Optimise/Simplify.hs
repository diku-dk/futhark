{-# LANGUAGE Strict #-}

module Futhark.Optimise.Simplify
  ( simplifyProg,
    simplifySomething,
    simplifyFun,
    simplifyLambda,
    simplifyStms,
    Engine.SimpleOps (..),
    Engine.SimpleM,
    Engine.SimplifyOp,
    Engine.bindableSimpleOps,
    Engine.noExtraHoistBlockers,
    Engine.neverHoist,
    Engine.SimplifiableRep,
    Engine.HoistBlockers,
    RuleBook,
  )
where

import Futhark.Analysis.SymbolTable qualified as ST
import Futhark.Analysis.UsageTable qualified as UT
import Futhark.IR
import Futhark.MonadFreshNames
import Futhark.Optimise.Simplify.Engine qualified as Engine
import Futhark.Optimise.Simplify.Rep
import Futhark.Optimise.Simplify.Rule
import Futhark.Pass

funDefUsages :: (FreeIn a) => [a] -> UT.UsageTable
funDefUsages funs =
  -- XXX: treat every constant used in the functions as consumed,
  -- because it is otherwise complicated to ensure we do not introduce
  -- more aliasing than specified by the return types.  CSE has the
  -- same problem.
  let free_in_funs = foldMap freeIn funs
   in UT.usages free_in_funs
        <> foldMap UT.consumedUsage (namesToList free_in_funs)

-- | Simplify the given program.  Even if the output differs from the
-- output, meaningful simplification may not have taken place - the
-- order of bindings may simply have been rearranged.
simplifyProg ::
  (Engine.SimplifiableRep rep) =>
  Engine.SimpleOps rep ->
  RuleBook (Engine.Wise rep) ->
  Engine.HoistBlockers rep ->
  Prog rep ->
  PassM (Prog rep)
simplifyProg simpl rules blockers prog = do
  let consts = progConsts prog
      funs = progFuns prog
  (consts_vtable, consts') <-
    simplifyConsts (funDefUsages funs) (ST.empty, informStms consts)

  -- We deepen the vtable so it will look like the constants are in an
  -- "outer loop"; this communicates useful information to some
  -- simplification rules (e.g. see issue #1302).
  funs' <- parPass (simplifyFun' (ST.deepen consts_vtable) . informFunDef) funs
  (_, consts'') <- simplifyConsts (funDefUsages funs') (ST.empty, consts')

  pure $
    prog
      { progConsts = fmap removeStmWisdom consts'',
        progFuns = fmap removeFunDefWisdom funs'
      }
  where
    simplifyFun' consts_vtable =
      simplifySomething
        (Engine.localVtable (const consts_vtable) . Engine.simplifyFun)
        id
        simpl
        rules
        blockers
        ST.empty

    simplifyConsts uses =
      simplifySomething
        (onConsts uses . snd)
        id
        simpl
        rules
        blockers
        ST.empty

    onConsts uses consts' = do
      consts'' <- Engine.simplifyStmsWithUsage uses consts'
      pure (ST.insertStms consts'' ST.empty, consts'')

-- | Run a simplification operation to convergence.
simplifySomething ::
  (MonadFreshNames m, Engine.SimplifiableRep rep) =>
  (a -> Engine.SimpleM rep b) ->
  (b -> a) ->
  Engine.SimpleOps rep ->
  RuleBook (Wise rep) ->
  Engine.HoistBlockers rep ->
  ST.SymbolTable (Wise rep) ->
  a ->
  m a
simplifySomething f g simpl rules blockers vtable x = do
  let f' x' = Engine.localVtable (const vtable) $ f x'
  loopUntilConvergence env simpl f' g x
  where
    env = Engine.emptyEnv rules blockers

-- | Simplify the given function.  Even if the output differs from the
-- output, meaningful simplification may not have taken place - the
-- order of bindings may simply have been rearranged.  Runs in a loop
-- until convergence.
simplifyFun ::
  (MonadFreshNames m, Engine.SimplifiableRep rep) =>
  Engine.SimpleOps rep ->
  RuleBook (Engine.Wise rep) ->
  Engine.HoistBlockers rep ->
  ST.SymbolTable (Wise rep) ->
  FunDef rep ->
  m (FunDef rep)
simplifyFun simpl rules blockers vtable fd =
  removeFunDefWisdom
    <$> simplifySomething
      Engine.simplifyFun
      id
      simpl
      rules
      blockers
      vtable
      (informFunDef fd)

-- | Simplify just a single t'Lambda'.
simplifyLambda ::
  ( MonadFreshNames m,
    HasScope rep m,
    Engine.SimplifiableRep rep
  ) =>
  Engine.SimpleOps rep ->
  RuleBook (Engine.Wise rep) ->
  Engine.HoistBlockers rep ->
  Lambda rep ->
  m (Lambda rep)
simplifyLambda simpl rules blockers orig_lam = do
  vtable <- ST.fromScope . addScopeWisdom <$> askScope
  removeLambdaWisdom
    <$> simplifySomething
      Engine.simplifyLambdaNoHoisting
      id
      simpl
      rules
      blockers
      vtable
      (informLambda orig_lam)

-- | Simplify a sequence of 'Stm's.
simplifyStms ::
  (MonadFreshNames m, Engine.SimplifiableRep rep) =>
  Engine.SimpleOps rep ->
  RuleBook (Engine.Wise rep) ->
  Engine.HoistBlockers rep ->
  Scope rep ->
  Stms rep ->
  m (Stms rep)
simplifyStms simpl rules blockers scope =
  fmap (fmap removeStmWisdom)
    . simplifySomething Engine.simplifyStms id simpl rules blockers vtable
    . informStms
  where
    vtable = ST.fromScope $ addScopeWisdom scope

loopUntilConvergence ::
  (MonadFreshNames m, Engine.SimplifiableRep rep) =>
  Engine.Env rep ->
  Engine.SimpleOps rep ->
  (a -> Engine.SimpleM rep b) ->
  (b -> a) ->
  a ->
  m a
loopUntilConvergence env simpl f g x = do
  (x', changed) <- modifyNameSource $ Engine.runSimpleM (f x) simpl env
  if changed then loopUntilConvergence env simpl f g (g x') else pure $ g x'
