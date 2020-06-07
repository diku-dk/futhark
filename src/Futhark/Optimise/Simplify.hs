{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Strict #-}
module Futhark.Optimise.Simplify
  ( simplifyProg
  , simplifySomething
  , simplifyFun
  , simplifyLambda
  , simplifyStms

  , Engine.SimpleOps (..)
  , Engine.SimpleM
  , Engine.SimplifyOp
  , Engine.bindableSimpleOps
  , Engine.noExtraHoistBlockers
  , Engine.neverHoist
  , Engine.SimplifiableLore
  , Engine.HoistBlockers
  , RuleBook
  )
  where

import Data.Bifunctor (second)
import Futhark.IR
import Futhark.MonadFreshNames
import qualified Futhark.Optimise.Simplify.Engine as Engine
import qualified Futhark.Analysis.SymbolTable as ST
import qualified Futhark.Analysis.UsageTable as UT
import Futhark.Optimise.Simplify.Rule
import Futhark.Optimise.Simplify.Lore
import Futhark.Pass

-- | Simplify the given program.  Even if the output differs from the
-- output, meaningful simplification may not have taken place - the
-- order of bindings may simply have been rearranged.
simplifyProg :: Engine.SimplifiableLore lore =>
                Engine.SimpleOps lore
             -> RuleBook (Engine.Wise lore)
             -> Engine.HoistBlockers lore
             -> Prog lore
             -> PassM (Prog lore)
simplifyProg simpl rules blockers (Prog consts funs) = do
  (consts_vtable, consts') <-
    simplifyConsts (UT.usages $ foldMap (freeIn . funDefBody) funs)
                   (mempty, consts)

  funs' <- parPass (simplifyFun' consts_vtable) funs
  let funs_uses = UT.usages $ foldMap (freeIn . funDefBody) funs'

  (_, consts'') <- simplifyConsts funs_uses (mempty, consts')

  return $ Prog consts'' funs'

  where simplifyFun' consts_vtable =
          simplifySomething
          (Engine.localVtable (consts_vtable<>) . Engine.simplifyFun)
          removeFunDefWisdom
          simpl rules blockers mempty

        simplifyConsts uses =
          simplifySomething (onConsts uses . snd)
          (second (removeStmWisdom<$>))
          simpl rules blockers mempty

        onConsts uses consts' = do
          (_, consts'') <-
            Engine.simplifyStms consts' (pure ((), mempty))
          (consts''', _) <-
            Engine.hoistStms rules (Engine.isFalse False) mempty uses consts''
          return (ST.insertStms consts''' mempty, consts''')

-- | Run a simplification operation to convergence.
simplifySomething :: (MonadFreshNames m, Engine.SimplifiableLore lore) =>
                     (a -> Engine.SimpleM lore b)
                  -> (b -> a)
                  -> Engine.SimpleOps lore
                  -> RuleBook (Wise lore)
                  -> Engine.HoistBlockers lore
                  -> ST.SymbolTable (Wise lore)
                  -> a
                  -> m a
simplifySomething f g simpl rules blockers vtable x = do
  let f' x' = Engine.localVtable (vtable<>) $ f x'
  loopUntilConvergence env simpl f' g x
  where env = Engine.emptyEnv rules blockers

-- | Simplify the given function.  Even if the output differs from the
-- output, meaningful simplification may not have taken place - the
-- order of bindings may simply have been rearranged.  Runs in a loop
-- until convergence.
simplifyFun :: (MonadFreshNames m, Engine.SimplifiableLore lore) =>
                Engine.SimpleOps lore
             -> RuleBook (Engine.Wise lore)
             -> Engine.HoistBlockers lore
             -> ST.SymbolTable (Wise lore)
             -> FunDef lore
             -> m (FunDef lore)
simplifyFun = simplifySomething Engine.simplifyFun removeFunDefWisdom

-- | Simplify just a single t'Lambda'.
simplifyLambda :: (MonadFreshNames m, HasScope lore m,
                   Engine.SimplifiableLore lore) =>
                  Engine.SimpleOps lore
               -> RuleBook (Engine.Wise lore)
               -> Engine.HoistBlockers lore
               -> Lambda lore
               -> m (Lambda lore)
simplifyLambda simpl rules blockers orig_lam = do
  vtable <- ST.fromScope . addScopeWisdom <$> askScope
  simplifySomething Engine.simplifyLambdaNoHoisting
    removeLambdaWisdom simpl rules blockers vtable orig_lam

-- | Simplify a list of 'Stm's.
simplifyStms :: (MonadFreshNames m, Engine.SimplifiableLore lore) =>
                Engine.SimpleOps lore
             -> RuleBook (Engine.Wise lore)
             -> Engine.HoistBlockers lore
             -> Scope lore
             -> Stms lore
             -> m (ST.SymbolTable (Wise lore), Stms lore)
simplifyStms simpl rules blockers scope =
  simplifySomething f g simpl rules blockers vtable . (mempty,)
  where vtable = ST.fromScope $ addScopeWisdom scope
        f (_, stms) =
          Engine.simplifyStms stms ((,mempty) <$> Engine.askVtable)
        g = second $ fmap removeStmWisdom

loopUntilConvergence :: (MonadFreshNames m, Engine.SimplifiableLore lore) =>
                        Engine.Env lore
                     -> Engine.SimpleOps lore
                     -> (a -> Engine.SimpleM lore b)
                     -> (b -> a)
                     -> a
                     -> m a
loopUntilConvergence env simpl f g x = do
  (x', changed) <- modifyNameSource $ Engine.runSimpleM (f x) simpl env
  if changed then loopUntilConvergence env simpl f g (g x') else return $ g x'
