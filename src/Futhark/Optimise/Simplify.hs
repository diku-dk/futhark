{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
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
  , Engine.SimplifiableLore
  , Engine.HoistBlockers
  , RuleBook
  )
  where

import Data.Monoid

import Futhark.Representation.AST
import Futhark.MonadFreshNames
import qualified Futhark.Optimise.Simplify.Engine as Engine
import qualified Futhark.Analysis.SymbolTable as ST
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
simplifyProg simpl rules blockers =
  intraproceduralTransformation $ simplifyFun simpl rules blockers

-- | Run a simplification operation to convergence.
simplifySomething :: (MonadFreshNames m, HasScope lore m,
                      Engine.SimplifiableLore lore) =>
                     (a -> Engine.SimpleM lore b)
                  -> (b -> a)
                  -> Engine.SimpleOps lore
                  -> RuleBook (Wise lore)
                  -> Engine.HoistBlockers lore
                  -> a
                  -> m a
simplifySomething f g simpl rules blockers x = do
  scope <- askScope
  let f' x' = Engine.localVtable (ST.fromScope (addScopeWisdom scope)<>) $ f x'
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
             -> FunDef lore
             -> m (FunDef lore)
simplifyFun simpl rules blockers =
  loopUntilConvergence env simpl Engine.simplifyFun removeFunDefWisdom
  where env = Engine.emptyEnv rules blockers

-- | Simplify just a single 'Lambda'.
simplifyLambda :: (MonadFreshNames m, HasScope lore m, Engine.SimplifiableLore lore) =>
                  Engine.SimpleOps lore
               -> RuleBook (Engine.Wise lore)
               -> Engine.HoistBlockers lore
               -> Lambda lore -> [Maybe VName]
               -> m (Lambda lore)
simplifyLambda simpl rules blockers orig_lam args =
  simplifySomething f removeLambdaWisdom simpl rules blockers orig_lam
  where f lam' = Engine.simplifyLambdaNoHoisting lam' args

-- | Simplify a list of 'Stm's.
simplifyStms :: (MonadFreshNames m, HasScope lore m, Engine.SimplifiableLore lore) =>
                Engine.SimpleOps lore
             -> RuleBook (Engine.Wise lore)
             -> Engine.HoistBlockers lore
             -> Stms lore
             -> m (Stms lore)
simplifyStms = simplifySomething f g
  where f stms = fmap snd $ Engine.simplifyStms stms $ return ((), mempty)
        g = fmap removeStmWisdom

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
