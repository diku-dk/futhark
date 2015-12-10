{-# LANGUAGE FlexibleContexts #-}
module Futhark.Optimise.Simplifier.Simplify
  ( SimpleOps (..)
  , SimpleM
  , bindableSimpleOps
  , simplifyProg
  , simplifyFun
  , simplifyLambda
  , simplifyBindings
  )
  where

import Data.Monoid

import Futhark.Representation.AST
import Futhark.MonadFreshNames
import qualified Futhark.Optimise.Simplifier.Engine as Engine
import qualified Futhark.Analysis.SymbolTable as ST
import Futhark.Optimise.Simplifier.Lore (Wise)
import Futhark.Optimise.Simplifier.Rule
import Futhark.Optimise.Simplifier.Simple
import Futhark.Tools (intraproceduralTransformation)

-- | Simplify the given program.  Even if the output differs from the
-- output, meaningful simplification may not have taken place - the
-- order of bindings may simply have been rearranged.
simplifyProg :: (MonadFreshNames m, Engine.MonadEngine (SimpleM lore)) =>
                SimpleOps (SimpleM lore)
             -> RuleBook (SimpleM lore)
             -> Engine.HoistBlockers (SimpleM lore)
             -> Prog lore
             -> m (Prog (Wise lore))
simplifyProg simpl rules blockers prog =
  intraproceduralTransformation
  (simplifyFun' simpl rules blockers (Just prog))
  prog

-- | Simplify the given function.  Even if the output differs from the
-- output, meaningful simplification may not have taken place - the
-- order of bindings may simply have been rearranged.
simplifyFun :: (MonadFreshNames m, Engine.MonadEngine (SimpleM lore)) =>
               SimpleOps (SimpleM lore)
            -> RuleBook (SimpleM lore)
            -> Engine.HoistBlockers (SimpleM lore)
            -> FunDec lore
            -> m (FunDec (Wise lore))
simplifyFun simpl rules blockers =
  simplifyFun' simpl rules blockers Nothing

simplifyFun' :: (MonadFreshNames m, Engine.MonadEngine (SimpleM lore)) =>
                SimpleOps (SimpleM lore)
             -> RuleBook (SimpleM lore)
             -> Engine.HoistBlockers (SimpleM lore)
             -> Maybe (Prog lore)
             -> FunDec lore
             -> m (FunDec (Wise lore))
simplifyFun' simpl rules blockers prog fundec =
  modifyNameSource $ runSimpleM (Engine.simplifyFun fundec) simpl $
  Engine.emptyEnv rules blockers prog

-- | Simplify just a single 'Lambda'.
simplifyLambda :: (MonadFreshNames m, HasTypeEnv m, Engine.MonadEngine (SimpleM lore)) =>
                  SimpleOps (SimpleM lore)
               -> RuleBook (SimpleM lore)
               -> Engine.HoistBlockers (SimpleM lore)
               -> Maybe (Prog lore)
               -> Lambda lore -> SubExp -> [Maybe VName]
               -> m (Lambda (Wise lore))
simplifyLambda simpl rules blockers prog lam w args = do
  types <- askTypeEnv
  let m = Engine.localVtable (<> ST.fromTypeEnv types) $
          Engine.simplifyLambdaNoHoisting lam w args
  modifyNameSource $ runSimpleM m simpl $
    Engine.emptyEnv rules blockers prog

-- | Simplify a list of 'Binding's.
simplifyBindings :: (MonadFreshNames m, HasTypeEnv m, Engine.MonadEngine (SimpleM lore)) =>
                    SimpleOps (SimpleM lore)
                 -> RuleBook (SimpleM lore)
                 -> Engine.HoistBlockers (SimpleM lore)
                 -> Maybe (Prog lore) -> [Binding lore]
                 -> m [Binding (Wise lore)]
simplifyBindings simpl rules blockers prog bnds = do
  types <- askTypeEnv
  let m = Engine.localVtable (<> ST.fromTypeEnv types) $
          fmap snd $ Engine.collectBindingsEngine $
          mapM_ Engine.simplifyBinding bnds
  modifyNameSource $ runSimpleM m simpl $
    Engine.emptyEnv rules blockers prog
