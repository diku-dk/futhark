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
import Futhark.Optimise.Simplifier.Rule
import Futhark.Optimise.Simplifier.Simple
import Futhark.Optimise.Simplifier.Lore (addTypeEnvWisdom)
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
simplifyProg simpl rules blockers =
  intraproceduralTransformation $
  simplifyFun simpl rules blockers

-- | Simplify the given function.  Even if the output differs from the
-- output, meaningful simplification may not have taken place - the
-- order of bindings may simply have been rearranged.
simplifyFun :: (MonadFreshNames m, Engine.MonadEngine (SimpleM lore)) =>
                SimpleOps (SimpleM lore)
             -> RuleBook (SimpleM lore)
             -> Engine.HoistBlockers (SimpleM lore)
             -> FunDec lore
             -> m (FunDec (Wise lore))
simplifyFun simpl rules blockers fundec =
  modifyNameSource $ runSimpleM (Engine.simplifyFun fundec) simpl $
  Engine.emptyEnv rules blockers

-- | Simplify just a single 'Lambda'.
simplifyLambda :: (MonadFreshNames m,
                   HasTypeEnv (NameType lore) m,
                   Engine.MonadEngine (SimpleM lore)) =>
                  SimpleOps (SimpleM lore)
               -> RuleBook (SimpleM lore)
               -> Engine.HoistBlockers (SimpleM lore)
               -> Lambda lore -> SubExp -> [Maybe VName]
               -> m (Lambda (Wise lore))
simplifyLambda simpl rules blockers lam w args = do
  types <- askTypeEnv
  let m = Engine.localVtable
          (<> ST.fromTypeEnv (addTypeEnvWisdom types)) $
          Engine.simplifyLambdaNoHoisting lam w args
  modifyNameSource $ runSimpleM m simpl $
    Engine.emptyEnv rules blockers

-- | Simplify a list of 'Binding's.
simplifyBindings :: (MonadFreshNames m,
                     HasTypeEnv (NameType lore) m,
                     Engine.MonadEngine (SimpleM lore)) =>
                    SimpleOps (SimpleM lore)
                 -> RuleBook (SimpleM lore)
                 -> Engine.HoistBlockers (SimpleM lore)
                 -> [Binding lore]
                 -> m [Binding (Wise lore)]
simplifyBindings simpl rules blockers bnds = do
  types <- askTypeEnv
  let m = Engine.localVtable
          (<> ST.fromTypeEnv (addTypeEnvWisdom types)) $
          fmap snd $ Engine.collectBindingsEngine $
          mapM_ Engine.simplifyBinding bnds
  modifyNameSource $ runSimpleM m simpl $
    Engine.emptyEnv rules blockers
