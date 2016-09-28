{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module Futhark.Optimise.Simplifier.Simplify
  ( Engine.SimpleOps (..)
  , Engine.SimpleM
  , Engine.SimplifyOp
  , Engine.bindableSimpleOps
  , simplifyProg
  , simplifyFun
  , simplifyLambda
  , simplifyStms
  )
  where

import Data.Monoid

import Futhark.Representation.AST
import Futhark.MonadFreshNames
import qualified Futhark.Optimise.Simplifier.Engine as Engine
import qualified Futhark.Analysis.SymbolTable as ST
import Futhark.Optimise.Simplifier.Rule
import Futhark.Optimise.Simplifier.Lore (addScopeWisdom)
import Futhark.Tools (intraproceduralTransformation)

-- | Simplify the given program.  Even if the output differs from the
-- output, meaningful simplification may not have taken place - the
-- order of bindings may simply have been rearranged.
simplifyProg :: (MonadFreshNames m, Engine.SimplifiableLore lore) =>
                Engine.SimpleOps lore
             -> RuleBook (Engine.SimpleM lore)
             -> Engine.HoistBlockers (Engine.SimpleM lore)
             -> Prog lore
             -> m (Prog (Engine.Wise lore))
simplifyProg simpl rules blockers =
  intraproceduralTransformation $
  simplifyFun simpl rules blockers

-- | Simplify the given function.  Even if the output differs from the
-- output, meaningful simplification may not have taken place - the
-- order of bindings may simply have been rearranged.
simplifyFun :: (MonadFreshNames m, Engine.SimplifiableLore lore) =>
                Engine.SimpleOps lore
             -> RuleBook (Engine.SimpleM lore)
             -> Engine.HoistBlockers (Engine.SimpleM lore)
             -> FunDef lore
             -> m (FunDef (Engine.Wise lore))
simplifyFun simpl rules blockers fundec =
  modifyNameSource $
  Engine.runSimpleM (Engine.simplifyFun fundec) simpl $
  Engine.emptyEnv rules blockers

-- | Simplify just a single 'Lambda'.
simplifyLambda :: (MonadFreshNames m, HasScope lore m, Engine.SimplifiableLore lore) =>
                  Engine.SimpleOps lore
               -> RuleBook (Engine.SimpleM lore)
               -> Engine.HoistBlockers (Engine.SimpleM lore)
               -> Lambda lore -> Maybe [SubExp] -> [Maybe VName]
               -> m (Lambda (Engine.Wise lore))
simplifyLambda simpl rules blockers lam nes args = do
  types <- askScope
  let m = Engine.localVtable
          (<> ST.fromScope (addScopeWisdom types)) $
          Engine.simplifyLambdaNoHoisting lam nes args
  modifyNameSource $ Engine.runSimpleM m simpl $
    Engine.emptyEnv rules blockers

-- | Simplify a list of 'Stm's.
simplifyStms :: (MonadFreshNames m, HasScope lore m, Engine.SimplifiableLore lore) =>
                Engine.SimpleOps lore
             -> RuleBook (Engine.SimpleM lore)
             -> Engine.HoistBlockers (Engine.SimpleM lore)
             -> [Stm lore]
             -> m [Stm (Engine.Wise lore)]
simplifyStms simpl rules blockers bnds = do
  types <- askScope
  let m = Engine.localVtable
          (<> ST.fromScope (addScopeWisdom types)) $
          fmap snd $ Engine.collectStmsEngine $
          mapM_ Engine.simplifyStm bnds
  modifyNameSource $ Engine.runSimpleM m simpl $
    Engine.emptyEnv rules blockers
