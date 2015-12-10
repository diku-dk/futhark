{-# LANGUAGE FlexibleContexts #-}
-- |
--
-- Apply the simplification engine
-- ("Futhark.Optimise.Simplifier.Engine") to an entire program, using
-- some set of simplification rules.
--
module Futhark.Optimise.Simplifier
  ( -- * Simple interface
    simplifyProgWithRules
  , simplifyFunWithRules
  , simplifyLambdaWithRules
  , simplifyBindingsWithRules
  , standardRules
  , basicRules
  , RuleBook
  , noExtraHoistBlockers
  , HoistBlockers (..)
  , simplifyBasicish
  )
  where

import Data.Functor
import Control.Monad

import Prelude

import qualified Futhark.Representation.AST.Annotations as Annotations
import Futhark.Binder.Class (Bindable)
import Futhark.Representation.AST
import Futhark.MonadFreshNames
import Futhark.Optimise.Simplifier.Lore
  (removeProgWisdom, removeFunDecWisdom, removeLambdaWisdom, removeBindingWisdom)
import Futhark.Optimise.Simplifier.Rule (RuleBook)
import Futhark.Optimise.Simplifier.Rules
import Futhark.Optimise.Simplifier.Simplify
import Futhark.Optimise.Simplifier.Engine
  (MonadEngine, HoistBlockers(..), noExtraHoistBlockers, Simplifiable, SimplifiableOp)

-- | Simplify the given program.  Even if the output differs from the
-- output, meaningful simplification may not have taken place - the
-- order of bindings may simply have been rearranged.
simplifyProgWithRules :: (MonadFreshNames m, MonadEngine (SimpleM lore)) =>
                         SimpleOps (SimpleM lore)
                      -> RuleBook (SimpleM lore)
                      -> HoistBlockers (SimpleM lore)
                      -> Prog lore -> m (Prog lore)
simplifyProgWithRules simpl rules blockers =
  liftM removeProgWisdom .
  simplifyProg simpl rules blockers

-- | Simplify just a single function declaration.
simplifyFunWithRules :: (MonadFreshNames m, MonadEngine (SimpleM lore)) =>
                        SimpleOps (SimpleM lore)
                     -> RuleBook (SimpleM lore)
                     -> HoistBlockers (SimpleM lore)
                     -> FunDec lore
                     -> m (FunDec lore)
simplifyFunWithRules simpl rules blockers =
  liftM removeFunDecWisdom .
  simplifyFun simpl rules blockers

-- | Simplify just a single 'Lambda'.
simplifyLambdaWithRules :: (MonadFreshNames m, HasTypeEnv m, MonadEngine (SimpleM lore)) =>
                           SimpleOps (SimpleM lore)
                        -> RuleBook (SimpleM lore)
                        -> HoistBlockers (SimpleM lore)
                        -> Prog lore
                        -> Lambda lore
                        -> SubExp
                        -> [Maybe VName]
                        -> m (Lambda lore)
simplifyLambdaWithRules simpl rules blockers prog lam w args =
  liftM removeLambdaWisdom $
  simplifyLambda simpl rules blockers (Just prog) lam w args

-- | Simplify a list of 'Binding's.
simplifyBindingsWithRules :: (MonadFreshNames m, HasTypeEnv m, MonadEngine (SimpleM lore)) =>
                             SimpleOps (SimpleM lore)
                          -> RuleBook (SimpleM lore)
                          -> HoistBlockers (SimpleM lore)
                          -> Maybe (Prog lore) -> [Binding lore]
                          -> m [Binding lore]
simplifyBindingsWithRules simpl rules blockers prog bnds =
  map removeBindingWisdom <$>
  simplifyBindings simpl rules blockers prog bnds

simplifyBasicish :: (MonadFreshNames m, Bindable lore,
                     Simplifiable (Annotations.LetBound lore),
                     Simplifiable (Annotations.FParam lore),
                     Simplifiable (Annotations.LParam lore),
                     Simplifiable (Annotations.RetType lore),
                     SimplifiableOp lore (Op lore)) =>
                    Prog lore -> m (Prog lore)
simplifyBasicish =
  simplifyProgWithRules bindableSimpleOps standardRules noExtraHoistBlockers
