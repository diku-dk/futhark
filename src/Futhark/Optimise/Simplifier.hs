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

import Prelude

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
  fmap removeProgWisdom .
  simplifyProg simpl rules blockers

-- | Simplify just a single function declaration.
simplifyFunWithRules :: (MonadFreshNames m, MonadEngine (SimpleM lore)) =>
                        SimpleOps (SimpleM lore)
                     -> RuleBook (SimpleM lore)
                     -> HoistBlockers (SimpleM lore)
                     -> FunDec lore
                     -> m (FunDec lore)
simplifyFunWithRules simpl rules blockers =
  fmap removeFunDecWisdom .
  simplifyFun simpl rules blockers

-- | Simplify just a single 'Lambda'.
simplifyLambdaWithRules :: (MonadFreshNames m,
                            HasScope lore m,
                            MonadEngine (SimpleM lore)) =>
                           SimpleOps (SimpleM lore)
                        -> RuleBook (SimpleM lore)
                        -> HoistBlockers (SimpleM lore)
                        -> Lambda lore
                        -> SubExp
                        -> Maybe [SubExp]
                        -> [Maybe VName]
                        -> m (Lambda lore)
simplifyLambdaWithRules simpl rules blockers lam w nes =
  fmap removeLambdaWisdom .
  simplifyLambda simpl rules blockers lam w nes

-- | Simplify a list of 'Binding's.
simplifyBindingsWithRules :: (MonadFreshNames m,
                              HasScope lore m,
                              MonadEngine (SimpleM lore)) =>
                             SimpleOps (SimpleM lore)
                          -> RuleBook (SimpleM lore)
                          -> HoistBlockers (SimpleM lore)
                          -> [Binding lore]
                          -> m [Binding lore]
simplifyBindingsWithRules simpl rules blockers bnds =
  map removeBindingWisdom <$>
  simplifyBindings simpl rules blockers bnds

simplifyBasicish :: (MonadFreshNames m, Bindable lore,
                     Simplifiable (LetAttr lore),
                     Simplifiable (FParamAttr lore),
                     Simplifiable (LParamAttr lore),
                     Simplifiable (RetType lore),
                     SimplifiableOp lore (Op lore)) =>
                    Prog lore -> m (Prog lore)
simplifyBasicish =
  simplifyProgWithRules bindableSimpleOps standardRules noExtraHoistBlockers
