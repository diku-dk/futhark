{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
-- |
--
-- Apply the simplification engine to an entire program, using some
-- set of simplification rules.
--
module Futhark.Optimise.Simplifier
  ( -- * Simple interface
    simplifyProgWithRules
  , simplifyFunWithRules
  , simplifyLambdaWithRules
  , simplifyStmsWithRules
  , standardRules
  , RuleBook
  , noExtraHoistBlockers
  , HoistBlockers (..)
  , simplifyBasicish
  , bindableSimpleOps
  , SimpleOps
  , SimplifyOp
  )
  where

import Data.Functor

import Prelude

import Futhark.Binder.Class (Bindable)
import Futhark.Representation.AST
import Futhark.MonadFreshNames
import Futhark.Optimise.Simplifier.Lore
  (removeProgWisdom, removeFunDefWisdom, removeLambdaWisdom, removeStmWisdom)
import Futhark.Optimise.Simplifier.Rule (RuleBook)
import Futhark.Optimise.Simplifier.Rules
import Futhark.Optimise.Simplifier.Simplify
import Futhark.Optimise.Simplifier.Engine
  (SimplifiableLore, HoistBlockers(..), noExtraHoistBlockers)

-- | Simplify the given program.  Even if the output differs from the
-- output, meaningful simplification may not have taken place - the
-- order of bindings may simply have been rearranged.
simplifyProgWithRules :: (MonadFreshNames m, SimplifiableLore lore) =>
                         SimpleOps lore
                      -> RuleBook (SimpleM lore)
                      -> HoistBlockers (SimpleM lore)
                      -> Prog lore -> m (Prog lore)
simplifyProgWithRules simpl rules blockers =
  fmap removeProgWisdom .
  simplifyProg simpl rules blockers

-- | Simplify just a single function declaration.
simplifyFunWithRules :: (MonadFreshNames m, SimplifiableLore lore) =>
                        SimpleOps lore
                     -> RuleBook (SimpleM lore)
                     -> HoistBlockers (SimpleM lore)
                     -> FunDef lore
                     -> m (FunDef lore)
simplifyFunWithRules simpl rules blockers =
  fmap removeFunDefWisdom .
  simplifyFun simpl rules blockers

-- | Simplify just a single 'Lambda'.
simplifyLambdaWithRules :: (MonadFreshNames m, HasScope lore m, SimplifiableLore lore) =>
                           SimpleOps lore
                        -> RuleBook (SimpleM lore)
                        -> HoistBlockers (SimpleM lore)
                        -> Lambda lore
                        -> Maybe [SubExp]
                        -> [Maybe VName]
                        -> m (Lambda lore)
simplifyLambdaWithRules simpl rules blockers lam nes =
  fmap removeLambdaWisdom .
  simplifyLambda simpl rules blockers lam nes

-- | Simplify a list of 'Stm's.
simplifyStmsWithRules :: (MonadFreshNames m, HasScope lore m, SimplifiableLore lore) =>
                             SimpleOps lore
                          -> RuleBook (SimpleM lore)
                          -> HoistBlockers (SimpleM lore)
                          -> [Stm lore]
                          -> m [Stm lore]
simplifyStmsWithRules simpl rules blockers bnds =
  map removeStmWisdom <$>
  simplifyStms simpl rules blockers bnds

simplifyBasicish :: (MonadFreshNames m, Bindable lore,
                     SimplifiableLore lore) =>
                    SimplifyOp lore ->
                    Prog lore -> m (Prog lore)
simplifyBasicish simplifyOp =
  simplifyProgWithRules (bindableSimpleOps simplifyOp) standardRules noExtraHoistBlockers
