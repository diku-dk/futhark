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
  , standardRules
  , basicRules
  , RuleBook
  )
  where

import Control.Monad

import Futhark.Representation.AST
import Futhark.MonadFreshNames
import Futhark.Optimise.Simplifier.Lore
  (removeProgWisdom, removeFunDecWisdom, removeLambdaWisdom)
import Futhark.Optimise.Simplifier.Rule (RuleBook)
import Futhark.Optimise.Simplifier.Rules
import Futhark.Optimise.Simplifier.Simplify

-- | Simplify the given program.  Even if the output differs from the
-- output, meaningful simplification may not have taken place - the
-- order of bindings may simply have been rearranged.
simplifyProgWithRules :: (MonadFreshNames m, Simplifiable lore) =>
                         SimpleOps (SimpleM lore)
                      -> RuleBook (SimpleM lore)
                      -> Prog lore -> m (Prog lore)
simplifyProgWithRules simpl rules =
  liftM removeProgWisdom .
  simplifyProg simpl rules

-- | Simplify just a single function declaration.
simplifyFunWithRules :: (MonadFreshNames m, Simplifiable lore) =>
                        SimpleOps (SimpleM lore)
                     -> RuleBook (SimpleM lore)
                     -> FunDec lore
                     -> m (FunDec lore)
simplifyFunWithRules simpl rules =
  liftM removeFunDecWisdom .
  simplifyFun simpl rules

-- | Simplify just a single 'Lambda'.
simplifyLambdaWithRules :: (MonadFreshNames m, HasTypeEnv m, Simplifiable lore) =>
                           SimpleOps (SimpleM lore)
                        -> RuleBook (SimpleM lore)
                        -> Prog lore
                        -> Lambda lore
                        -> SubExp
                        -> [Maybe VName]
                        -> m (Lambda lore)
simplifyLambdaWithRules simpl rules prog lam w args =
  liftM removeLambdaWisdom $
  simplifyLambda simpl rules (Just prog) lam w args
