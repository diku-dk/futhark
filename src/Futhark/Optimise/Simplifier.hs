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
import Futhark.Binder
import Futhark.Representation.Aliases
  (removeProgAliases, removeFunDecAliases, removeLambdaAliases)
import Futhark.Optimise.Simplifier.Rule (RuleBook)
import Futhark.Optimise.Simplifier.Rules
import Futhark.Optimise.Simplifier.Simplifiable

-- | Simplify the given program.  Even if the output differs from the
-- output, meaningful simplification may not have taken place - the
-- order of bindings may simply have been rearranged.
simplifyProgWithRules :: Proper lore =>
                         Simplifiable (SimpleM lore)
                      -> RuleBook (SimpleM lore)
                      -> Prog lore -> Prog lore
simplifyProgWithRules simpl rules =
  removeProgAliases . simplifyProg simpl rules

-- | Simplify just a single function declaration.
simplifyFunWithRules :: (MonadFreshNames m, Proper lore) =>
                        Simplifiable (SimpleM lore)
                     -> RuleBook (SimpleM lore)
                     -> FunDec lore
                     -> m (FunDec lore)
simplifyFunWithRules simpl rules =
  liftM removeFunDecAliases .
  simplifyFun simpl rules

-- | Simplify just a single 'Lambda'.
simplifyLambdaWithRules :: (MonadFreshNames m, Proper lore) =>
                           Simplifiable (SimpleM lore)
                        -> RuleBook (SimpleM lore)
                        -> Prog lore
                        -> Lambda lore
                        -> [Maybe Ident]
                        -> m (Lambda lore)
simplifyLambdaWithRules simpl rules prog lam args =
  liftM removeLambdaAliases $
  simplifyLambda simpl rules (Just prog) lam args
