-- |
--
-- Apply the simplification engine
-- ("Futhark.Optimise.Simplifier.Engine") to an entire program, using
-- some set of simplification rules.
--
module Futhark.Optimise.Simplifier
  ( -- * Simple interface
    simplifyProgWithStandardRules
  , simplifyFunWithStandardRules
  , simplifyLambdaWithStandardRules
  )
  where

import Control.Monad

import Futhark.Representation.AST
import Futhark.MonadFreshNames
import Futhark.Binder
import Futhark.Representation.Aliases
  (removeProgAliases, removeFunDecAliases, removeLambdaAliases)
import Futhark.Optimise.Simplifier.Rules
import Futhark.Optimise.Simplifier.Simplifiable

-- | Simplify the given program.  Even if the output differs from the
-- output, meaningful simplification may not have taken place - the
-- order of bindings may simply have been rearranged.
simplifyProgWithStandardRules :: Proper lore =>
                                 Simplifiable (SimpleM lore)
                              -> Prog lore -> Prog lore
simplifyProgWithStandardRules simpl =
  removeProgAliases .
  simplifyProg simpl standardRules

-- | Simplify just a single function declaration.
simplifyFunWithStandardRules :: (MonadFreshNames m, Proper lore) =>
                                Simplifiable (SimpleM lore)
                             -> FunDec lore
                             -> m (FunDec lore)
simplifyFunWithStandardRules simpl =
  liftM removeFunDecAliases .
  simplifyFun simpl standardRules

-- | Simplify just a single 'Lambda'.
simplifyLambdaWithStandardRules :: (MonadFreshNames m, Proper lore) =>
                                   Simplifiable (SimpleM lore)
                                -> Prog lore
                                -> Lambda lore
                                -> [Maybe SubExp]
                                -> m (Lambda lore)
simplifyLambdaWithStandardRules simpl prog lam args =
  liftM removeLambdaAliases $
  simplifyLambda simpl standardRules (Just prog) lam args
