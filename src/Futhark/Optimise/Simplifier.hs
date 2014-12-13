-- |
--
-- Apply the simplification engine
-- ("Futhark.Optimise.Simplifier.Engine") to an entire program,
-- using the default simplification rules.
--
module Futhark.Optimise.Simplifier
  ( -- * Simple interface
    simplifyProg
  , simplifyFun
  , simplifyOneLambda
  )
  where

import Control.Monad
import Futhark.Representation.Aliases
  (removeProgAliases, removeFunDecAliases, removeLambdaAliases)
import Futhark.Representation.AST
import Futhark.MonadFreshNames
import Futhark.Optimise.Simplifier.Rules
import qualified Futhark.Optimise.Simplifier.Simplifiable as Engine

-- | Simplify the given program.  Even if the output differs from the
-- output, meaningful simplification may not have taken place - the
-- order of bindings may simply have been rearranged.
simplifyProg :: Engine.Simplifiable lore =>
                Prog lore -> Prog lore
simplifyProg = removeProgAliases . Engine.simplifyProg standardRules

-- | Simplify just a single function declaration.
simplifyFun :: (MonadFreshNames m, Engine.Simplifiable lore) =>
               FunDec lore -> m (FunDec lore)
simplifyFun = liftM removeFunDecAliases . Engine.simplifyOneFun standardRules

-- | Simplify just a single 'Lambda'.
simplifyOneLambda :: (MonadFreshNames m, Engine.Simplifiable lore) =>
                     Prog lore -> Lambda lore -> [Maybe SubExp] -> m (Lambda lore)
simplifyOneLambda prog lam args =
  liftM removeLambdaAliases $
  Engine.simplifyOneLambda standardRules (Just prog) lam args
