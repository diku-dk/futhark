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
import Futhark.Representation.Basic
import Futhark.MonadFreshNames
import Futhark.Optimise.Simplifier.Rules
import qualified Futhark.Optimise.Simplifier.Engine as Engine


-- | Simplify the given program.  Even if the output differs from the
-- output, meaningful simplification may not have taken place - the
-- order of bindings may simply have been rearranged.  The function is
-- idempotent, however.
simplifyProg :: Prog -> Prog
simplifyProg = removeProgAliases . Engine.simplifyProg standardRules

-- | Simplify just a single function declaration.
simplifyFun :: MonadFreshNames m => FunDec -> m FunDec
simplifyFun = liftM removeFunDecAliases . Engine.simplifyOneFun standardRules

-- | Simplify just a single 'Lambda'.
simplifyOneLambda :: MonadFreshNames m => Prog -> Lambda -> m Lambda
simplifyOneLambda prog = liftM removeLambdaAliases . Engine.simplifyOneLambda standardRules (Just prog)
