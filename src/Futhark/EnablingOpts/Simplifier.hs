{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |
--
-- Apply the simplification engine
-- ("Futhark.EnablingOpts.Simplifier.Engine") to an entire program,
-- using the default simplification rules.
--
module Futhark.EnablingOpts.Simplifier
  ( simplifyProg
  , simplifyOneLambda
  )
  where

import Futhark.InternalRep
import Futhark.MonadFreshNames
import Futhark.EnablingOpts.Simplifier.Rules
import qualified Futhark.EnablingOpts.Simplifier.Engine as Engine

-- | Simplify the given program.  Even if the output differs from the
-- output, meaningful simplification may not have taken place - the
-- order of bindings may simply have been rearranged.  The function is
-- idempotent, however.
simplifyProg :: Prog -> Prog
simplifyProg = Engine.simplifyProg standardRules

-- | Simplify just a single 'Lambda'.
simplifyOneLambda :: MonadFreshNames m => Prog -> Lambda -> m Lambda
simplifyOneLambda = Engine.simplifyOneLambda standardRules . Just
