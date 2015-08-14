module Futhark.Optimise.SimpleOpts
  ( simpleOptProg
  , CallGraph
  , buildCallGraph
  , aggInlineDriver
  , simpleOptLambda
  , Error(..)
    -- * Re-exports
  , bindableSimpleOps
  , RuleBook
  , basicRules
  , standardRules
  )
  where

import Futhark.Representation.AST
import qualified Futhark.Representation.Basic as Basic
import Futhark.MonadFreshNames

import Futhark.Optimise.InliningDeadFun
import Futhark.Optimise.Simplifier
import Futhark.Optimise.Simplifier.Simple
import Futhark.Optimise.DeadVarElim
import Futhark.Optimise.Errors

simpleOptProg :: Simplifiable lore =>
                 SimpleOps (SimpleM lore)
              -> RuleBook (SimpleM lore)
              -> Prog lore -> Either Error (Prog lore)
simpleOptProg simpl rules =
  -- XXX: A given simplification rule may leave the program in a form
  -- that is technically type-incorrect, but which will be correct
  -- after copy-propagation.  Right now, we just run the simplifier a
  -- number of times and hope that it is enough.  Will be fixed later;
  -- promise.
  Right . foldl (.) id (replicate num_passes pass)
  where pass = deadCodeElim . simplifyProgWithRules simpl rules
        num_passes = 5

simpleOptLambda :: (MonadFreshNames m, HasTypeEnv m) =>
                   Basic.Prog
                -> Basic.Lambda
                -> SubExp
                -> [Maybe VName]
                -> m Basic.Lambda
simpleOptLambda = simplifyLambdaWithRules bindableSimpleOps basicRules
