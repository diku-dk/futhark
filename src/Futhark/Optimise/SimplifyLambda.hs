module Futhark.Optimise.SimplifyLambda
  ( simplifyLambda
  )
  where

import Futhark.Representation.AST
import qualified Futhark.Representation.Basic as Basic
import Futhark.MonadFreshNames
import Futhark.Optimise.Simplifier
import Futhark.Optimise.Simplifier.Simple

-- | Simplify a lambda using the standard simplification rules.
simplifyLambda :: (MonadFreshNames m, HasTypeEnv m) =>
                  Basic.Prog
               -> Basic.Lambda
               -> SubExp
               -> [Maybe VName]
               -> m Basic.Lambda
simplifyLambda = simplifyLambdaWithRules bindableSimpleOps basicRules
