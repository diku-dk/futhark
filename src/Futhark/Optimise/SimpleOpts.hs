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
simpleOptProg simpl rules prog = do
  let prog_enopt1   = pass prog
      prog_enopt2   = pass prog_enopt1
      prog_flat_opt = pass prog_enopt2

  return $ pass prog_flat_opt
  where pass = deadCodeElim . simplifyProgWithRules simpl rules

simpleOptLambda :: (MonadFreshNames m, HasTypeEnv m) =>
                     Basic.Prog
                  -> Basic.Lambda
                  -> SubExp
                  -> [Maybe VName]
                  -> m Basic.Lambda
simpleOptLambda = simplifyLambdaWithRules bindableSimpleOps basicRules
