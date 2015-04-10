module Futhark.Optimise.SimpleOpts
  ( simpleOpts
  , CallGraph
  , buildCallGraph
  , aggInlineDriver
  , normCopyOneLambda
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

--import qualified Futhark.Optimise.AlgSimplify as AS
--import qualified Futhark.Analysis.ScalExp as ScExp
--import Debug.Trace

simpleOpts :: Simplifiable lore =>
              SimpleOps (SimpleM lore)
           -> RuleBook (SimpleM lore)
           -> Prog lore -> Either Error (Prog lore)
simpleOpts simpl rules prog = do
--  esc <- AS.canSimplify 3
--  let prog' = trace (ScExp.ppScalExp esc) prog
  let prog_enopt1   = pass prog
      prog_enopt2   = pass prog_enopt1
      prog_flat_opt = pass prog_enopt2

  return $ pass prog_flat_opt
  where pass = deadCodeElim . simplifyProgWithRules simpl rules

normCopyOneLambda :: (MonadFreshNames m, HasTypeEnv m) =>
                     Basic.Prog
                  -> Basic.Lambda
                  -> [Maybe VName]
                  -> m Basic.Lambda
normCopyOneLambda = simplifyLambdaWithRules bindableSimpleOps basicRules
