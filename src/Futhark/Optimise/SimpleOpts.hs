module Futhark.Optimise.SimpleOpts
  ( simpleOpts
  , CallGraph
  , buildCallGraph
  , aggInlineDriver
  , normCopyOneLambda
  , Error(..)
    -- * Re-exports
  , bindableSimplifiable
  , RuleBook
  , basicRules
  , standardRules
  )
  where

import Futhark.Representation.AST
import qualified Futhark.Representation.Basic as Basic
import Futhark.MonadFreshNames
import Futhark.Binder (Proper)

import Futhark.Optimise.InliningDeadFun
import Futhark.Optimise.Simplifier
import Futhark.Optimise.Simplifier.Simplifiable
import Futhark.Optimise.DeadVarElim
import Futhark.Optimise.Errors
import Futhark.Representation.AST.Attributes.Ranges

--import qualified Futhark.Optimise.AlgSimplify as AS
--import qualified Futhark.Analysis.ScalExp as ScExp
--import Debug.Trace

simpleOpts :: (Proper lore, Ranged lore) =>
              Simplifiable (SimpleM lore)
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

normCopyOneLambda :: MonadFreshNames m =>
                     Basic.Prog
                  -> Basic.Lambda
                  -> [Maybe Ident]
                  -> m Basic.Lambda
normCopyOneLambda = simplifyLambdaWithRules bindableSimplifiable basicRules
