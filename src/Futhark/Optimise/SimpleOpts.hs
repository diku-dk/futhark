module Futhark.Optimise.SimpleOpts
  ( simpleOpts
  , CallGraph
  , buildCallGraph
  , aggInlineDriver
  , normCopyDeadOpts
  , normCopyOneLambda
  , Error(..)
    -- * Re-exports
  , bindableSimplifiable
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

--import qualified Futhark.Optimise.AlgSimplify as AS
--import qualified Futhark.Analysis.ScalExp as ScExp
--import Debug.Trace

simpleOpts :: Proper lore =>
              Simplifiable (SimpleM lore)
              -> Prog lore -> Either Error (Prog lore)
simpleOpts simpl prog = do
--  esc <- AS.canSimplify 3
--  let prog' = trace (ScExp.ppScalExp esc) prog
  let prog_enopt1   = normCopyDeadOpts simpl prog
      prog_enopt2   = normCopyDeadOpts simpl prog_enopt1
      prog_flat_opt = normCopyDeadOpts simpl prog_enopt2

  return $ normCopyDeadOpts simpl prog_flat_opt

normCopyDeadOpts :: Proper lore =>
                    Simplifiable (SimpleM lore)
                 -> Prog lore -> Prog lore
normCopyDeadOpts simpl = deadCodeElim . simplifyProgWithStandardRules simpl

normCopyOneLambda :: MonadFreshNames m =>
                     Basic.Prog
                  -> Basic.Lambda
                  -> [Maybe SubExp]
                  -> m Basic.Lambda
normCopyOneLambda = simplifyLambdaWithStandardRules bindableSimplifiable
