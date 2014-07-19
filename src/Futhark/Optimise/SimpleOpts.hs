module Futhark.Optimise.SimpleOpts
  ( simpleOpts
  , CallGraph
  , buildCallGraph
  , aggInlineDriver
  , normCopyDeadOpts
  , normCopyOneLambda
  , Error(..)
  )
  where

import Futhark.Representation.Basic
import Futhark.MonadFreshNames

import Futhark.Optimise.InliningDeadFun
import Futhark.Optimise.Simplifier
import Futhark.Optimise.DeadVarElim
import Futhark.Optimise.Errors

--import qualified Futhark.Optimise.AlgSimplify as AS
--import qualified Futhark.Analysis.ScalExp as ScExp
--import Debug.Trace

simpleOpts :: Prog -> Either Error Prog
simpleOpts prog = do
--  esc <- AS.canSimplify 3
--  let prog' = trace (ScExp.ppScalExp esc) prog
  let prog_enopt1   = normCopyDeadOpts prog
      prog_enopt2   = normCopyDeadOpts prog_enopt1
      prog_flat_opt = normCopyDeadOpts prog_enopt2

  return $ normCopyDeadOpts prog_flat_opt

normCopyDeadOpts :: Prog -> Prog
normCopyDeadOpts = deadCodeElim . simplifyProg

normCopyOneLambda :: MonadFreshNames m => Prog -> Lambda ->
                     m Lambda
normCopyOneLambda = simplifyOneLambda
