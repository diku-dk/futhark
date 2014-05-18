module Futhark.Optimise.SimpleOpts
  ( simpleOpts
  , CallGraph
  , buildCallGraph
  , aggInlineDriver
  , deadFunElim
  , normCopyDeadOpts
  , normCopyOneLambda
  , Error(..)
  )
  where

import Futhark.InternalRep
import Futhark.MonadFreshNames

import Futhark.Optimise.InliningDeadFun
import Futhark.Optimise.Simplifier
import Futhark.Optimise.DeadVarElim
import Futhark.Optimise.Errors

simpleOpts :: Prog -> Either Error Prog
simpleOpts prog = do
  let prog_enopt1 = normCopyDeadOpts prog
  let prog_enopt2 = normCopyDeadOpts prog_enopt1
  prog_deadf2 <- deadFunElim      prog_enopt2
  let prog_flat_opt = normCopyDeadOpts prog_deadf2

  return $ normCopyDeadOpts prog_flat_opt

normCopyDeadOpts :: Prog -> Prog
normCopyDeadOpts = deadCodeElim . simplifyProg

normCopyOneLambda :: MonadFreshNames m => Prog -> Lambda ->
                     m Lambda
normCopyOneLambda = simplifyOneLambda
