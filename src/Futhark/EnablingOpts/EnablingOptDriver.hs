module Futhark.EnablingOpts.EnablingOptDriver
  ( enablingOpts
  , CallGraph
  , buildCallGraph
  , aggInlineDriver
  , deadFunElim
  , normCopyDeadOpts
  , normCopyOneLambda
  , EnablingOptError(..)
  )
  where

import Futhark.InternalRep
import Futhark.MonadFreshNames

import Futhark.EnablingOpts.InliningDeadFun
import Futhark.EnablingOpts.Simplifier
import Futhark.EnablingOpts.DeadVarElim
import Futhark.EnablingOpts.EnablingOptErrors

--------------------------------------------------------------
---- Enabling Optimization Driver
--------------------------------------------------------------

enablingOpts :: Prog -> Either EnablingOptError Prog
enablingOpts prog = do
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
