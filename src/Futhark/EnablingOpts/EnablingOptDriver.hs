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
    prog_enopt1 <- normCopyDeadOpts prog
    prog_enopt2 <- normCopyDeadOpts prog_enopt1
    prog_deadf2 <- deadFunElim      prog_enopt2
    prog_flat_opt <- normCopyDeadOpts prog_deadf2

    normCopyDeadOpts prog_flat_opt

normCopyDeadOpts :: Prog -> Either EnablingOptError Prog
normCopyDeadOpts prog = do
  let prog_cp  =  simplifyProg prog
  let prog_dce = deadCodeElim prog_cp
  return prog_dce

normCopyOneLambda :: MonadFreshNames m => Prog -> Lambda ->
                     m Lambda
normCopyOneLambda = simplifyOneLambda
