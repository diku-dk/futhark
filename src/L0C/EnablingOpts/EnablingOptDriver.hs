{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
module L0C.EnablingOpts.EnablingOptDriver
  (
   enablingOpts
  , copyCtProp
  , CallGraph
  , buildCG
  , aggInlineDriver
  , deadFunElim
  , normCopyDeadOpts
  , normCopyOneLambda
  , EnablingOptError(..)
  )
  where

import L0C.InternalRep
import L0C.MonadFreshNames
import qualified L0C.IndexInliner as II

import L0C.EnablingOpts.InliningDeadFun
import L0C.EnablingOpts.CopyCtPropFold
import L0C.EnablingOpts.DeadVarElim
import L0C.EnablingOpts.EnablingOptErrors

--------------------------------------------------------------
---- Enabling Optimization Driver
--------------------------------------------------------------

enablingOpts :: Prog -> Either EnablingOptError Prog
enablingOpts prog = do

    prog_inl    <- aggInlineDriver $ mkUnnamedLamPrg prog

    prog_dfe    <- deadFunElim     prog_inl

    prog_enopt1 <- normCopyDeadOpts prog_dfe
    prog_enopt2 <- normCopyDeadOpts prog_enopt1
    prog_deadf2 <- deadFunElim      prog_enopt2
    prog_flat_opt <- normCopyDeadOpts prog_deadf2

    normCopyDeadOpts prog_flat_opt

normCopyDeadOpts :: Prog -> Either EnablingOptError Prog
normCopyDeadOpts prog = do
    (_,prog_cp)    <- copyCtProp      prog
    (_, prog_dce)  <- deadCodeElim    prog_cp
    return prog_dce

normCopyOneLambda :: MonadFreshNames m => Prog -> Lambda ->
                     m (Either EnablingOptError Lambda)
normCopyOneLambda prog lam = do
  nmsrc <- getNameSource
  let res = do lam' <- copyCtPropOneLambda prog  lam
               let (lam'', nmsrc') = II.transformLambda nmsrc lam'
               return (lam'', nmsrc')
  case res of Left e -> return $ Left e
              Right (normLam, nmsrc') -> do
                putNameSource nmsrc'
                return $ Right normLam
