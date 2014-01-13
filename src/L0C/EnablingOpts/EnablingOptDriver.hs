{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
module L0C.EnablingOpts.EnablingOptDriver (
                                    enablingOpts
                                  , copyCtProp
                                  , CallGraph
                                  , buildCG
                                  , aggInlineDriver
                                  , deadFunElim
                                  , normCopyDeadOpts
                                  , normCopyOneTupleLambda
                                  , EnablingOptError(..)
                            )
  where

import L0C.L0
import L0C.Renamer
import L0C.MonadFreshNames
import qualified L0C.IndexInliner as II

import L0C.EnablingOpts.InliningDeadFun
import L0C.EnablingOpts.CopyCtPropFold
import L0C.EnablingOpts.DeadVarElim
import L0C.EnablingOpts.LetNormalizer
import L0C.EnablingOpts.EnablingOptErrors

import qualified L0C.TupleTransform as TT

--------------------------------------------------------------
---- Enabling Optimization Driver
--------------------------------------------------------------

enablingOpts :: Prog -> Either EnablingOptError Prog
enablingOpts prog = do

    prog_inl      <- aggInlineDriver $ mkUnnamedLamPrg prog

    prog_dfe      <- deadFunElim     prog_inl

    let prog_uniq = renameProg prog_dfe

    prog_enopt1 <- normCopyDeadOpts prog_uniq
    prog_enopt2 <- normCopyDeadOpts prog_enopt1
    prog_deadf2 <- deadFunElim      prog_enopt2
    prog_flat_opt <- normCopyDeadOpts prog_deadf2

    normCopyDeadOpts prog_flat_opt

--    if(succs)
--    then enablingOpts outprog
--    else return       outprog

normCopyDeadOpts :: Prog -> Either EnablingOptError Prog
normCopyDeadOpts prog = do
    (_, prog_nlet) <- letNormProg     prog
    (_,prog_cp)    <- copyCtProp      $ TT.transformProg prog_nlet
    (_, prog_dce)  <- deadCodeElim    prog_cp
    return prog_dce

normCopyOneTupleLambda :: MonadFreshNames VName m => Prog -> TupleLambda ->
                          m (Either EnablingOptError TupleLambda)
normCopyOneTupleLambda prog lam = do
  nmsrc <- getNameSource
  let res = do (lam', nmsrc') <- letNormOneTupleLambda    nmsrc lam
               lam''          <- copyCtPropOneTupleLambda prog  lam'
               let (lam''', nmsrc'') = II.transformLambda nmsrc' lam''
               return (lam''', nmsrc'')
  case res of Left e -> return $ Left e
              Right (normLam, nmsrc') -> do
                putNameSource nmsrc'
                return $ Right normLam
