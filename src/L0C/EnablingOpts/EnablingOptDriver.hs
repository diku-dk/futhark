{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
 
 
--import Data.Either
 
import L0C.L0
import L0C.Renamer
import L0C.FreshNames

import L0C.EnablingOpts.InliningDeadFun
import L0C.EnablingOpts.CopyCtPropFold
import L0C.EnablingOpts.DeadVarElim
import L0C.EnablingOpts.TupleNormalizer
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

    let prog_rn   = renameProg       prog_dfe

    prog_ntup     <- tupleNormProg   prog_rn

    prog_enopt1 <- normCopyDeadOpts prog_ntup
    prog_enopt2 <- normCopyDeadOpts prog_enopt1
    prog_deadf2 <- deadFunElim      prog_enopt2
    prog_flat_opt <- normCopyDeadOpts $ renameProg $ TT.transformProg prog_deadf2

    tupleNormProg   prog_flat_opt >>= tupleNormProg >>= normCopyDeadOpts

--    if(succs)
--    then enablingOpts outprog
--    else return       outprog

normCopyDeadOpts :: Prog -> Either EnablingOptError Prog
normCopyDeadOpts prog = do
    (_, prog_nlet) <- letNormProg     prog
    (_,prog_cp)    <- copyCtProp      prog_nlet
    (_, prog_dce)  <- deadCodeElim    prog_cp
    return prog_dce

normCopyOneTupleLambda :: Prog -> VNameSource -> TupleLambda ->
                          Either EnablingOptError (VNameSource, TupleLambda)
normCopyOneTupleLambda prog nmsrc lam = do
    (nmsrc', lam') <- letNormOneTupleLambda    nmsrc lam
    lam''          <- copyCtPropOneTupleLambda prog  lam'
    return (nmsrc', lam'')
