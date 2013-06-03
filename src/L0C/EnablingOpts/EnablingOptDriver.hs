{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module L0C.EnablingOpts.EnablingOptDriver (
                                    enablingOpts
                                  , copyCtProp
                                  , CallGraph
                                  , buildCG
                                  , aggInlineDriver
                                  , deadFunElim
                                  , EnablingOptError(..)
                            )
  where
 
 
--import Data.Either
 
import Language.L0
import L0C.Renamer

import L0C.EnablingOpts.InliningDeadFun
import L0C.EnablingOpts.CopyCtPropFold
import L0C.EnablingOpts.DeadVarElim
import L0C.EnablingOpts.TupleNormalizer
import L0C.EnablingOpts.LetNormalizer
import L0C.EnablingOpts.EnablingOptErrors

import qualified L0C.TupleTransform as TT

import Debug.Trace

--------------------------------------------------------------
---- Enabling Optimization Driver
--------------------------------------------------------------

--enablingOpts :: TypeBox tf => Prog tf -> Either EnablingOptError (Prog tf)
enablingOpts :: Prog Type -> Either EnablingOptError (Prog Type)
enablingOpts prog = do

    prog_inl      <- aggInlineDriver prog

    prog_dfe      <- deadFunElim     prog_inl

    let prog_rn   = renameProg       prog_dfe

    prog_ntup     <- tupleNormProg   prog_rn

    prog_enopt1 <- normCopyDeadOpts prog_ntup
    prog_enopt2 <- normCopyDeadOpts prog_enopt1
    prog_deadf2 <- deadFunElim     prog_enopt2
    normCopyDeadOpts $ TT.transformProg prog_deadf2

--    if(succs)
--    then enablingOpts outprog
--    else return       outprog

normCopyDeadOpts :: Prog Type -> Either EnablingOptError (Prog Type)
normCopyDeadOpts prog = do
    (_, prog_nlet) <- letNormProg     prog
    (_,prog_cp)    <- copyCtProp      prog_nlet
    --(_, prog_dce)  <- trace (prettyPrint prog_cp1) (deadCodeElim prog_cp1)
    (_, prog_dce)  <- deadCodeElim    prog_cp

    return prog_dce

