{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module L0.EnablingOpts.EnablingOptDriver ( 
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
 
import L0.AbSyn
import L0.Renamer

import L0.EnablingOpts.InliningDeadFun
import L0.EnablingOpts.CopyCtPropFold
import L0.EnablingOpts.DeadVarElim
import L0.EnablingOpts.TupleNormalizer
import L0.EnablingOpts.LetNormalizer
import L0.EnablingOpts.EnablingOptErrors

import L0.EnablingOpts.ArrTup2TupArr

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

    prog_flat <- arr2tupProg prog_enopt2
    normCopyDeadOpts prog_flat

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

