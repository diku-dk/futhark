{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables #-}

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
import L0.EnablingOpts.EnablingOptErrors


--------------------------------------------------------------
---- Enabling Optimization Driver
--------------------------------------------------------------

--enablingOpts :: TypeBox tf => Prog tf -> Either EnablingOptError (Prog tf)
enablingOpts :: Prog Type -> Either EnablingOptError (Prog Type)
enablingOpts prog = do

    proginline <- aggInlineDriver   prog

    progdfelim <- deadFunElim       proginline

    let progrename = renameProg     progdfelim

    (succCP , progcp ) <- copyCtProp   progrename

    (succDCE, progdce) <- deadCodeElim progcp

    let (succs, outprog) = (succCP || succDCE, progdce)

    if(succs)
    then enablingOpts outprog
    else return       outprog

