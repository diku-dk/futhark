{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module L0C.HOTrans.HOTransDriver( 
                                    highOrdTransf
                                )
  where
 
 
--import Data.Either
 
--import L0.AbSyn
--import L0.Renamer

import Language.L0
import L0C.EnablingOpts.EnablingOptDriver

import L0C.HOTrans.Fusion

--------------------------------------------------------------
---- HigherOrderTransformation Driver
--------------------------------------------------------------

--enablingOpts :: TypeBox tf => Prog tf -> Either EnablingOptError (Prog tf)
highOrdTransf :: Prog Type -> Either EnablingOptError (Prog Type)
highOrdTransf prog = do
    
{-
    (succc0, prog') <- normCopyDeadOpts prog  >>= fuseProg
    (succc1, prog1) <- normCopyDeadOpts prog' >>= fuseProg
    (succc2, prog2) <- normCopyDeadOpts prog1 >>= fuseProg
    (succc3, prog3) <- normCopyDeadOpts prog2 >>= fuseProg
    (succc4, prog4) <- normCopyDeadOpts prog3 >>= fuseProg
    return prog4
-}

    (succc, prog') <- fuseProg prog
    if succc 
    then normCopyDeadOpts prog' >>= highOrdTransf 
    else return prog'

