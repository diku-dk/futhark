{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module L0C.HOTrans.HOTransDriver( 
                                    highOrdTransf
                                )
  where
 
 
--import Data.Either
 
--import L0.AbSyn
--import L0.Renamer

import L0C.L0
import L0C.EnablingOpts.EnablingOptDriver

import L0C.HOTrans.Fusion

--------------------------------------------------------------
---- HigherOrderTransformation Driver
--------------------------------------------------------------

--enablingOpts :: TypeBox tf => Prog tf -> Either EnablingOptError (Prog tf)
highOrdTransf :: Prog -> Either EnablingOptError Prog
highOrdTransf prog = do
    (_, prog') <- fuseProg prog
    return prog'
--    if succc 
--    then normCopyDeadOpts prog' >>= highOrdTransf 
--    else return prog'
--    (succc0, prog') <- normCopyDeadOpts prog  >>= fuseProg

