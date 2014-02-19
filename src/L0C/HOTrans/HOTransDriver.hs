{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module L0C.HOTrans.HOTransDriver
  (
   highOrdTransf
  )
  where

import L0C.InternalRep
import L0C.EnablingOpts.EnablingOptDriver

import L0C.HOTrans.Fusion

--------------------------------------------------------------
---- HigherOrderTransformation Driver
--------------------------------------------------------------

highOrdTransf :: Prog -> Either EnablingOptError Prog
highOrdTransf prog = do
    (_, prog') <- fuseProg prog
    return prog'
