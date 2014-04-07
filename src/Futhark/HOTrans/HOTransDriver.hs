{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Futhark.HOTrans.HOTransDriver
  (
   highOrdTransf
  )
  where

import Futhark.InternalRep
import Futhark.EnablingOpts.EnablingOptDriver

import Futhark.HOTrans.Fusion

--------------------------------------------------------------
---- HigherOrderTransformation Driver
--------------------------------------------------------------

highOrdTransf :: Prog -> Either EnablingOptError Prog
highOrdTransf prog = do
    (_, prog') <- fuseProg prog
    return prog'
