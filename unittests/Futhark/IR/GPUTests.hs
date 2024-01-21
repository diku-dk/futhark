{-# OPTIONS_GHC -fno-warn-orphans #-}

module Futhark.IR.GPUTests () where

import Data.String
import Futhark.IR.GPU
import Futhark.IR.Parse
import Futhark.IR.SyntaxTests (parseString)

-- There isn't anything to test in this module, but we define some
-- convenience instances.

instance IsString (Stm GPU) where
  fromString = parseString "Stm GPU" parseStmGPU

instance IsString (Body GPU) where
  fromString = parseString "Body GPU" parseBodyGPU

instance IsString (Prog GPU) where
  fromString = parseString "Prog GPU" parseGPU
