{-# OPTIONS_GHC -fno-warn-orphans #-}

module Futhark.IR.MCTests () where

import Data.String
import Futhark.IR.MC
import Futhark.IR.Parse
import Futhark.IR.SyntaxTests (parseString)

-- There isn't anything to test in this module, but we define some
-- convenience instances.

instance IsString (Stm MC) where
  fromString = parseString "Stm MC" parseStmMC

instance IsString (Body MC) where
  fromString = parseString "Body MC" parseBodyMC

instance IsString (Prog MC) where
  fromString = parseString "Prog MC" parseMC
