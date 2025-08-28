module Futhark.IR.SOACSTests () where

import Data.String
import Futhark.IR.Parse
import Futhark.IR.SOACS
import Futhark.IR.SyntaxTests (parseString)

instance IsString (Lambda SOACS) where
  fromString = parseString "Lambda" parseLambdaSOACS

instance IsString (Body SOACS) where
  fromString = parseString "Body" parseBodySOACS

instance IsString (Stm SOACS) where
  fromString = parseString "Stm" parseStmSOACS
