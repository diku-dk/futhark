module Futhark.Optimise.Fusion.Screma
  ( 
  )
where

import Data.List (mapAccumL)
import Data.Map.Strict qualified as M
import Data.Maybe
import Futhark.Analysis.HORep.SOAC qualified as SOAC
import Futhark.Builder (Buildable (..), insertStm, insertStms, mkLet)
import Futhark.Construct (mapResult)
import Futhark.IR
import Futhark.Util (dropLast, splitAt3, takeLast)
