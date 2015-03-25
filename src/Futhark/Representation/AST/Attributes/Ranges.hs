-- | Utility declarations for performing range analysis.
module Futhark.Representation.AST.Attributes.Ranges
       ( Range
       , Ranged (..)
       )
       where

import Futhark.Representation.AST.Syntax
import Futhark.Representation.AST.Lore (Lore)
import Futhark.Analysis.ScalExp

-- | Upper and lower bound, both inclusive.
type Range = (Maybe ScalExp, Maybe ScalExp)

-- | The lore has embedded range information.  Note that it may not be
-- up to date, unless whatever maintains the syntax tree is careful.
class Lore lore => Ranged lore where
  -- | The range of the value parts of the 'Body'.  The default method
  -- returns empty/unknown ranges.
  bodyRanges :: Body lore -> [Range]
  bodyRanges body =
    replicate (length $ resultSubExps $ bodyResult body)
    (Nothing, Nothing)
