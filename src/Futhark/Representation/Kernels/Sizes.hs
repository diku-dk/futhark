module Futhark.Representation.Kernels.Sizes
  ( SizeClass (..) )
  where

import Futhark.Util.Pretty

-- | The class of some kind of configurable size.  Each class may
-- impose constraints on the valid values.
data SizeClass = SizeThreshold
               | SizeGroup
               | SizeNumGroups
               | SizeTile
               deriving (Eq, Ord, Show)

instance Pretty SizeClass where
  ppr SizeThreshold = text "threshold"
  ppr SizeGroup = text "group_size"
  ppr SizeNumGroups = text "num_groups"
  ppr SizeTile = text "tile_size"
