{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Futhark.Representation.Kernels.Sizes
  ( SizeClass (..)
  , KernelPath
  , Count(..)
  , NumGroups, GroupSize, NumThreads
  )
  where

import Data.Int (Int32)
import Data.Traversable

import Futhark.Util.Pretty
import Language.Futhark.Core (Name)
import Futhark.Util.IntegralExp (IntegralExp)
import Futhark.Representation.AST.Attributes.Names (FreeIn)

-- | An indication of which comparisons have been performed to get to
-- this point, as well as the result of each comparison.
type KernelPath = [(Name, Bool)]

-- | The class of some kind of configurable size.  Each class may
-- impose constraints on the valid values.
data SizeClass = SizeThreshold KernelPath
               | SizeGroup
               | SizeNumGroups
               | SizeTile
               | SizeLocalMemory
               -- ^ Likely not useful on its own, but querying the
               -- maximum can be handy.
               | SizeBespoke Name Int32
               -- ^ A bespoke size with a default.
               deriving (Eq, Ord, Show)

instance Pretty SizeClass where
  ppr (SizeThreshold path) = text $ "threshold (" ++ unwords (map pStep path) ++ ")"
    where pStep (v, True) = pretty v
          pStep (v, False) = '!' : pretty v
  ppr SizeGroup = text "group_size"
  ppr SizeNumGroups = text "num_groups"
  ppr SizeTile = text "tile_size"
  ppr SizeLocalMemory = text "local_memory"
  ppr (SizeBespoke k _) = ppr k

-- | A wrapper supporting a phantom type for indicating what we are counting.
newtype Count u e = Count { unCount :: e }
                deriving (Eq, Ord, Show, Num, IntegralExp, FreeIn, Pretty)

instance Functor (Count u) where
  fmap = fmapDefault

instance Foldable (Count u) where
  foldMap = foldMapDefault

instance Traversable (Count u) where
  traverse f (Count x) = Count <$> f x

-- | Phantom type for the number of groups of some kernel.
data NumGroups

-- | Phantom type for the group size of some kernel.
data GroupSize

-- | Phantom type for number of threads.
data NumThreads
