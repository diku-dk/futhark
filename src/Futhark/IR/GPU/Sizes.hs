{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}

-- | In the context of this module, a "size" is any kind of tunable
-- (run-time) constant.
module Futhark.IR.GPU.Sizes
  ( SizeClass (..),
    sizeDefault,
    KernelPath,
    Count (..),
    NumGroups,
    GroupSize,
    NumThreads,
  )
where

import Data.Int (Int64)
import Data.Traversable
import Futhark.IR.Prop.Names (FreeIn)
import Futhark.Transform.Substitute
import Futhark.Util.IntegralExp (IntegralExp)
import Futhark.Util.Pretty
import Language.Futhark.Core (Name)
import Prelude hiding (id, (.))

-- | An indication of which comparisons have been performed to get to
-- this point, as well as the result of each comparison.
type KernelPath = [(Name, Bool)]

-- | The class of some kind of configurable size.  Each class may
-- impose constraints on the valid values.
data SizeClass
  = -- | A threshold with an optional default.
    SizeThreshold KernelPath (Maybe Int64)
  | SizeGroup
  | SizeNumGroups
  | SizeTile
  | SizeRegTile
  | -- | Likely not useful on its own, but querying the
    -- maximum can be handy.
    SizeLocalMemory
  | -- | A bespoke size with a default.
    SizeBespoke Name Int64
  deriving (Eq, Ord, Show)

instance Pretty SizeClass where
  ppr (SizeThreshold path def) =
    "threshold" <> parens (def' <> comma <+> spread (map pStep path))
    where
      pStep (v, True) = ppr v
      pStep (v, False) = "!" <> ppr v
      def' = maybe "def" ppr def
  ppr SizeGroup = text "group_size"
  ppr SizeNumGroups = text "num_groups"
  ppr SizeTile = text "tile_size"
  ppr SizeRegTile = text "reg_tile_size"
  ppr SizeLocalMemory = text "local_memory"
  ppr (SizeBespoke k def) =
    text "bespoke" <> parens (ppr k <> comma <+> ppr def)

-- | The default value for the size.  If 'Nothing', that means the backend gets to decide.
sizeDefault :: SizeClass -> Maybe Int64
sizeDefault (SizeThreshold _ x) = x
sizeDefault (SizeBespoke _ x) = Just x
sizeDefault _ = Nothing

-- | A wrapper supporting a phantom type for indicating what we are counting.
newtype Count u e = Count {unCount :: e}
  deriving (Eq, Ord, Show, Num, IntegralExp, FreeIn, Pretty, Substitute)

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
