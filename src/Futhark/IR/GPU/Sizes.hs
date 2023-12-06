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
  | -- | Amount of registers available per workgroup. Mostly
    -- meaningful for querying the maximum.
    SizeRegisters
  | -- | Amount of L2 cache memory, in bytes. Mostly meaningful for
    -- querying the maximum.
    SizeCache
  deriving (Eq, Ord, Show)

instance Pretty SizeClass where
  pretty (SizeThreshold path def) =
    "threshold" <> parens (def' <> comma <+> hsep (map pStep path))
    where
      pStep (v, True) = pretty v
      pStep (v, False) = "!" <> pretty v
      def' = maybe "def" pretty def
  pretty SizeGroup = "group_size"
  pretty SizeNumGroups = "num_groups"
  pretty SizeTile = "tile_size"
  pretty SizeRegTile = "reg_tile_size"
  pretty SizeLocalMemory = "local_memory"
  pretty (SizeBespoke k def) =
    "bespoke" <> parens (pretty k <> comma <+> pretty def)
  pretty SizeRegisters = "registers"
  pretty SizeCache = "cache"

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
