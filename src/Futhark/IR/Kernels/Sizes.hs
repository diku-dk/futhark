{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}

-- | In the context of this module, a "size" is any kind of tunable
-- (run-time) constant.
module Futhark.IR.Kernels.Sizes
  ( SizeClass (..),
    sizeDefault,
    KernelPath,
    Count (..),
    NumGroups,
    GroupSize,
    NumThreads,
  )
where

import Control.Category
import Data.Int (Int64)
import Data.Traversable
import Futhark.IR.Prop.Names (FreeIn)
import Futhark.Transform.Substitute
import Futhark.Util.IntegralExp (IntegralExp)
import Futhark.Util.Pretty
import GHC.Generics (Generic)
import Language.Futhark.Core (Name)
import Language.SexpGrammar as Sexp
import Language.SexpGrammar.Generic
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
  | -- | Likely not useful on its own, but querying the
    -- maximum can be handy.
    SizeLocalMemory
  | -- | A bespoke size with a default.
    SizeBespoke Name Int64
  deriving (Eq, Ord, Show, Generic)

instance SexpIso SizeClass where
  sexpIso =
    match $
      With (. Sexp.list (Sexp.el (Sexp.sym "threshold") >>> Sexp.el sexpIso >>> props (Sexp.optKey "default" (iso fromIntegral fromIntegral . Sexp.int)))) $
        With (. Sexp.sym "group") $
          With (. Sexp.sym "num-groups") $
            With (. Sexp.sym "tile") $
              With (. Sexp.sym "local-memory") $
                With
                  (. Sexp.list (Sexp.el (Sexp.sym "bespoke") >>> Sexp.el sexpIso >>> Sexp.el (iso fromIntegral fromIntegral . Sexp.int)))
                  End

instance Pretty SizeClass where
  ppr (SizeThreshold path _) = text $ "threshold (" ++ unwords (map pStep path) ++ ")"
    where
      pStep (v, True) = pretty v
      pStep (v, False) = '!' : pretty v
  ppr SizeGroup = text "group_size"
  ppr SizeNumGroups = text "num_groups"
  ppr SizeTile = text "tile_size"
  ppr SizeLocalMemory = text "local_memory"
  ppr (SizeBespoke k _) = ppr k

-- | The default value for the size.  If 'Nothing', that means the backend gets to decide.
sizeDefault :: SizeClass -> Maybe Int64
sizeDefault (SizeThreshold _ x) = x
sizeDefault (SizeBespoke _ x) = Just x
sizeDefault _ = Nothing

-- | A wrapper supporting a phantom type for indicating what we are counting.
newtype Count u e = Count {unCount :: e}
  deriving (Eq, Ord, Show, Num, IntegralExp, FreeIn, Pretty, Substitute, Generic)

instance SexpIso e => SexpIso (Count u e) where
  sexpIso = with $ \count -> sexpIso >>> count

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
