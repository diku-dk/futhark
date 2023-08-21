{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module provides an efficient value representation as well as
-- parsing and comparison functions.
module Futhark.Test.Values
  ( module Futhark.Data,
    module Futhark.Data.Compare,
    module Futhark.Data.Reader,
    Compound (..),
    CompoundValue,
    mkCompound,
    unCompound,
  )
where

import Data.Map qualified as M
import Data.Text qualified as T
import Data.Traversable
import Futhark.Data
import Futhark.Data.Compare
import Futhark.Data.Reader
import Futhark.Util.Pretty

instance Pretty Value where
  pretty = pretty . valueText

instance Pretty ValueType where
  pretty = pretty . valueTypeText

-- | The structure of a compound value, parameterised over the actual
-- values.  For most cases you probably want 'CompoundValue'.
data Compound v
  = ValueRecord (M.Map T.Text (Compound v))
  | -- | Must not be single value.
    ValueTuple [Compound v]
  | ValueAtom v
  deriving (Eq, Ord, Show)

instance Functor Compound where
  fmap = fmapDefault

instance Foldable Compound where
  foldMap = foldMapDefault

instance Traversable Compound where
  traverse f (ValueAtom v) = ValueAtom <$> f v
  traverse f (ValueTuple vs) = ValueTuple <$> traverse (traverse f) vs
  traverse f (ValueRecord m) = ValueRecord <$> traverse (traverse f) m

instance (Pretty v) => Pretty (Compound v) where
  pretty (ValueAtom v) = pretty v
  pretty (ValueTuple vs) = parens $ commasep $ map pretty vs
  pretty (ValueRecord m) = braces $ commasep $ map field $ M.toList m
    where
      field (k, v) = pretty k <> equals <> pretty v

-- | Create a tuple for a non-unit list, and otherwise a 'ValueAtom'
mkCompound :: [Compound v] -> Compound v
mkCompound [v] = v
mkCompound vs = ValueTuple vs

-- | If the value is a tuple, extract the components, otherwise return
-- a singleton list of the value.
unCompound :: Compound v -> [Compound v]
unCompound (ValueTuple vs) = vs
unCompound v = [v]

-- | Like a 'Value', but also grouped in compound ways that are not
-- supported by raw values.  You cannot parse or read these in
-- standard ways, and they cannot be elements of arrays.
type CompoundValue = Compound Value
