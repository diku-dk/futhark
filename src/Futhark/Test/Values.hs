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

    -- * Random value generation
    Range,
    RandomConfiguration (..),
    initialRandomConfiguration,
    randomValue,
  )
where

import Control.Monad.ST
import Data.Int
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Traversable
import Data.Vector.Storable qualified as SVec
import Data.Vector.Storable.Mutable qualified as USVec
import Data.Word
import Futhark.Data
import Futhark.Data.Compare
import Futhark.Data.Reader
import Futhark.Util (convFloat)
import Futhark.Util.Pretty (Pretty (..), braces, commasep, equals, parens)
import Numeric.Half
import System.Random.Stateful (UniformRange (..), mkStdGen, uniformR)

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

randomVector ::
  (SVec.Storable v, UniformRange v) =>
  Range v ->
  (SVec.Vector Int -> SVec.Vector v -> Value) ->
  [Int] ->
  Word64 ->
  Value
randomVector range final ds seed = runST $ do
  -- Use some nice impure computation where we can preallocate a
  -- vector of the desired size, populate it via the random number
  -- generator, and then finally reutrn a frozen binary vector.
  arr <- USVec.new n
  let fill g i
        | i < n = do
            let (v, g') = uniformR range g
            USVec.write arr i v
            g' `seq` fill g' $! i + 1
        | otherwise =
            pure ()
  fill (mkStdGen $ fromIntegral seed) 0
  final (SVec.fromList ds) . SVec.convert <$> SVec.freeze arr
  where
    n = product ds

-- XXX: The following instance is an orphan.  Maybe it could be
-- avoided with some newtype trickery or refactoring, but it's so
-- convenient this way.
instance UniformRange Half where
  uniformRM (a, b) g =
    (convFloat :: Float -> Half) <$> uniformRM (convFloat a, convFloat b) g

-- | Closed interval, as in @System.Random@.
type Range a = (a, a)

data RandomConfiguration = RandomConfiguration
  { i8Range :: Range Int8,
    i16Range :: Range Int16,
    i32Range :: Range Int32,
    i64Range :: Range Int64,
    u8Range :: Range Word8,
    u16Range :: Range Word16,
    u32Range :: Range Word32,
    u64Range :: Range Word64,
    f16Range :: Range Half,
    f32Range :: Range Float,
    f64Range :: Range Double
  }

initialRandomConfiguration :: RandomConfiguration
initialRandomConfiguration =
  RandomConfiguration
    (minBound, maxBound)
    (minBound, maxBound)
    (minBound, maxBound)
    (minBound, maxBound)
    (minBound, maxBound)
    (minBound, maxBound)
    (minBound, maxBound)
    (minBound, maxBound)
    (0.0, 1.0)
    (0.0, 1.0)
    (0.0, 1.0)

randomValue :: RandomConfiguration -> ValueType -> Word64 -> Value
randomValue conf (ValueType ds t) seed =
  case t of
    I8 -> gen i8Range I8Value
    I16 -> gen i16Range I16Value
    I32 -> gen i32Range I32Value
    I64 -> gen i64Range I64Value
    U8 -> gen u8Range U8Value
    U16 -> gen u16Range U16Value
    U32 -> gen u32Range U32Value
    U64 -> gen u64Range U64Value
    F16 -> gen f16Range F16Value
    F32 -> gen f32Range F32Value
    F64 -> gen f64Range F64Value
    Bool -> gen (const (False, True)) BoolValue
  where
    gen range final = randomVector (range conf) final ds seed
