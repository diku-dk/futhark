{-# LANGUAGE Strict #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module provides an efficient value representation as well as
-- parsing and comparison functions.
module Futhark.Test.Values
  ( module Futhark.Data,
    module Futhark.Data.Compare,
    module Futhark.Data.Reader,

    -- * Random value generation
    Range,
    RandomConfiguration (..),
    initialRandomConfiguration,
    randomValue,
  )
where

import Control.Monad.ST
import Data.Int
import Data.Vector.Storable qualified as SVec
import Data.Vector.Storable.Mutable qualified as USVec
import Data.Word
import Futhark.Data
import Futhark.Data.Compare
import Futhark.Data.Reader
import Futhark.Util (convFloat)
import Futhark.Util.Pretty (Pretty (..))
import Numeric.Half
import System.Random.Stateful (UniformRange (..), mkStdGen, uniformR)

instance Pretty Value where
  pretty = pretty . valueText

instance Pretty ValueType where
  pretty = pretty . valueTypeText

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
