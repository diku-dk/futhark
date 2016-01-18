{-# LANGUAGE RankNTypes #-}
-- | Queries and operations on values.  Useful for the interpreter and
-- constant folding.
module Futhark.Representation.AST.Attributes.Values
       (
         valueType
       , valueShape
       , valueSize
       , IsValue (..)
       , intconst

         -- * Extracting
       , valueInt

         -- * Rearranging
       , permuteArray
       , stripeArray
       , unstripeArray

         -- * Operations
       , numBinOp
       , intBinOp
       , floatBinOp
       , ordBinOp
       , numUnOp
       , intUnOp

         -- * Conversion
       , zeroExtend
       , signExtend
       , intToFloat
       , uintToFloat

         -- * Miscellaneous
       , arrayString
       , zeroIsh
       , oneIsh
       )
       where

import Control.Applicative
import Data.Array
import Data.Bits
import Data.List
import Data.Word

import Prelude

import Futhark.Representation.AST.Syntax
import Futhark.Representation.AST.Attributes.Constants
import Futhark.Representation.AST.Attributes.Rearrange
import Futhark.Representation.AST.Attributes.Stripe

-- | Return the type of the given value.
valueType :: Value -> Type
valueType (PrimVal v) =
  Prim $ primValueType v
valueType (ArrayVal _ et shape) =
  Array et (Shape $ map constant shape) NoUniqueness

-- | Return the size of the first dimension of an array, or zero for
-- non-arrays.
valueSize :: Value -> Int
valueSize t = case valueShape t of
                []  -> 0
                n:_ -> n

-- | Return a list of the sizes of an array (the shape, in other
-- terms).  For non-arrays, this is the empty list.  A two-dimensional
-- array with five rows and three columns would return the list @[5,
-- 3]@.  If an array has @n@ dimensions, the result is always a list
-- of @n@ elements.
valueShape :: Value -> [Int]
valueShape (ArrayVal _ _ shape) = shape
valueShape _ = []

-- | Convert an 'IntValue' to an 'Integer'.
valueInt :: IntValue -> Integer
valueInt (Int8Value x) = toInteger x
valueInt (Int16Value x) = toInteger x
valueInt (Int32Value x) = toInteger x
valueInt (Int64Value x) = toInteger x

-- | Permute the dimensions of an array value.  If the given value is
-- not an array, it is returned unchanged.  The length of the
-- permutation must be equal to the rank of the value.
permuteArray :: [Int] -> Value -> Value
permuteArray perm (ArrayVal inarr et oldshape) =
  let newshape = move oldshape
      idx is shape = sum (zipWith (*) is (map product $ drop 1 (tails shape)))
  in ArrayVal (listArray (bounds inarr)
               [ inarr ! idx (invmove is) oldshape
               | is <- map reverse $ picks $ reverse newshape ])
     et newshape
  where move = rearrangeShape perm
        invmove = rearrangeShape $ rearrangeInverse perm
        picks [] = []
        picks [n] = map (:[]) [0..n-1]
        picks (n:ns) = [ i:is | is <- picks ns, i <- [0..n-1] ]
permuteArray _ v = v

-- | Stripe the elements of an array value.  If the given value is
-- not an array, it is returned unchanged.
stripeArray :: Int -> Value -> Value
stripeArray stride (ArrayVal inarr et shape) =
  ArrayVal (listArray (0, upper_bound)
            [ inarr ! i | i <- stripeIndices n stride ])
  et shape
  where upper_bound = snd $ bounds inarr
        n = upper_bound + 1
stripeArray _ v = v

-- | Inversely stripe the elements of an array value.  If the given value is
-- not an array, it is returned unchanged.
unstripeArray :: Int -> Value -> Value
unstripeArray stride (ArrayVal inarr et shape) =
  ArrayVal (listArray (0, upper_bound)
            [ inarr ! i | i <- stripeIndicesInverse n stride ])
  et shape
  where upper_bound = snd $ bounds inarr
        n = upper_bound + 1
unstripeArray _ v = v

-- | If the given value is a nonempty array containing only
-- characters, return the corresponding 'String', otherwise return
-- 'Nothing'.
arrayString :: Value -> Maybe String
arrayString (ArrayVal arr _ _)
  | c:cs <- elems arr = mapM asChar $ c:cs
  where asChar (CharValue c) = Just c
        asChar _           = Nothing
arrayString _ = Nothing


-- | If both values are of the same type, apply the operator, otherwise 'Fail'.
ordBinOp :: (Functor m, Monad m) =>
            (forall a. Ord a => a -> a -> m Bool)
         -> PrimValue -> PrimValue -> m PrimValue
ordBinOp op (IntValue (Int8Value x)) (IntValue (Int8Value y)) =
  BoolValue <$> x `op` y
ordBinOp op (IntValue (Int16Value x)) (IntValue (Int16Value y)) =
  BoolValue <$> x `op` y
ordBinOp op (IntValue (Int32Value x)) (IntValue (Int32Value y)) =
  BoolValue <$> x `op` y
ordBinOp op (IntValue (Int64Value x)) (IntValue (Int64Value y)) =
  BoolValue <$> x `op` y
ordBinOp op (CharValue x) (CharValue y) =
  BoolValue <$> x `op` y
ordBinOp op (FloatValue (Float32Value x)) (FloatValue (Float32Value y)) =
  BoolValue <$> x `op` y
ordBinOp op (FloatValue (Float64Value x)) (FloatValue (Float64Value y)) =
  BoolValue <$> x `op` y
ordBinOp op (BoolValue x) (BoolValue y) =
  BoolValue <$> x `op` y
ordBinOp _ _ _ =
  fail "ordBinOp: operands not of appropriate type."

-- | If both values are of the same numeric type, apply the operator,
-- otherwise 'Fail'.
numBinOp :: (Functor m, Monad m) =>
            (forall num. (Eq num, Num num) => num -> num -> m num)
         -> PrimValue -> PrimValue -> m PrimValue
numBinOp op (IntValue (Int8Value x)) (IntValue (Int8Value y)) =
  IntValue <$> Int8Value <$> x `op` y
numBinOp op (IntValue (Int16Value x)) (IntValue (Int16Value y)) =
  IntValue <$> Int16Value <$> x `op` y
numBinOp op (IntValue (Int32Value x)) (IntValue (Int32Value y)) =
  IntValue <$> Int32Value <$> x `op` y
numBinOp op (IntValue (Int64Value x)) (IntValue (Int64Value y)) =
  IntValue <$> Int64Value <$> x `op` y
numBinOp op (FloatValue (Float32Value x)) (FloatValue (Float32Value y)) =
  FloatValue <$> Float32Value <$> x `op` y
numBinOp op (FloatValue (Float64Value x)) (FloatValue (Float64Value y)) =
  FloatValue <$> Float64Value <$> x `op` y
numBinOp _ _ _ =
  fail "numBinOp: operands not of appropriate type."

-- | If both values are of the same integer type, apply the operator,
-- otherwise 'Fail'.
intBinOp :: (Functor m, Monad m) =>
            (forall int. (Eq int, Integral int, Bits int) => int -> int -> m int)
         -> PrimValue -> PrimValue -> m PrimValue
intBinOp op (IntValue (Int8Value x)) (IntValue (Int8Value y)) =
  IntValue <$> Int8Value <$> x `op` y
intBinOp op (IntValue (Int16Value x)) (IntValue (Int16Value y)) =
  IntValue <$> Int16Value <$> x `op` y
intBinOp op (IntValue (Int32Value x)) (IntValue (Int32Value y)) =
  IntValue <$> Int32Value <$> x `op` y
intBinOp op (IntValue (Int64Value x)) (IntValue (Int64Value y)) =
  IntValue <$> Int64Value <$> x `op` y
intBinOp _ _ _ =
  fail "intBinOp: operands not of appropriate type."

-- | If both values are of the same floating-point type, apply the operator,
-- otherwise 'Fail'.
floatBinOp :: (Functor m, Monad m) =>
            (forall float. (Eq float, Floating float) => float -> float -> m float)
         -> PrimValue -> PrimValue -> m PrimValue
floatBinOp op (FloatValue (Float32Value x)) (FloatValue (Float32Value y)) =
  FloatValue <$> Float32Value <$> x `op` y
floatBinOp op (FloatValue (Float64Value x)) (FloatValue (Float64Value y)) =
  FloatValue <$> Float64Value <$> x `op` y
floatBinOp _ _ _ =
  fail "floatBinOp: operands not of appropriate type."

-- | If the value is of a numeric type, apply the operator,
-- otherwise 'Fail'.
numUnOp :: (Functor m, Monad m) =>
            (forall num. Num num => num -> m num)
         -> PrimValue -> m PrimValue
numUnOp op (IntValue (Int8Value x)) =
  IntValue <$> Int8Value <$> op x
numUnOp op (IntValue (Int16Value x)) =
  IntValue <$> Int16Value <$> op x
numUnOp op (IntValue (Int32Value x)) =
  IntValue <$> Int32Value <$> op x
numUnOp op (IntValue (Int64Value x)) =
  IntValue <$> Int64Value <$> op x
numUnOp op (FloatValue (Float32Value x)) =
  FloatValue <$> Float32Value <$> op x
numUnOp op (FloatValue (Float64Value x)) =
  FloatValue <$> Float64Value <$> op x
numUnOp _ _ =
  fail "numUnOp: operands not of appropriate type."

-- | If the value is of an integer type, apply the operator,
-- otherwise 'Fail'.
intUnOp :: (Functor m, Monad m) =>
           (forall int. (Integral int, Bits int) => int -> m int)
           -> PrimValue -> m PrimValue
intUnOp op (IntValue (Int8Value x)) =
  IntValue <$> Int8Value <$> op x
intUnOp op (IntValue (Int16Value x)) =
  IntValue <$> Int16Value <$> op x
intUnOp op (IntValue (Int32Value x)) =
  IntValue <$> Int32Value <$> op x
intUnOp op (IntValue (Int64Value x)) =
  IntValue <$> Int64Value <$> op x
intUnOp _ _ =
  fail "intUnOp: operands not of appropriate type."

-- | Zero-extend the given integer value to the size of the given
-- type.  If the type is smaller than the given value, the result is a
-- truncation.
zeroExtend :: IntValue -> IntType -> IntValue
zeroExtend (Int8Value x) t = intvalue' t $ toInteger (fromIntegral x :: Word8)
zeroExtend (Int16Value x) t = intvalue' t $ toInteger (fromIntegral x :: Word16)
zeroExtend (Int32Value x) t = intvalue' t $ toInteger (fromIntegral x :: Word32)
zeroExtend (Int64Value x) t = intvalue' t $ toInteger (fromIntegral x :: Word64)

-- | Sign-extend the given integer value to the size of the given
-- type.  If the type is smaller than the given value, the result is a
-- truncation.
signExtend :: IntValue -> IntType -> IntValue
signExtend (Int8Value x) t = intvalue' t $ toInteger x
signExtend (Int16Value x) t = intvalue' t $ toInteger x
signExtend (Int32Value x) t = intvalue' t $ toInteger x
signExtend (Int64Value x) t = intvalue' t $ toInteger x

intToFloat :: IntValue -> FloatType -> FloatValue
intToFloat (Int8Value x) t = floatvalue t $ toRational x
intToFloat (Int16Value x) t = floatvalue t $ toRational x
intToFloat (Int32Value x) t = floatvalue t $ toRational x
intToFloat (Int64Value x) t = floatvalue t $ toRational x

uintToFloat :: IntValue -> FloatType -> FloatValue
uintToFloat (Int8Value x) t = floatvalue t $ toRational (fromIntegral x :: Word8)
uintToFloat (Int16Value x) t = floatvalue t $ toRational (fromIntegral x :: Word16)
uintToFloat (Int32Value x) t = floatvalue t $ toRational (fromIntegral x :: Word32)
uintToFloat (Int64Value x) t = floatvalue t $ toRational (fromIntegral x :: Word64)

-- | Is the given value kind of zero?
zeroIsh :: PrimValue -> Bool
zeroIsh (IntValue (Int8Value k)) = k == 0
zeroIsh (IntValue (Int16Value k)) = k == 0
zeroIsh (IntValue (Int32Value k)) = k == 0
zeroIsh (IntValue (Int64Value k)) = k == 0
zeroIsh (FloatValue (Float32Value k)) = k == 0
zeroIsh (FloatValue (Float64Value k)) = k == 0
zeroIsh (BoolValue False) = True
zeroIsh _ = False

-- | Is the given value kind of one?
oneIsh :: PrimValue -> Bool
oneIsh (IntValue (Int8Value k)) = k == 1
oneIsh (IntValue (Int16Value k)) = k == 1
oneIsh (IntValue (Int32Value k)) = k == 1
oneIsh (IntValue (Int64Value k)) = k == 1
oneIsh (FloatValue (Float32Value k)) = k == 1
oneIsh (FloatValue (Float64Value k)) = k == 1
oneIsh (BoolValue True) = True
oneIsh _ = False
