{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Safe #-}
-- | Definitions of primitive types, the values that inhabit these
-- types, and operations on these values.  A primitive value can also
-- be called a scalar.
--
-- Essentially, this module describes the subset of the (internal)
-- Futhark language that operates on primitive types.
module Futhark.IR.Primitive
       ( -- * Types
         IntType (..), allIntTypes
       , FloatType (..), allFloatTypes
       , PrimType (..), allPrimTypes
       , Int8, Int16, Int32, Int64

         -- * Values
       , IntValue(..)
       , intValue, intValueType, valueIntegral
       , FloatValue(..)
       , floatValue, floatValueType
       , PrimValue(..)
       , primValueType
       , blankPrimValue

         -- * Operations
       , Overflow (..)
       , Safety(..)
       , UnOp (..), allUnOps
       , BinOp (..), allBinOps
       , ConvOp (..), allConvOps
       , CmpOp (..), allCmpOps

         -- ** Unary Operations
       , doUnOp
       , doComplement
       , doAbs, doFAbs
       , doSSignum, doUSignum

         -- ** Binary Operations
       , doBinOp

         -- ** Conversion Operations
       , doConvOp
--       , doZExt, doSExt
--       , doFPConv
--       , doFPToUI, doFPToSI
--       , doUIToFP, doSIToFP
--       , intToInt64, intToWord64

         -- * Comparison Operations
       , doCmpOp
       -- , doCmpEq
       -- , doCmpUlt, doCmpUle
       -- , doCmpSlt, doCmpSle
       -- , doFCmpLt, doFCmpLe

        -- * Type Of
       , binOpType
       , unOpType
       , cmpOpType
       , convOpType

       -- * Utility
       -- , zeroIsh
       -- , zeroIshInt
       -- , oneIsh
       -- , oneIshInt
       -- , negativeIsh
       , primBitSize
       , primByteSize
       , intByteSize
       , floatByteSize
       , commutativeBinOp

       -- * Prettyprinting
       , convOpFun
       , prettySigned

       -- * Untyped interface
       , UT(..)
       , UT2(..)
       , fmapUT
       , doBinOp'
       , doCmpOp'
       , doConvOp'
       , primFuns
       )
       where

import qualified Data.Binary.Get as G
import qualified Data.Binary.Put as P
import qualified Data.Bits as Bits
import Data.Fixed (mod') -- Weird location.
import Data.Int
import qualified Data.Map as M
import Data.Word

import Futhark.Util.Pretty
import Futhark.Util (roundFloat, ceilFloat, floorFloat,
                     roundDouble, ceilDouble, floorDouble,
                     lgamma, lgammaf, tgamma, tgammaf)

-- | An integer type, ordered by size.  Note that signedness is not a
-- property of the type, but a property of the operations performed on
-- values of these types.
data IntType t where
  Int8 :: IntType Int8
  Int16 :: IntType Int16
  Int32 :: IntType Int32
  Int64 :: IntType Int64

deriving instance Eq (IntType t)
deriving instance Ord (IntType t)
deriving instance Show (IntType t)

-- | A floating point type.
data FloatType t where
  Float32 :: FloatType Float
  Float64 :: FloatType Double

deriving instance Eq (FloatType t)
deriving instance Ord (FloatType t)
deriving instance Show (FloatType t)

-- | Low-level primitive types.
data PrimType t where
  IntType :: IntType it -> PrimType it
  FloatType :: FloatType ft -> PrimType ft
  Bool :: PrimType Bool
  Cert :: PrimType Bool

deriving instance Eq (PrimType t)
deriving instance Ord (PrimType t)
deriving instance Show (PrimType t)

-- | An integer value.
data IntValue t where
  Int8Value :: Int8 -> IntValue Int8
  Int16Value :: Int16 -> IntValue Int16
  Int32Value :: Int32 -> IntValue Int32
  Int64Value :: Int64 -> IntValue Int64

deriving instance Eq (IntValue t)
deriving instance Ord (IntValue t)
deriving instance Show (IntValue t)

-- | A floating-point value.
data FloatValue ft where
  Float32Value :: Float -> FloatValue Float
  Float64Value :: Double -> FloatValue Double

deriving instance Eq (FloatValue t)
deriving instance Ord (FloatValue t)
deriving instance Show (FloatValue t)

-- | Non-array values.
data PrimValue t where
  IntValue :: IntValue t -> PrimValue t
  FloatValue :: FloatValue t -> PrimValue t
  BoolValue :: Bool -> PrimValue Bool
  Checked :: PrimValue Bool

deriving instance Eq (PrimValue t)
deriving instance Ord (PrimValue t)
deriving instance Show (PrimValue t)

-- | The type of an integer value.
intValueType :: IntValue t -> IntType t
intValueType Int8Value{}  = Int8
intValueType Int16Value{} = Int16
intValueType Int32Value{} = Int32
intValueType Int64Value{} = Int64

-- | The type of a floating-point value.
floatValueType :: FloatValue t -> FloatType t
floatValueType Float32Value{} = Float32
floatValueType Float64Value{} = Float64

-- | The type of a basic value.
primValueType :: PrimValue t -> PrimType t
primValueType (IntValue v)   = IntType $ intValueType v
primValueType (FloatValue v) = FloatType $ floatValueType v
primValueType BoolValue{}    = Bool
primValueType Checked        = Cert

-- | Create an t'IntValue' from a value of the appropriate type.
intValue :: IntType t -> t -> IntValue t
intValue Int8  = Int8Value
intValue Int16 = Int16Value
intValue Int32 = Int32Value
intValue Int64 = Int64Value

-- | Create a t'FloatValue' from a value of the appropriate type.
floatValue :: FloatType t -> t -> FloatValue t
floatValue Float32 = Float32Value
floatValue Float64 = Float64Value

-- | A "blank" value of the given primitive type - this is zero, or
-- whatever is close to it.  Don't depend on this value, but use it
-- for e.g. creating arrays to be populated by do-loops.
blankPrimValue :: PrimType t -> PrimValue t
blankPrimValue (IntType Int8)      = IntValue $ Int8Value 0
blankPrimValue (IntType Int16)     = IntValue $ Int16Value 0
blankPrimValue (IntType Int32)     = IntValue $ Int32Value 0
blankPrimValue (IntType Int64)     = IntValue $ Int64Value 0
blankPrimValue (FloatType Float32) = FloatValue $ Float32Value 0.0
blankPrimValue (FloatType Float64) = FloatValue $ Float64Value 0.0
blankPrimValue Bool                = BoolValue False
blankPrimValue Cert                = Checked

-- | Convert an t'IntValue' to any 'Integral' type.
valueIntegral :: Integral int => IntValue t -> int
valueIntegral (Int8Value  v) = fromIntegral v
valueIntegral (Int16Value v) = fromIntegral v
valueIntegral (Int32Value v) = fromIntegral v
valueIntegral (Int64Value v) = fromIntegral v

-- | What to do in case of arithmetic overflow.  Futhark's semantics
-- are that overflow does wraparound, but for generated code (like
-- address arithmetic), it can be beneficial for overflow to be
-- undefined behaviour, as it allows better optimisation of things
-- such as GPU kernels.
--
-- Note that all values of this type are considered equal for 'Eq' and
-- 'Ord'.
data Overflow = OverflowWrap | OverflowUndef
              deriving (Show)


instance Eq Overflow where
  _ == _ = True

instance Ord Overflow where
  _ `compare` _ = EQ

-- | Whether something is safe or unsafe (mostly function calls, and
-- in the context of whether operations are dynamically checked).
-- When we inline an 'Unsafe' function, we remove all safety checks in
-- its body.  The 'Ord' instance picks 'Unsafe' as being less than
-- 'Safe'.
--
-- For operations like integer division, a safe division will not
-- explode the computer in case of division by zero, but instead
-- return some unspecified value.  This always involves a run-time
-- check, so generally the unsafe variant is what the compiler will
-- insert, but guarded by an explicit assertion elsewhere.  Safe
-- operations are useful when the optimiser wants to move e.g. a
-- division to a location where the divisor may be zero, but where the
-- result will only be used when it is non-zero (so it doesn't matter
-- what result is provided with a zero divisor, as long as the program
-- keeps running).
data Safety = Unsafe | Safe deriving (Eq, Ord, Show)

-- | Various unary operators.  It is a bit ad-hoc what is a unary
-- operator and what is a built-in function.  Perhaps these should all
-- go away eventually.
data UnOp t where
  Not :: UnOp Bool
  -- ^ E.g., @! True == False@.
  Complement :: IntType t -> UnOp t
  -- ^ E.g., @!(!1) = 1@.
  Abs :: IntType t-> UnOp t
  -- ^ @abs(-2) = 2@.
  FAbs :: FloatType t -> UnOp t
  -- ^ @fabs(-2.0) = 2.0@.
  SSignum :: IntType t -> UnOp t
  -- ^ Signed sign function: @ssignum(-2)@ = -1.
  USignum :: IntType t -> UnOp t
  -- ^ Unsigned sign function: @usignum(2)@ = 1.
  FSignum :: FloatType t-> UnOp t
  -- ^ Floating sign function: @usignum(2.0)@ = 1.0.

deriving instance Eq (UnOp t)
deriving instance Ord (UnOp t)
deriving instance (Show (UnOp t))

-- | Binary operators.  These correspond closely to the binary
-- operators in LLVM.  Most are parametrised by their expected input
-- and output types.
data BinOp t where
  Add :: IntType t -> Overflow -> BinOp t
  Sub :: IntType t -> Overflow -> BinOp t
  Mul :: IntType t -> Overflow -> BinOp t

  UDiv :: IntType t -> Safety -> BinOp t
  UDivUp :: IntType t-> Safety -> BinOp t

  SDiv :: IntType t -> Safety -> BinOp t
  SDivUp :: IntType t -> Safety -> BinOp t

  UMod :: IntType t -> Safety -> BinOp t
  SMod :: IntType t -> Safety -> BinOp t
  SQuot :: IntType t -> Safety -> BinOp t
  SRem :: IntType t -> Safety -> BinOp t

  Pow :: IntType t -> BinOp t

  Shl :: IntType t -> BinOp t
  LShr :: IntType t -> BinOp t
  AShr :: IntType t -> BinOp t

  SMin :: IntType t -> BinOp t
  UMin :: IntType t -> BinOp t
  SMax :: IntType t -> BinOp t
  UMax :: IntType t -> BinOp t
  And :: IntType t -> BinOp t
  Or :: IntType t -> BinOp t
  Xor :: IntType t -> BinOp t


  FAdd :: FloatType t -> BinOp t
  FSub :: FloatType t -> BinOp t
  FMul :: FloatType t -> BinOp t
  FDiv :: FloatType t -> BinOp t
  FMod :: FloatType t -> BinOp t
  FMin :: FloatType t -> BinOp t
  FMax :: FloatType t -> BinOp t
  FPow :: FloatType t -> BinOp t

  LogAnd :: BinOp Bool
  LogOr :: BinOp Bool

deriving instance Eq (BinOp t)
deriving instance Ord (BinOp t)
deriving instance Show (BinOp t)

-- | Comparison operators are like 'BinOp's, but they always return a
-- boolean value.  The somewhat ugly constructor names are straight
-- out of LLVM.
data CmpOp t where
  CmpEq :: PrimType t -> CmpOp t
  CmpUlt :: IntType t -> CmpOp t
  CmpUle :: IntType t -> CmpOp t
  CmpSlt :: IntType t -> CmpOp t
  CmpSle :: IntType t -> CmpOp t

  FCmpLt :: FloatType t -> CmpOp t
  FCmpLe :: FloatType t -> CmpOp t

  CmpLlt :: CmpOp Bool
  CmpLle :: CmpOp Bool

deriving instance Eq (CmpOp t)
deriving instance Ord (CmpOp t)
deriving instance Show (CmpOp t)

-- | Conversion operators try to generalise the @from t0 x to t1@
-- instructions from LLVM.
data ConvOp f t where
  ZExt :: IntType f -> IntType t -> ConvOp f t
  SExt :: IntType f -> IntType t -> ConvOp f t

  FPConv :: FloatType f -> FloatType t -> ConvOp f t
  FPToSI :: FloatType f -> IntType t -> ConvOp f t
  FPToUI :: FloatType f -> IntType t -> ConvOp f t
  UIToFP :: IntType f -> FloatType t -> ConvOp f t
  SIToFP :: IntType f -> FloatType t -> ConvOp f t

  IToB :: IntType f -> ConvOp f Bool
  BToI :: IntType t -> ConvOp Bool t

deriving instance Eq (ConvOp f t)
deriving instance Ord (ConvOp f t)
deriving instance Show (ConvOp f t)

word8 :: Int8 -> Word8
word8 = fromIntegral

word16 :: Int16 -> Word16
word16 = fromIntegral

word32 :: Int32 -> Word32
word32 = fromIntegral

word64 :: Int64 -> Word64
word64 = fromIntegral

int8 :: Word8 -> Int8
int8 = fromIntegral

int16 :: Word16 -> Int16
int16 = fromIntegral

int32 :: Word32 -> Int32
int32 = fromIntegral

int64 :: Word64 -> Int64
int64 = fromIntegral

intUnOp :: (forall int.(Bits.Bits int, Integral int) => int -> int)
        -> IntValue t -> IntValue t
intUnOp f (Int8Value x) = Int8Value $ f x
intUnOp f (Int16Value x) = Int16Value $ f x
intUnOp f (Int32Value x) = Int32Value $ f x
intUnOp f (Int64Value x) = Int64Value $ f x

floatUnOp :: (forall float.Real float => float -> float)
        -> FloatValue t -> FloatValue t
floatUnOp f (Float32Value x) = Float32Value $ f x
floatUnOp f (Float64Value x) = Float64Value $ f x

-- | E.g., @!(!1) = 1@.
doComplement :: IntValue t -> IntValue t
doComplement = intUnOp Bits.complement

-- | @abs(-2) = 2@.
doAbs :: IntValue t -> IntValue t
doAbs = intUnOp abs

-- | @abs(-2.0) = 2.0@.
doFAbs :: FloatValue t -> FloatValue t
doFAbs = floatUnOp abs

-- | @ssignum(-2)@ = -1.
doSSignum :: IntValue t -> IntValue t
doSSignum = intUnOp signum

-- | @usignum(-2)@ = -1.
doUSignum :: IntValue t -> IntValue t
doUSignum (Int8Value x) = Int8Value $ int8 $ signum $ word8 x
doUSignum (Int16Value x) = Int16Value $ int16 $ signum $ word16 x
doUSignum (Int32Value x) = Int32Value $ int32 $ signum $ word32 x
doUSignum (Int64Value x) = Int64Value $ int64 $ signum $ word64 x

doUnOp :: UnOp t -> PrimValue t -> Maybe (PrimValue t)
doUnOp Not (BoolValue b) = Just $ BoolValue $ not b
doUnOp Complement{} (IntValue v) = Just $ IntValue $ doComplement v
doUnOp Abs{} (IntValue v)        = Just $ IntValue $ doAbs v
doUnOp FAbs{} (FloatValue v)     = Just $ FloatValue $ doFAbs v
doUnOp SSignum{} (IntValue v)    = Just $ IntValue $ doSSignum v
doUnOp USignum{} (IntValue v)    = Just $ IntValue $ doUSignum v
doUnOp _ _ = Nothing

doIntBinOp :: BinOp t -> IntValue t -> IntValue t -> Maybe (IntValue t)

doIntBinOp Add{} (Int8Value x) (Int8Value y) =
  Just $ Int8Value $ x + y
doIntBinOp Add{} (Int16Value x) (Int16Value y) =
  Just $ Int16Value $ x + y
doIntBinOp Add{} (Int32Value x) (Int32Value y) =
  Just $ Int32Value $ x + y
doIntBinOp Add{} (Int64Value x) (Int64Value y) =
  Just $ Int64Value $ x + y

doIntBinOp Sub{} (Int8Value x) (Int8Value y) =
  Just $ Int8Value $ x - y
doIntBinOp Sub{} (Int16Value x) (Int16Value y) =
  Just $ Int16Value $ x - y
doIntBinOp Sub{} (Int32Value x) (Int32Value y) =
  Just $ Int32Value $ x - y
doIntBinOp Sub{} (Int64Value x) (Int64Value y) =
  Just $ Int64Value $ x - y

doIntBinOp Mul{} (Int8Value x) (Int8Value y) =
  Just $ Int8Value $ x * y
doIntBinOp Mul{} (Int16Value x) (Int16Value y) =
  Just $ Int16Value $ x * y
doIntBinOp Mul{} (Int32Value x) (Int32Value y) =
  Just $ Int32Value $ x * y
doIntBinOp Mul{} (Int64Value x) (Int64Value y) =
  Just $ Int64Value $ x * y

doIntBinOp UDiv{} (Int8Value x) (Int8Value y)
  | y == 0 = Nothing
  | otherwise = Just $ Int8Value $ int8 $ word8 x `div` word8 y
doIntBinOp UDiv{} (Int16Value x) (Int16Value y)
  | y == 0 = Nothing
  | otherwise = Just $ Int16Value $ int16 $ word16 x `div` word16 y
doIntBinOp UDiv{} (Int32Value x) (Int32Value y)
  | y == 0 = Nothing
  | otherwise = Just $ Int32Value $ int32 $ word32 x `div` word32 y
doIntBinOp UDiv{} (Int64Value x) (Int64Value y)
  | y == 0 = Nothing
  | otherwise = Just $ Int64Value $ int64 $ word64 x `div` word64 y

doIntBinOp UDivUp{} (Int8Value x) (Int8Value y)
  | y == 0 = Nothing
  | otherwise = Just $ Int8Value $
                int8 $ (word8 x + word8 y - 1) `div` word8 y
doIntBinOp UDivUp{} (Int16Value x) (Int16Value y)
  | y == 0 = Nothing
  | otherwise = Just $ Int16Value $
                int16 $ (word16 x + word16 y - 1) `div` word16 y
doIntBinOp UDivUp{} (Int32Value x) (Int32Value y)
  | y == 0 = Nothing
  | otherwise = Just $ Int32Value $
                int32 $ (word32 x + word32 y - 1) `div` word32 y
doIntBinOp UDivUp{} (Int64Value x) (Int64Value y)
  | y == 0 = Nothing
  | otherwise = Just $ Int64Value $
                int64 $ (word64 x + word64 y - 1) `div` word64 y

doIntBinOp SDiv{} (Int8Value x) (Int8Value y)
  | y == 0 = Nothing
  | otherwise = Just $ Int8Value $ x `div` y
doIntBinOp SDiv{} (Int16Value x) (Int16Value y)
  | y == 0 = Nothing
  | otherwise = Just $ Int16Value $ x `div` y
doIntBinOp SDiv{} (Int32Value x) (Int32Value y)
  | y == 0 = Nothing
  | otherwise = Just $ Int32Value $ x `div` y
doIntBinOp SDiv{} (Int64Value x) (Int64Value y)
  | y == 0 = Nothing
  | otherwise = Just $ Int64Value $ x `div` y

doIntBinOp SDivUp{} (Int8Value x) (Int8Value y)
  | y == 0 = Nothing
  | otherwise = Just $ Int8Value $ (x+y-1) `div` y
doIntBinOp SDivUp{} (Int16Value x) (Int16Value y)
  | y == 0 = Nothing
  | otherwise = Just $ Int16Value $ (x+y-1) `div` y
doIntBinOp SDivUp{} (Int32Value x) (Int32Value y)
  | y == 0 = Nothing
  | otherwise = Just $ Int32Value $ (x+y-1) `div` y
doIntBinOp SDivUp{} (Int64Value x) (Int64Value y)
  | y == 0 = Nothing
  | otherwise = Just $ Int64Value $ (x+y-1) `div` y

doIntBinOp UMod{} (Int8Value x) (Int8Value y)
  | y == 0 = Nothing
  | otherwise = Just $ Int8Value $ int8 $ word8 x `mod` word8 y
doIntBinOp UMod{} (Int16Value x) (Int16Value y)
  | y == 0 = Nothing
  | otherwise = Just $ Int16Value $ int16 $ word16 x `mod` word16 y
doIntBinOp UMod{} (Int32Value x) (Int32Value y)
  | y == 0 = Nothing
  | otherwise = Just $ Int32Value $ int32 $ word32 x `mod` word32 y
doIntBinOp UMod{} (Int64Value x) (Int64Value y)
  | y == 0 = Nothing
  | otherwise = Just $ Int64Value $ int64 $ word64 x `mod` word64 y

doIntBinOp SMod{} (Int8Value x) (Int8Value y)
  | y == 0 = Nothing
  | otherwise = Just $ Int8Value $ x `mod` y
doIntBinOp SMod{} (Int16Value x) (Int16Value y)
  | y == 0 = Nothing
  | otherwise = Just $ Int16Value $ x `mod` y
doIntBinOp SMod{} (Int32Value x) (Int32Value y)
  | y == 0 = Nothing
  | otherwise = Just $ Int32Value $ x `mod` y
doIntBinOp SMod{} (Int64Value x) (Int64Value y)
  | y == 0 = Nothing
  | otherwise = Just $ Int64Value $ x `mod` y

doIntBinOp SQuot{} (Int8Value x) (Int8Value y)
  | y == 0 = Nothing
  | otherwise = Just $ Int8Value $ x `quot` y
doIntBinOp SQuot{} (Int16Value x) (Int16Value y)
  | y == 0 = Nothing
  | otherwise = Just $ Int16Value $ x `quot` y
doIntBinOp SQuot{} (Int32Value x) (Int32Value y)
  | y == 0 = Nothing
  | otherwise = Just $ Int32Value $ x `quot` y
doIntBinOp SQuot{} (Int64Value x) (Int64Value y)
  | y == 0 = Nothing
  | otherwise = Just $ Int64Value $ x `quot` y

doIntBinOp SRem{} (Int8Value x) (Int8Value y)
  | y == 0 = Nothing
  | otherwise = Just $ Int8Value $ x `rem` y
doIntBinOp SRem{} (Int16Value x) (Int16Value y)
  | y == 0 = Nothing
  | otherwise = Just $ Int16Value $ x `rem` y
doIntBinOp SRem{} (Int32Value x) (Int32Value y)
  | y == 0 = Nothing
  | otherwise = Just $ Int32Value $ x `rem` y
doIntBinOp SRem{} (Int64Value x) (Int64Value y)
  | y == 0 = Nothing
  | otherwise = Just $ Int64Value $ x `rem` y

doIntBinOp Pow{} (Int8Value x) (Int8Value y)
  | y == 0 = Nothing
  | otherwise = Just $ Int8Value $ x ^ y
doIntBinOp Pow{} (Int16Value x) (Int16Value y)
  | y == 0 = Nothing
  | otherwise = Just $ Int16Value $ x ^ y
doIntBinOp Pow{} (Int32Value x) (Int32Value y)
  | y == 0 = Nothing
  | otherwise = Just $ Int32Value $ x ^ y
doIntBinOp Pow{} (Int64Value x) (Int64Value y)
  | y == 0 = Nothing
  | otherwise = Just $ Int64Value $ x ^ y

doIntBinOp Shl{} (Int8Value x) (Int8Value y) =
  Just $ Int8Value $ x `Bits.shift` fromIntegral y
doIntBinOp Shl{} (Int16Value x) (Int16Value y) =
  Just $ Int16Value $ x `Bits.shift` fromIntegral y
doIntBinOp Shl{} (Int32Value x) (Int32Value y) =
  Just $ Int32Value $ x `Bits.shift` fromIntegral y
doIntBinOp Shl{} (Int64Value x) (Int64Value y) =
  Just $ Int64Value $ x `Bits.shift` fromIntegral y

doIntBinOp LShr{} (Int8Value x) (Int8Value y) =
  Just $ Int8Value $ int8 $ word8 x `Bits.shift` (-fromIntegral y)
doIntBinOp LShr{} (Int16Value x) (Int16Value y) =
  Just $ Int16Value $ int16 $ word16 x `Bits.shift` (-fromIntegral y)
doIntBinOp LShr{} (Int32Value x) (Int32Value y) =
  Just $ Int32Value $ int32 $ word32 x `Bits.shift` (-fromIntegral y)
doIntBinOp LShr{} (Int64Value x) (Int64Value y) =
  Just $ Int64Value $ int64 $ word64 x `Bits.shift` (-fromIntegral y)

doIntBinOp AShr{} (Int8Value x) (Int8Value y) =
  Just $ Int8Value $ x `Bits.shift` (-fromIntegral y)
doIntBinOp AShr{} (Int16Value x) (Int16Value y) =
  Just $ Int16Value $ x `Bits.shift` (-fromIntegral y)
doIntBinOp AShr{} (Int32Value x) (Int32Value y) =
  Just $ Int32Value $ x `Bits.shift` (-fromIntegral y)
doIntBinOp AShr{} (Int64Value x) (Int64Value y) =
  Just $ Int64Value $ x `Bits.shift` (-fromIntegral y)

doIntBinOp SMin{} (Int8Value x) (Int8Value y) =
  Just $ Int8Value $ x `min` y
doIntBinOp SMin{} (Int16Value x) (Int16Value y) =
  Just $ Int16Value $ x `min` y
doIntBinOp SMin{} (Int32Value x) (Int32Value y) =
  Just $ Int32Value $ x `min` y
doIntBinOp SMin{} (Int64Value x) (Int64Value y) =
  Just $ Int64Value $ x `min` y

doIntBinOp UMin{} (Int8Value x) (Int8Value y) =
  Just $ Int8Value $ int8 $ word8 x `min` word8 y
doIntBinOp UMin{} (Int16Value x) (Int16Value y) =
  Just $ Int16Value $ int16 $ word16 x `min` word16 y
doIntBinOp UMin{} (Int32Value x) (Int32Value y) =
  Just $ Int32Value $ int32 $ word32 x `min` word32 y
doIntBinOp UMin{} (Int64Value x) (Int64Value y) =
  Just $ Int64Value $ int64 $ word64 x `min` word64 y

doIntBinOp SMax{} (Int8Value x) (Int8Value y) =
  Just $ Int8Value $ x `max` y
doIntBinOp SMax{} (Int16Value x) (Int16Value y) =
  Just $ Int16Value $ x `max` y
doIntBinOp SMax{} (Int32Value x) (Int32Value y) =
  Just $ Int32Value $ x `max` y
doIntBinOp SMax{} (Int64Value x) (Int64Value y) =
  Just $ Int64Value $ x `max` y

doIntBinOp UMax{} (Int8Value x) (Int8Value y) =
  Just $ Int8Value $ int8 $ word8 x `max` word8 y
doIntBinOp UMax{} (Int16Value x) (Int16Value y) =
  Just $ Int16Value $ int16 $ word16 x `max` word16 y
doIntBinOp UMax{} (Int32Value x) (Int32Value y) =
  Just $ Int32Value $ int32 $ word32 x `max` word32 y
doIntBinOp UMax{} (Int64Value x) (Int64Value y) =
  Just $ Int64Value $ int64 $ word64 x `max` word64 y

doIntBinOp And{} (Int8Value x) (Int8Value y) =
  Just $ Int8Value $ x Bits..&. y
doIntBinOp And{} (Int16Value x) (Int16Value y) =
  Just $ Int16Value $ x Bits..&. y
doIntBinOp And{} (Int32Value x) (Int32Value y) =
  Just $ Int32Value $ x Bits..&. y
doIntBinOp And{} (Int64Value x) (Int64Value y) =
  Just $ Int64Value $ x Bits..&. y

doIntBinOp Or{} (Int8Value x) (Int8Value y) =
  Just $ Int8Value $ x Bits..|. y
doIntBinOp Or{} (Int16Value x) (Int16Value y) =
  Just $ Int16Value $ x Bits..|. y
doIntBinOp Or{} (Int32Value x) (Int32Value y) =
  Just $ Int32Value $ x Bits..|. y
doIntBinOp Or{} (Int64Value x) (Int64Value y) =
  Just $ Int64Value $ x Bits..|. y

doIntBinOp Xor{} (Int8Value x) (Int8Value y) =
  Just $ Int8Value $ x `Bits.xor` y
doIntBinOp Xor{} (Int16Value x) (Int16Value y) =
  Just $ Int16Value $ x `Bits.xor` y
doIntBinOp Xor{} (Int32Value x) (Int32Value y) =
  Just $ Int32Value $ x `Bits.xor` y
doIntBinOp Xor{} (Int64Value x) (Int64Value y) =
  Just $ Int64Value $ x `Bits.xor` y

doIntBinOp _ _ _ = Nothing

doFloatBinOp :: BinOp t -> FloatValue t -> FloatValue t -> Maybe (FloatValue t)
doFloatBinOp FAdd{} (Float32Value x) (Float32Value y) =
  Just $ Float32Value $ x + y
doFloatBinOp FAdd{} (Float64Value x) (Float64Value y) =
  Just $ Float64Value $ x + y

doFloatBinOp FSub{} (Float32Value x) (Float32Value y) =
  Just $ Float32Value $ x + y
doFloatBinOp FSub{} (Float64Value x) (Float64Value y) =
  Just $ Float64Value $ x - y

doFloatBinOp FMul{} (Float32Value x) (Float32Value y) =
  Just $ Float32Value $ x * y
doFloatBinOp FMul{} (Float64Value x) (Float64Value y) =
  Just $ Float64Value $ x * y

doFloatBinOp FDiv{} (Float32Value x) (Float32Value y) =
  Just $ Float32Value $ x / y
doFloatBinOp FDiv{} (Float64Value x) (Float64Value y) =
  Just $ Float64Value $ x / y

doFloatBinOp FMod{} (Float32Value x) (Float32Value y) =
  Just $ Float32Value $ x `mod'` y
doFloatBinOp FMod{} (Float64Value x) (Float64Value y) =
  Just $ Float64Value $ x `mod'` y

doFloatBinOp FMin{} (Float32Value x) (Float32Value y) =
  Just $ Float32Value $ x `min` y
doFloatBinOp FMin{} (Float64Value x) (Float64Value y) =
  Just $ Float64Value $ x `min` y

doFloatBinOp FMax{} (Float32Value x) (Float32Value y) =
  Just $ Float32Value $ x `max` y
doFloatBinOp FMax{} (Float64Value x) (Float64Value y) =
  Just $ Float64Value $ x `max` y

doFloatBinOp FPow{} (Float32Value x) (Float32Value y) =
  Just $ Float32Value $ x ** y
doFloatBinOp FPow{} (Float64Value x) (Float64Value y) =
  Just $ Float64Value $ x ** y

doFloatBinOp _ _ _ = Nothing

doBinOp :: BinOp t -> PrimValue t -> PrimValue t -> Maybe (PrimValue t)
doBinOp op (IntValue x) (IntValue y) = IntValue <$> doIntBinOp op x y
doBinOp op (FloatValue x) (FloatValue y) = FloatValue <$> doFloatBinOp op x y
doBinOp LogAnd{} (BoolValue x) (BoolValue y) = Just $ BoolValue $ x && y
doBinOp LogOr{} (BoolValue x) (BoolValue y) = Just $ BoolValue $ x || y
doBinOp _ _ _ = Nothing

-- | Apply a 'ConvOp' to an operand.
doConvOp :: ConvOp f t -> PrimValue f -> PrimValue t
-- doConvOp (ZExt _ to) (IntValue v)     = IntValue $ doZExt v to
-- doConvOp (SExt _ to) (IntValue v)     = IntValue $ doSExt v to
-- doConvOp (FPConv _ to) (FloatValue v) = FloatValue $ doFPConv v to
-- doConvOp (FPToUI _ to) (FloatValue v) = IntValue $ doFPToUI v to
-- doConvOp (FPToSI _ to) (FloatValue v) = IntValue $ doFPToSI v to
-- doConvOp (UIToFP _ to) (IntValue v)   = FloatValue $ doUIToFP v to
-- doConvOp (SIToFP _ to) (IntValue v)   = FloatValue $ doSIToFP v to
-- doConvOp (IToB _) (IntValue v)        = BoolValue $ intToInt64 v /= 0
-- doConvOp (BToI to) (BoolValue v)      = IntValue $ intValue to $ if v then 1 else 0::Int
doConvOp _ _                          = error "doConvOp: missing case"

doCmpOp :: CmpOp t -> PrimValue t -> PrimValue t -> Bool
doCmpOp _ _ _ = error "doCmpOp: missing case"

-- | The result type of a binary operator.
binOpType :: BinOp t -> PrimType t
binOpType (Add t _) = IntType t
binOpType (Sub t _) = IntType t
binOpType (Mul t _) = IntType t
binOpType (SDiv t _)   = IntType t
binOpType (SDivUp t _) = IntType t
binOpType (SMod t _)  = IntType t
binOpType (SQuot t _) = IntType t
binOpType (SRem t _)  = IntType t
binOpType (UDiv t _)  = IntType t
binOpType (UDivUp t _) = IntType t
binOpType (UMod t _)   = IntType t
binOpType (SMin t)  = IntType t
binOpType (UMin t)  = IntType t
binOpType (FMin t)  = FloatType t
binOpType (SMax t)  = IntType t
binOpType (UMax t)  = IntType t
binOpType (FMax t)  = FloatType t
binOpType (Shl t)   = IntType t
binOpType (LShr t)  = IntType t
binOpType (AShr t)  = IntType t
binOpType (And t)   = IntType t
binOpType (Or t)    = IntType t
binOpType (Xor t)   = IntType t
binOpType (Pow t)   = IntType t
binOpType (FPow t)  = FloatType t
binOpType LogAnd    = Bool
binOpType LogOr     = Bool
binOpType (FAdd t)  = FloatType t
binOpType (FSub t)  = FloatType t
binOpType (FMul t)  = FloatType t
binOpType (FDiv t)  = FloatType t
binOpType (FMod t)  = FloatType t

-- | The operand types of a comparison operator.
cmpOpType :: CmpOp t -> PrimType t
cmpOpType (CmpEq t) = t
cmpOpType (CmpSlt t) = IntType t
cmpOpType (CmpSle t) = IntType t
cmpOpType (CmpUlt t) = IntType t
cmpOpType (CmpUle t) = IntType t
cmpOpType (FCmpLt t) = FloatType t
cmpOpType (FCmpLe t) = FloatType t
cmpOpType CmpLlt = Bool
cmpOpType CmpLle = Bool

-- | The operand and result type of a unary operator.
unOpType :: UnOp t -> PrimType t
unOpType (SSignum t)    = IntType t
unOpType (USignum t)    = IntType t
unOpType (FSignum t)    = FloatType t
unOpType Not            = Bool
unOpType (Complement t) = IntType t
unOpType (Abs t)        = IntType t
unOpType (FAbs t)       = FloatType t

-- | The input and output types of a conversion operator.
convOpType :: ConvOp f t -> (PrimType f, PrimType t)
convOpType (ZExt from to) = (IntType from, IntType to)
convOpType (SExt from to) = (IntType from, IntType to)
convOpType (FPConv from to) = (FloatType from, FloatType to)
convOpType (FPToUI from to) = (FloatType from, IntType to)
convOpType (FPToSI from to) = (FloatType from, IntType to)
convOpType (UIToFP from to) = (IntType from, FloatType to)
convOpType (SIToFP from to) = (IntType from, FloatType to)
convOpType (IToB from) = (IntType from, Bool)
convOpType (BToI to) = (Bool, IntType to)

-- | The size of a value of a given primitive type in bites.
primBitSize :: PrimType t -> Int
primBitSize = (*8) . primByteSize

-- | The size of a value of a given primitive type in eight-bit bytes.
primByteSize :: Num a => PrimType t -> a
primByteSize (IntType t)   = intByteSize t
primByteSize (FloatType t) = floatByteSize t
primByteSize Bool          = 1
primByteSize Cert          = 1

-- | The size of a value of a given integer type in eight-bit bytes.
intByteSize :: Num a => IntType t -> a
intByteSize Int8  = 1
intByteSize Int16 = 2
intByteSize Int32 = 4
intByteSize Int64 = 8

-- | The size of a value of a given floating-point type in eight-bit bytes.
floatByteSize :: Num a => FloatType t -> a
floatByteSize Float32 = 4
floatByteSize Float64 = 8

-- | True if the given binary operator is commutative.
commutativeBinOp :: BinOp t -> Bool
commutativeBinOp Add{} = True
commutativeBinOp FAdd{} = True
commutativeBinOp Mul{} = True
commutativeBinOp FMul{} = True
commutativeBinOp And{} = True
commutativeBinOp Or{} = True
commutativeBinOp Xor{} = True
commutativeBinOp LogOr{} = True
commutativeBinOp LogAnd{} = True
commutativeBinOp SMax{} = True
commutativeBinOp SMin{} = True
commutativeBinOp UMax{} = True
commutativeBinOp UMin{} = True
commutativeBinOp FMax{} = True
commutativeBinOp FMin{} = True
commutativeBinOp _ = False

-- Untyped interface

data UT f where
  UT :: forall f t.f t -> UT f

data UT2 f where
  UT2 :: forall f t1 t2.f t1 t2 -> UT2 f

deriving instance Show (UT IntType)
deriving instance Show (UT PrimType)
deriving instance Show (UT PrimValue)
deriving instance Show (UT UnOp)
deriving instance Show (UT BinOp)
deriving instance Show (UT2 ConvOp)
deriving instance Show (UT CmpOp)

-- XXX: now for some ugly stuff that works anyway.

instance Eq (UT IntType) where
  x == y = compare x y == EQ
instance Ord (UT IntType) where
  x `compare` y = show x `compare` show y

instance Eq (UT PrimType) where
  x == y = compare x y == EQ
instance Ord (UT PrimType) where
  x `compare` y = show x `compare` show y

instance Eq (UT PrimValue) where
  x == y = compare x y == EQ
instance Ord (UT PrimValue) where
  x `compare` y = show x `compare` show y

instance Eq (UT UnOp) where
  x == y = compare x y == EQ
instance Ord (UT UnOp) where
  x `compare` y = show x `compare` show y

instance Eq (UT BinOp) where
  x == y = compare x y == EQ
instance Ord (UT BinOp) where
  x `compare` y = show x `compare` show y

instance Eq (UT CmpOp) where
  x == y = compare x y == EQ
instance Ord (UT CmpOp) where
  x `compare` y = show x `compare` show y

instance Eq (UT2 ConvOp) where
  x == y = compare x y == EQ
instance Ord (UT2 ConvOp) where
  x `compare` y = show x `compare` show y

fmapUT :: (forall t. f t -> f t) -> UT f -> UT f
fmapUT f (UT x) = UT (f x)

expect :: PrimValue got -> PrimType expected -> Maybe (PrimValue expected)
expect (IntValue (Int8Value x)) (IntType Int8) =
  Just (IntValue $ Int8Value x)
expect (IntValue (Int16Value x)) (IntType Int16) =
  Just (IntValue $ Int16Value x)
expect (IntValue (Int32Value x)) (IntType Int32) =
  Just (IntValue $ Int32Value x)
expect (IntValue (Int64Value x)) (IntType Int64) =
  Just (IntValue $ Int64Value x)
expect (FloatValue (Float32Value x)) (FloatType Float32) =
  Just (FloatValue $ Float32Value x)
expect (FloatValue (Float64Value x)) (FloatType Float64) =
  Just (FloatValue $ Float64Value x)
expect (BoolValue x) Bool =
  Just $ BoolValue x
expect _ _ =
  Nothing

doBinOp' :: UT BinOp -> UT PrimValue -> UT PrimValue -> Maybe (UT PrimValue)
doBinOp' (UT op) (UT x) (UT y) = do
  x' <- expect x $ binOpType op
  y' <- expect y $ binOpType op
  UT <$> doBinOp op x' y'

doConvOp' :: UT2 ConvOp -> UT PrimValue -> Maybe (UT PrimValue)
doConvOp' (UT2 op) (UT x) = do
  let (from, _to) = convOpType op
  x' <- expect x from
  Just $ UT $ doConvOp op x'

doCmpOp' :: UT CmpOp -> UT PrimValue -> UT PrimValue -> Maybe (UT PrimValue)
doCmpOp' (UT op) (UT x) (UT y) = do
  x' <- expect x $ cmpOpType op
  y' <- expect y $ cmpOpType op
  Just $ UT $ BoolValue $ doCmpOp op x' y'

allIntTypes :: [UT IntType]
allIntTypes = [UT Int8, UT Int16, UT Int32, UT Int64]

allFloatTypes :: [UT FloatType]
allFloatTypes = [UT Float32, UT Float64]

allPrimTypes :: [UT PrimType]
allPrimTypes =
  [UT $ IntType Int8, UT $ IntType Int16, UT $ IntType Int32, UT $ IntType Int64]
  ++ [UT $ FloatType Float32, UT $ FloatType Float64]
  ++ [UT Bool, UT Cert]

allUnOps :: [UT UnOp]
allUnOps =
  concat [ [UT Not]
         , [ UT $ Abs t | UT t <- allIntTypes ]
         , [ UT $ FAbs t | UT t <- allFloatTypes ]
         , [ UT $ SSignum t | UT t <- allIntTypes ]
         , [ UT $ USignum t | UT t <- allIntTypes ]
         , [ UT $ FSignum t | UT t <- allFloatTypes ]
         ]

allCmpOps :: [UT CmpOp]
allCmpOps = undefined

allBinOps :: [UT BinOp]
allBinOps = undefined

allConvOps :: [UT2 ConvOp]
allConvOps = undefined

floatToWord :: Float -> Word32
floatToWord = G.runGet G.getWord32le . P.runPut . P.putFloatle

wordToFloat :: Word32 -> Float
wordToFloat = G.runGet G.getFloatle . P.runPut . P.putWord32le

doubleToWord :: Double -> Word64
doubleToWord = G.runGet G.getWord64le . P.runPut . P.putDoublele

wordToDouble :: Word64 -> Double
wordToDouble = G.runGet G.getDoublele . P.runPut . P.putWord64le


mul_hi8 :: Int8 -> Int8 -> Int8
mul_hi8 a b =
  fromIntegral $ word64 $
  fromIntegral (word8 a) * fromIntegral (word8 b) `Bits.shiftR` 8

mul_hi16 :: Int16 -> Int16 -> Int16
mul_hi16 a b =
  fromIntegral $ word64 $
  fromIntegral (word16 a) * fromIntegral (word16 b) `Bits.shiftR` 16

mul_hi32 :: Int32 -> Int32 -> Int32
mul_hi32 a b =
  fromIntegral $ word32 $
  fromIntegral (word32 a) * fromIntegral (word32 b) `Bits.shiftR` 32

mul_hi64 :: Int64 -> Int64 -> Int64
mul_hi64 a b =
  int64 $ word64 $ fromInteger $
  toInteger (word64 a) * toInteger (word64 b) `Bits.shiftR` 64

mad_hi8 :: Int8 -> Int8 -> Int8 -> Int8
mad_hi8 a b c = mul_hi8 a b + c

mad_hi16 :: Int16 -> Int16 -> Int16 -> Int16
mad_hi16 a b c = mul_hi16 a b + c

mad_hi32 :: Int32 -> Int32 -> Int32 -> Int32
mad_hi32 a b c = mul_hi32 a b + c

mad_hi64 :: Int64 -> Int64 -> Int64 -> Int64
mad_hi64 a b c = mul_hi64 a b + c

-- | A mapping from names of primitive functions to their parameter
-- types, their result type, and a function for evaluating them.  This
-- is necessarily untyped.
primFuns :: M.Map String ([UT PrimType], UT PrimType,
                          [UT PrimValue] -> Maybe (UT PrimValue))
primFuns = M.fromList
  [ f32 "sqrt32" sqrt, f64 "sqrt64" sqrt
  , f32 "log32" log, f64 "log64" log
  , f32 "log10_32" (logBase 10), f64 "log10_64" (logBase 10)
  , f32 "log2_32" (logBase 2), f64 "log2_64" (logBase 2)
  , f32 "exp32" exp, f64 "exp64" exp

  , f32 "sin32" sin, f64 "sin64" sin
  , f32 "sinh32" sinh, f64 "sinh64" sinh
  , f32 "cos32" cos, f64 "cos64" cos
  , f32 "cosh32" cosh, f64 "cosh64" cosh
  , f32 "tan32" tan, f64 "tan64" tan
  , f32 "tanh32" tanh, f64 "tanh64" tanh
  , f32 "asin32" asin, f64 "asin64" asin
  , f32 "asinh32" asinh, f64 "asinh64" asinh
  , f32 "acos32" acos, f64 "acos64" acos
  , f32 "acosh32" acosh, f64 "acosh64" acosh
  , f32 "atan32" atan, f64 "atan64" atan
  , f32 "atanh32" atanh, f64 "atanh64" atanh

  , f32 "round32" roundFloat, f64 "round64" roundDouble
  , f32 "ceil32" ceilFloat, f64 "ceil64" ceilDouble
  , f32 "floor32" floorFloat, f64 "floor64" floorDouble
  , f32 "gamma32" tgammaf, f64 "gamma64" tgamma
  , f32 "lgamma32" lgammaf, f64 "lgamma64" lgamma

  , i8 "clz8" $ fromIntegral . Bits.countLeadingZeros
  , i16 "clz16" $ fromIntegral . Bits.countLeadingZeros
  , i32 "clz32" $ fromIntegral . Bits.countLeadingZeros
  , i64 "clz64" $ fromIntegral . Bits.countLeadingZeros

  , i8 "ctz8" $ fromIntegral . Bits.countTrailingZeros
  , i16 "ctz16" $ fromIntegral . Bits.countTrailingZeros
  , i32 "ctz32" $ fromIntegral . Bits.countTrailingZeros
  , i64 "ctz64" $ fromIntegral . Bits.countTrailingZeros

  , i8 "popc8" $ fromIntegral . Bits.popCount
  , i16 "popc16" $ fromIntegral . Bits.popCount
  , i32 "popc32" $ fromIntegral . Bits.popCount
  , i64 "popc64" $ fromIntegral . Bits.popCount

  , ("mad_hi8", ([UT (IntType Int8),
                  UT (IntType Int8),
                  UT (IntType Int8)],
                  UT (IntType Int8),
                 \case
                   [UT (IntValue (Int8Value a)),
                    UT (IntValue (Int8Value b)),
                    UT (IntValue (Int8Value c))] ->
                     Just . UT . IntValue . Int8Value $ mad_hi8 a b c
                   _ -> Nothing
                ))
  , ("mad_hi16", ([UT (IntType Int16),
                   UT (IntType Int16),
                   UT (IntType Int16)],
                   UT (IntType Int16),
                 \case
                   [UT (IntValue (Int16Value a)),
                    UT (IntValue (Int16Value b)),
                    UT (IntValue (Int16Value c))] ->
                     Just . UT . IntValue . Int16Value $ mad_hi16 a b c
                   _ -> Nothing
                ))
  , ("mad_hi32", ([UT (IntType Int32),
                   UT (IntType Int32),
                   UT (IntType Int32)],
                   UT (IntType Int32),
                  \case
                   [UT (IntValue (Int32Value a)),
                    UT (IntValue (Int32Value b)),
                    UT (IntValue (Int32Value c))] ->
                     Just . UT . IntValue . Int32Value  $ mad_hi32 a b c
                   _ -> Nothing
                ))
  , ("mad_hi64", ([UT (IntType Int64),
                   UT (IntType Int64),
                   UT (IntType Int64)],
                   UT (IntType Int64),
                  \case
                    [UT (IntValue (Int64Value a)),
                     UT (IntValue (Int64Value b)),
                     UT (IntValue (Int64Value c))] ->
                      Just . UT . IntValue . Int64Value $ mad_hi64 a b c
                    _ -> Nothing
                ))

  , ("mul_hi8", ([UT (IntType Int8),
                  UT (IntType Int8)],
                  UT (IntType Int8),
                 \case
                   [UT (IntValue (Int8Value a)),
                    UT (IntValue (Int8Value b))] ->
                     Just . UT . IntValue . Int8Value $ mul_hi8 a b
                   _ -> Nothing
                ))
  , ("mul_hi16", ([UT (IntType Int16),
                   UT (IntType Int16)],
                   UT (IntType Int16),
                 \case
                   [UT (IntValue (Int16Value a)),
                    UT (IntValue (Int16Value b))] ->
                     Just . UT . IntValue . Int16Value $ mul_hi16 a b
                   _ -> Nothing
                ))
  , ("mul_hi32", ([UT (IntType Int32),
                   UT (IntType Int32)],
                   UT (IntType Int32),
                 \case
                   [UT (IntValue (Int32Value a)),
                    UT (IntValue (Int32Value b))] ->
                     Just . UT . IntValue . Int32Value $ mul_hi32 a b
                   _ -> Nothing
                ))
  , ("mul_hi64", ([UT (IntType Int64),
                   UT (IntType Int64)],
                  UT (IntType Int64),
                  \case
                    [UT (IntValue (Int64Value a)),
                     UT (IntValue (Int64Value b))] ->
                      Just . UT . IntValue . Int64Value $ mul_hi64 a b
                    _ -> Nothing
                ))

  , ("atan2_32",
     ([UT (FloatType Float32),
       UT (FloatType Float32)],
       UT (FloatType Float32),
      \case
        [UT (FloatValue (Float32Value x)),
         UT (FloatValue (Float32Value y))] ->
          Just . UT . FloatValue . Float32Value $ atan2 x y
        _ -> Nothing))
  , ("atan2_64",
     ([UT (FloatType Float64),
       UT (FloatType Float64)],
       UT (FloatType Float64),
       \case
         [UT (FloatValue (Float64Value x)),
          UT (FloatValue (Float64Value y))] ->
           Just . UT . FloatValue . Float64Value $ atan2 x y
         _ -> Nothing))

  , ("isinf32",
     ([UT (FloatType Float32)],
      UT Bool,
      \case
        [UT (FloatValue (Float32Value x))] -> Just . UT . BoolValue $ isInfinite x
        _ -> Nothing))
  , ("isinf64",
     ([UT (FloatType Float64)], UT Bool,
      \case
        [UT (FloatValue (Float64Value x))] -> Just . UT . BoolValue $ isInfinite x
        _ -> Nothing))

  , ("isnan32",
     ([UT (FloatType Float32)], UT Bool,
      \case
        [UT (FloatValue (Float32Value x))] -> Just . UT . BoolValue $ isNaN x
        _ -> Nothing))
  , ("isnan64",
     ([UT (FloatType Float64)], UT Bool,
      \case
        [UT (FloatValue (Float64Value x))] -> Just . UT . BoolValue $ isNaN x
        _ -> Nothing))

  , ("to_bits32",
     ([UT (FloatType Float32)], UT (IntType Int32),
      \case
        [UT (FloatValue (Float32Value x))] ->
          Just . UT . IntValue . Int32Value $ fromIntegral $ floatToWord x
        _ -> Nothing))
  , ("to_bits64",
     ([UT (FloatType Float64)], UT (IntType Int64),
      \case
        [UT (FloatValue (Float64Value x))] ->
          Just $ UT $ IntValue $ Int64Value $ fromIntegral $ doubleToWord x
        _ -> Nothing))

  , ("from_bits32",
     ([UT (IntType Int32)], UT (FloatType Float32),
      \case
        [UT (IntValue (Int32Value x))] ->
          Just $ UT $ FloatValue $ Float32Value $ wordToFloat $ fromIntegral x
        _ -> Nothing))
  , ("from_bits64",
     ([UT (IntType Int64)], UT (FloatType Float64),
      \case
        [UT (IntValue (Int64Value x))] ->
          Just $ UT $ FloatValue $ Float64Value $ wordToDouble $ fromIntegral x
        _ -> Nothing))

  , f32_3 "lerp32" (\v0 v1 t -> v0 + (v1-v0)*max 0 (min 1 t))
  , f64_3 "lerp64" (\v0 v1 t -> v0 + (v1-v0)*max 0 (min 1 t))

  , f32_3 "mad32" (\a b c -> a*b+c)
  , f64_3 "mad64" (\a b c -> a*b+c)

  , f32_3 "fma32" (\a b c -> a*b+c)
  , f64_3 "fma64" (\a b c -> a*b+c)

  ]
  where i8 s f = (s, ([UT (IntType Int8)], UT (IntType Int32), i8PrimFun f))
        i16 s f = (s, ([UT (IntType Int16)], UT (IntType Int32), i16PrimFun f))
        i32 s f = (s, ([UT (IntType Int32)], UT (IntType Int32), i32PrimFun f))
        i64 s f = (s, ([UT (IntType Int64)], UT (IntType Int32), i64PrimFun f))
        f32 s f = (s, ([UT (FloatType Float32)], UT (FloatType Float32), f32PrimFun f))
        f64 s f = (s, ([UT (FloatType Float64)], UT (FloatType Float64), f64PrimFun f))
        f32_3 s f = (s, ([UT (FloatType Float32),
                          UT (FloatType Float32),
                          UT (FloatType Float32)],
                          UT (FloatType Float32),
                          f32PrimFun3 f))
        f64_3 s f = (s, ([UT (FloatType Float64),
                          UT (FloatType Float64),
                          UT (FloatType Float64)],
                         UT (FloatType Float64),
                         f64PrimFun3 f))

        i8PrimFun :: (Int8 -> Int32) -> [UT PrimValue] -> Maybe (UT PrimValue)
        i8PrimFun f [UT (IntValue (Int8Value x))] =
          Just $ UT $ IntValue $ Int32Value $ f x
        i8PrimFun _ _ = Nothing

        i16PrimFun :: (Int16 -> Int32) -> [UT PrimValue] -> Maybe (UT PrimValue)
        i16PrimFun f [UT (IntValue (Int16Value x))] =
          Just $ UT $ IntValue $ Int32Value $ f x
        i16PrimFun _ _ = Nothing

        i32PrimFun :: (Int32 -> Int32) -> [UT PrimValue] -> Maybe (UT PrimValue)
        i32PrimFun f [UT (IntValue (Int32Value x))] =
          Just $ UT $ IntValue $ Int32Value $ f x
        i32PrimFun _ _ = Nothing

        i64PrimFun :: (Int64 -> Int32) -> [UT PrimValue] -> Maybe (UT PrimValue)
        i64PrimFun f [UT (IntValue (Int64Value x))] =
          Just $ UT $ IntValue $ Int32Value $ f x
        i64PrimFun _ _ = Nothing

        f32PrimFun :: (Float -> Float) -> [UT PrimValue] -> Maybe (UT PrimValue)
        f32PrimFun f [UT (FloatValue (Float32Value x))] =
          Just $ UT $ FloatValue $ Float32Value $ f x
        f32PrimFun _ _ = Nothing

        f64PrimFun :: (Double -> Double) -> [UT PrimValue] -> Maybe (UT PrimValue)
        f64PrimFun f [UT (FloatValue (Float64Value x))] =
          Just $ UT $ FloatValue $ Float64Value $ f x
        f64PrimFun _ _ = Nothing

        f32PrimFun3 :: (Float -> Float -> Float -> Float)
                    -> [UT PrimValue] -> Maybe (UT PrimValue)
        f32PrimFun3 f [UT (FloatValue (Float32Value a)),
                       UT (FloatValue (Float32Value b)),
                       UT (FloatValue (Float32Value c))] =
          Just $ UT $ FloatValue $ Float32Value $ f a b c
        f32PrimFun3 _ _ = Nothing

        f64PrimFun3 :: (Double -> Double -> Double -> Double)
                    -> [UT PrimValue] -> Maybe (UT PrimValue)
        f64PrimFun3 f [UT (FloatValue (Float64Value a)),
                       UT (FloatValue (Float64Value b)),
                       UT (FloatValue (Float64Value c))] =
          Just $ UT $ FloatValue $ Float64Value $ f a b c
        f64PrimFun3 _ _ = Nothing

-- Prettyprinting instances

instance Pretty (IntType t) where
  ppr Int8  = text "i8"
  ppr Int16 = text "i16"
  ppr Int32 = text "i32"
  ppr Int64 = text "i64"

instance Pretty (FloatType t) where
  ppr Float32 = text "f32"
  ppr Float64 = text "f64"

instance Pretty (PrimType t) where
  ppr (IntType t)   = ppr t
  ppr (FloatType t) = ppr t
  ppr Bool          = text "bool"
  ppr Cert          = text "cert"

instance Pretty (IntValue t) where
  ppr (Int8Value v)  = text $ show v ++ "i8"
  ppr (Int16Value v) = text $ show v ++ "i16"
  ppr (Int32Value v) = text $ show v ++ "i32"
  ppr (Int64Value v) = text $ show v ++ "i64"

instance Pretty (FloatValue t) where
  ppr (Float32Value v)
    | isInfinite v, v >= 0 = text "f32.inf"
    | isInfinite v, v <  0 = text "-f32.inf"
    | isNaN v = text "f32.nan"
    | otherwise = text $ show v ++ "f32"
  ppr (Float64Value v)
    | isInfinite v, v >= 0 = text "f64.inf"
    | isInfinite v, v <  0 = text "-f64.inf"
    | isNaN v = text "f64.nan"
    | otherwise = text $ show v ++ "f64"

instance Pretty (PrimValue t) where
  ppr (IntValue v)      = ppr v
  ppr (BoolValue True)  = text "true"
  ppr (BoolValue False) = text "false"
  ppr (FloatValue v)    = ppr v
  ppr Checked           = text "checked"

-- | The human-readable name for a 'ConvOp'.  This is used to expose
-- the 'ConvOp' in the @intrinsics@ module of a Futhark program.
convOpFun :: ConvOp f t -> String
convOpFun ZExt{}   = "zext"
convOpFun SExt{}   = "sext"
convOpFun FPConv{} = "fpconv"
convOpFun FPToUI{} = "fptoui"
convOpFun FPToSI{} = "fptosi"
convOpFun UIToFP{} = "uitofp"
convOpFun SIToFP{} = "sitofp"
convOpFun IToB{}   = "itob"
convOpFun BToI{}   = "btoi"

taggedI :: String -> IntType t -> Doc
taggedI s Int8  = text $ s ++ "8"
taggedI s Int16 = text $ s ++ "16"
taggedI s Int32 = text $ s ++ "32"
taggedI s Int64 = text $ s ++ "64"

taggedF :: String -> FloatType t -> Doc
taggedF s Float32 = text $ s ++ "32"
taggedF s Float64 = text $ s ++ "64"

convOp :: (Pretty from, Pretty to) => String -> from -> to -> Doc
convOp s from to = text s <> text "_" <> ppr from <> text "_" <> ppr to

-- | True if signed.  Only makes a difference for integer types.
prettySigned :: Bool -> PrimType t -> String
prettySigned True (IntType it) = 'u' : drop 1 (pretty it)
prettySigned _ t = pretty t

instance Pretty (BinOp t) where
  ppr (Add t OverflowWrap)  = taggedI "add" t
  ppr (Add t OverflowUndef) = taggedI "add_nw" t
  ppr (Sub t OverflowWrap)  = taggedI "sub" t
  ppr (Sub t OverflowUndef) = taggedI "sub_nw" t
  ppr (Mul t OverflowWrap)  = taggedI "mul" t
  ppr (Mul t OverflowUndef) = taggedI "mul_nw" t
  ppr (FAdd t)  = taggedF "fadd" t
  ppr (FSub t)  = taggedF "fsub" t
  ppr (FMul t)  = taggedF "fmul" t
  ppr (UDiv t Safe)    = taggedI "udiv_safe" t
  ppr (UDiv t Unsafe)  = taggedI "udiv" t
  ppr (UDivUp t Safe)   = taggedI "udiv_up_safe" t
  ppr (UDivUp t Unsafe) = taggedI "udiv_up" t
  ppr (UMod t Safe)    = taggedI "umod_safe" t
  ppr (UMod t Unsafe)  = taggedI "umod" t
  ppr (SDiv t Safe)    = taggedI "sdiv_safe" t
  ppr (SDiv t Unsafe)  = taggedI "sdiv" t
  ppr (SDivUp t Safe)   = taggedI "sdiv_up_safe" t
  ppr (SDivUp t Unsafe) = taggedI "sdiv_up" t
  ppr (SMod t Safe)    = taggedI "smod_safe" t
  ppr (SMod t Unsafe)  = taggedI "smod" t
  ppr (SQuot t Safe)   = taggedI "squot_safe" t
  ppr (SQuot t Unsafe) = taggedI "squot" t
  ppr (SRem t Safe)    = taggedI "srem_safe" t
  ppr (SRem t Unsafe)  = taggedI "srem" t
  ppr (FDiv t)  = taggedF "fdiv" t
  ppr (FMod t)  = taggedF "fmod" t
  ppr (SMin t)  = taggedI "smin" t
  ppr (UMin t)  = taggedI "umin" t
  ppr (FMin t)  = taggedF "fmin" t
  ppr (SMax t)  = taggedI "smax" t
  ppr (UMax t)  = taggedI "umax" t
  ppr (FMax t)  = taggedF "fmax" t
  ppr (Shl t)   = taggedI "shl" t
  ppr (LShr t)  = taggedI "lshr" t
  ppr (AShr t)  = taggedI "ashr" t
  ppr (And t)   = taggedI "and" t
  ppr (Or t)    = taggedI "or" t
  ppr (Xor t)   = taggedI "xor" t
  ppr (Pow t)   = taggedI "pow" t
  ppr (FPow t)  = taggedF "fpow" t
  ppr LogAnd    = text "logand"
  ppr LogOr     = text "logor"

instance Pretty (CmpOp t) where
  ppr (CmpEq t)  = text "eq_" <> ppr t
  ppr (CmpUlt t) = taggedI "ult" t
  ppr (CmpUle t) = taggedI "ule" t
  ppr (CmpSlt t) = taggedI "slt" t
  ppr (CmpSle t) = taggedI "sle" t
  ppr (FCmpLt t) = taggedF "lt" t
  ppr (FCmpLe t) = taggedF "le" t
  ppr CmpLlt = text "llt"
  ppr CmpLle = text "lle"

instance Pretty (ConvOp f t) where
  ppr op = convOp (convOpFun op) from to
    where (from, to) = convOpType op

instance Pretty (UnOp t) where
  ppr Not            = text "not"
  ppr (Abs t)        = taggedI "abs" t
  ppr (FAbs t)       = taggedF "fabs" t
  ppr (SSignum t)    = taggedI "ssignum" t
  ppr (USignum t)    = taggedI "usignum" t
  ppr (FSignum t)    = taggedF "fsignum" t
  ppr (Complement t) = taggedI "complement" t
