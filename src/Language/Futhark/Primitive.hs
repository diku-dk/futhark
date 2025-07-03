{-# LANGUAGE LambdaCase #-}

-- | Definitions of primitive types, the values that inhabit these
-- types, and operations on these values.  A primitive value can also
-- be called a scalar.
--
-- This module diverges from the actual Futhark language in that it
-- does not distinguish signed and unsigned types.  Further, we allow
-- a "unit" type that is only indirectly present in source Futhark in
-- the form of empty tuples.
module Language.Futhark.Primitive
  ( -- * Types
    IntType (..),
    allIntTypes,
    FloatType (..),
    allFloatTypes,
    PrimType (..),
    allPrimTypes,
    module Data.Int,
    module Data.Word,
    Half,

    -- * Values
    IntValue (..),
    intValue,
    intValueType,
    valueIntegral,
    FloatValue (..),
    floatValue,
    floatValueType,
    PrimValue (..),
    primValueType,
    blankPrimValue,
    onePrimValue,

    -- * Operations
    Overflow (..),
    Safety (..),
    UnOp (..),
    allUnOps,
    BinOp (..),
    allBinOps,
    ConvOp (..),
    allConvOps,
    CmpOp (..),
    allCmpOps,

    -- ** Unary Operations
    doUnOp,
    doComplement,
    doAbs,
    doFAbs,
    doSSignum,
    doUSignum,

    -- ** Binary Operations
    doBinOp,
    doAdd,
    doMul,
    doSDiv,
    doSMod,
    doPow,

    -- ** Conversion Operations
    doConvOp,
    doZExt,
    doSExt,
    doFPConv,
    doFPToUI,
    doFPToSI,
    doUIToFP,
    doSIToFP,
    intToInt64,
    intToWord64,
    flipConvOp,

    -- * Comparison Operations
    doCmpOp,
    doCmpEq,
    doCmpUlt,
    doCmpUle,
    doCmpSlt,
    doCmpSle,
    doFCmpLt,
    doFCmpLe,

    -- * Type Of
    binOpType,
    unOpType,
    cmpOpType,
    convOpType,

    -- * Primitive functions
    primFuns,
    condFun,
    isCondFun,

    -- * Utility
    zeroIsh,
    zeroIshInt,
    oneIsh,
    oneIshInt,
    negativeIsh,
    primBitSize,
    primByteSize,
    intByteSize,
    floatByteSize,
    commutativeBinOp,
    associativeBinOp,

    -- * Prettyprinting
    convOpFun,
    prettySigned,
  )
where

import Control.Category
import Data.Binary.Get qualified as G
import Data.Binary.Put qualified as P
import Data.Bits
  ( complement,
    countLeadingZeros,
    countTrailingZeros,
    popCount,
    shift,
    shiftR,
    xor,
    (.&.),
    (.|.),
  )
import Data.Fixed (mod') -- Weird location.
import Data.Int (Int16, Int32, Int64, Int8)
import Data.List qualified as L
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.C.Types (CUShort (..))
import Futhark.Util (convFloat)
import Futhark.Util.CMath
import Futhark.Util.Pretty
import Numeric (log1p)
import Numeric.Half
import Prelude hiding (id, (.))

-- | An integer type, ordered by size.  Note that signedness is not a
-- property of the type, but a property of the operations performed on
-- values of these types.
data IntType
  = Int8
  | Int16
  | Int32
  | Int64
  deriving (Eq, Ord, Show, Enum, Bounded)

instance Pretty IntType where
  pretty Int8 = "i8"
  pretty Int16 = "i16"
  pretty Int32 = "i32"
  pretty Int64 = "i64"

-- | A list of all integer types.
allIntTypes :: [IntType]
allIntTypes = [minBound .. maxBound]

-- | A floating point type.
data FloatType
  = Float16
  | Float32
  | Float64
  deriving (Eq, Ord, Show, Enum, Bounded)

instance Pretty FloatType where
  pretty Float16 = "f16"
  pretty Float32 = "f32"
  pretty Float64 = "f64"

-- | A list of all floating-point types.
allFloatTypes :: [FloatType]
allFloatTypes = [minBound .. maxBound]

-- | Low-level primitive types.
data PrimType
  = IntType IntType
  | FloatType FloatType
  | Bool
  | -- | An informationless type - An array of this type takes up no space.
    Unit
  deriving (Eq, Ord, Show)

instance Enum PrimType where
  toEnum 0 = IntType Int8
  toEnum 1 = IntType Int16
  toEnum 2 = IntType Int32
  toEnum 3 = IntType Int64
  toEnum 4 = FloatType Float16
  toEnum 5 = FloatType Float32
  toEnum 6 = FloatType Float64
  toEnum 7 = Bool
  toEnum _ = Unit

  fromEnum (IntType Int8) = 0
  fromEnum (IntType Int16) = 1
  fromEnum (IntType Int32) = 2
  fromEnum (IntType Int64) = 3
  fromEnum (FloatType Float16) = 4
  fromEnum (FloatType Float32) = 5
  fromEnum (FloatType Float64) = 6
  fromEnum Bool = 7
  fromEnum Unit = 8

instance Bounded PrimType where
  minBound = IntType Int8
  maxBound = Unit

instance Pretty PrimType where
  pretty (IntType t) = pretty t
  pretty (FloatType t) = pretty t
  pretty Bool = "bool"
  pretty Unit = "unit"

-- | A list of all primitive types.
allPrimTypes :: [PrimType]
allPrimTypes =
  map IntType allIntTypes
    ++ map FloatType allFloatTypes
    ++ [Bool, Unit]

-- | An integer value.
data IntValue
  = Int8Value !Int8
  | Int16Value !Int16
  | Int32Value !Int32
  | Int64Value !Int64
  deriving (Eq, Ord, Show)

instance Pretty IntValue where
  pretty (Int8Value v) = pretty $ show v ++ "i8"
  pretty (Int16Value v) = pretty $ show v ++ "i16"
  pretty (Int32Value v) = pretty $ show v ++ "i32"
  pretty (Int64Value v) = pretty $ show v ++ "i64"

-- | Create an t'IntValue' from a type and an 'Integer'.
intValue :: (Integral int) => IntType -> int -> IntValue
intValue Int8 = Int8Value . fromIntegral
intValue Int16 = Int16Value . fromIntegral
intValue Int32 = Int32Value . fromIntegral
intValue Int64 = Int64Value . fromIntegral

-- | The type of an integer value.
intValueType :: IntValue -> IntType
intValueType Int8Value {} = Int8
intValueType Int16Value {} = Int16
intValueType Int32Value {} = Int32
intValueType Int64Value {} = Int64

-- | Convert an t'IntValue' to any 'Integral' type.
valueIntegral :: (Integral int) => IntValue -> int
valueIntegral (Int8Value v) = fromIntegral v
valueIntegral (Int16Value v) = fromIntegral v
valueIntegral (Int32Value v) = fromIntegral v
valueIntegral (Int64Value v) = fromIntegral v

-- | A floating-point value.
data FloatValue
  = Float16Value !Half
  | Float32Value !Float
  | Float64Value !Double
  deriving (Show)

instance Eq FloatValue where
  Float16Value x == Float16Value y = isNaN x && isNaN y || x == y
  Float32Value x == Float32Value y = isNaN x && isNaN y || x == y
  Float64Value x == Float64Value y = isNaN x && isNaN y || x == y
  _ == _ = False

-- The derived Ord instance does not handle NaNs correctly.
instance Ord FloatValue where
  Float16Value x <= Float16Value y = x <= y
  Float32Value x <= Float32Value y = x <= y
  Float64Value x <= Float64Value y = x <= y
  Float16Value _ <= Float32Value _ = True
  Float16Value _ <= Float64Value _ = True
  Float32Value _ <= Float16Value _ = False
  Float32Value _ <= Float64Value _ = True
  Float64Value _ <= Float16Value _ = False
  Float64Value _ <= Float32Value _ = False

  Float16Value x < Float16Value y = x < y
  Float32Value x < Float32Value y = x < y
  Float64Value x < Float64Value y = x < y
  Float16Value _ < Float32Value _ = True
  Float16Value _ < Float64Value _ = True
  Float32Value _ < Float16Value _ = False
  Float32Value _ < Float64Value _ = True
  Float64Value _ < Float16Value _ = False
  Float64Value _ < Float32Value _ = False

  (>) = flip (<)
  (>=) = flip (<=)

instance Pretty FloatValue where
  pretty (Float16Value v)
    | isInfinite v, v >= 0 = "f16.inf"
    | isInfinite v, v < 0 = "-f16.inf"
    | isNaN v = "f16.nan"
    | otherwise = pretty $ show v ++ "f16"
  pretty (Float32Value v)
    | isInfinite v, v >= 0 = "f32.inf"
    | isInfinite v, v < 0 = "-f32.inf"
    | isNaN v = "f32.nan"
    | otherwise = pretty $ show v ++ "f32"
  pretty (Float64Value v)
    | isInfinite v, v >= 0 = "f64.inf"
    | isInfinite v, v < 0 = "-f64.inf"
    | isNaN v = "f64.nan"
    | otherwise = pretty $ show v ++ "f64"

-- | Create a t'FloatValue' from a type and a 'Rational'.
floatValue :: (Real num) => FloatType -> num -> FloatValue
floatValue Float16 = Float16Value . fromRational . toRational
floatValue Float32 = Float32Value . fromRational . toRational
floatValue Float64 = Float64Value . fromRational . toRational

-- | The type of a floating-point value.
floatValueType :: FloatValue -> FloatType
floatValueType Float16Value {} = Float16
floatValueType Float32Value {} = Float32
floatValueType Float64Value {} = Float64

-- | Non-array values.
data PrimValue
  = IntValue !IntValue
  | FloatValue !FloatValue
  | BoolValue !Bool
  | -- | The only value of type 'Unit'.
    UnitValue
  deriving (Eq, Ord, Show)

instance Pretty PrimValue where
  pretty (IntValue v) = pretty v
  pretty (BoolValue True) = "true"
  pretty (BoolValue False) = "false"
  pretty (FloatValue v) = pretty v
  pretty UnitValue = "()"

-- | The type of a basic value.
primValueType :: PrimValue -> PrimType
primValueType (IntValue v) = IntType $ intValueType v
primValueType (FloatValue v) = FloatType $ floatValueType v
primValueType BoolValue {} = Bool
primValueType UnitValue = Unit

-- | A "blank" value of the given primitive type - this is zero, or
-- whatever is close to it.  Don't depend on this value, but use it
-- for e.g. creating arrays to be populated by do-loops.
blankPrimValue :: PrimType -> PrimValue
blankPrimValue (IntType Int8) = IntValue $ Int8Value 0
blankPrimValue (IntType Int16) = IntValue $ Int16Value 0
blankPrimValue (IntType Int32) = IntValue $ Int32Value 0
blankPrimValue (IntType Int64) = IntValue $ Int64Value 0
blankPrimValue (FloatType Float16) = FloatValue $ Float16Value 0.0
blankPrimValue (FloatType Float32) = FloatValue $ Float32Value 0.0
blankPrimValue (FloatType Float64) = FloatValue $ Float64Value 0.0
blankPrimValue Bool = BoolValue False
blankPrimValue Unit = UnitValue

-- | A one value of the given primitive type - this is one
-- whatever is close to it.
onePrimValue :: PrimType -> PrimValue
onePrimValue (IntType Int8) = IntValue $ Int8Value 1
onePrimValue (IntType Int16) = IntValue $ Int16Value 1
onePrimValue (IntType Int32) = IntValue $ Int32Value 1
onePrimValue (IntType Int64) = IntValue $ Int64Value 1
onePrimValue (FloatType Float16) = FloatValue $ Float16Value 1.0
onePrimValue (FloatType Float32) = FloatValue $ Float32Value 1.0
onePrimValue (FloatType Float64) = FloatValue $ Float64Value 1.0
onePrimValue Bool = BoolValue True
onePrimValue Unit = UnitValue

-- | Various unary operators.  It is a bit ad-hoc what is a unary
-- operator and what is a built-in function.  Perhaps these should all
-- go away eventually.
data UnOp
  = -- | Flip sign. Logical negation for booleans.
    Neg PrimType
  | -- | E.g., @~(~1) = 1@.
    Complement IntType
  | -- | @abs(-2) = 2@.
    Abs IntType
  | -- | @fabs(-2.0) = 2.0@.
    FAbs FloatType
  | -- | Signed sign function: @ssignum(-2)@ = -1.
    SSignum IntType
  | -- | Unsigned sign function: @usignum(2)@ = 1.
    USignum IntType
  | -- | Floating-point sign function.
    FSignum FloatType
  deriving (Eq, Ord, Show)

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

-- | Binary operators.  These correspond closely to the binary operators in
-- LLVM.  Most are parametrised by their expected input and output
-- types.
data BinOp
  = -- | Integer addition.
    Add IntType Overflow
  | -- | Floating-point addition.
    FAdd FloatType
  | -- | Integer subtraction.
    Sub IntType Overflow
  | -- | Floating-point subtraction.
    FSub FloatType
  | -- | Integer multiplication.
    Mul IntType Overflow
  | -- | Floating-point multiplication.
    FMul FloatType
  | -- | Unsigned integer division.  Rounds towards
    -- negativity infinity.  Note: this is different
    -- from LLVM.
    UDiv IntType Safety
  | -- | Unsigned integer division.  Rounds towards positive
    -- infinity.
    UDivUp IntType Safety
  | -- | Signed integer division.  Rounds towards
    -- negativity infinity.  Note: this is different
    -- from LLVM.
    SDiv IntType Safety
  | -- | Signed integer division.  Rounds towards positive
    -- infinity.
    SDivUp IntType Safety
  | -- | Floating-point division.
    FDiv FloatType
  | -- | Floating-point modulus.
    FMod FloatType
  | -- | Unsigned integer modulus; the countepart to 'UDiv'.
    UMod IntType Safety
  | -- | Signed integer modulus; the countepart to 'SDiv'.
    SMod IntType Safety
  | -- | Signed integer division.  Rounds towards zero.  This
    -- corresponds to the @sdiv@ instruction in LLVM and
    -- integer division in C.
    SQuot IntType Safety
  | -- | Signed integer division.  Rounds towards zero.  This
    -- corresponds to the @srem@ instruction in LLVM and
    -- integer modulo in C.
    SRem IntType Safety
  | -- | Returns the smallest of two signed integers.
    SMin IntType
  | -- | Returns the smallest of two unsigned integers.
    UMin IntType
  | -- | Returns the smallest of two floating-point numbers.
    FMin FloatType
  | -- | Returns the greatest of two signed integers.
    SMax IntType
  | -- | Returns the greatest of two unsigned integers.
    UMax IntType
  | -- | Returns the greatest of two floating-point numbers.
    FMax FloatType
  | -- | Left-shift.
    Shl IntType
  | -- | Logical right-shift, zero-extended.
    LShr IntType
  | -- | Arithmetic right-shift, sign-extended.
    AShr IntType
  | -- | Bitwise and.
    And IntType
  | -- | Bitwise or.
    Or IntType
  | -- | Bitwise exclusive-or.
    Xor IntType
  | -- | Integer exponentiation.
    Pow IntType
  | -- | Floating-point exponentiation.
    FPow FloatType
  | -- | Boolean and - not short-circuiting.
    LogAnd
  | -- | Boolean or - not short-circuiting.
    LogOr
  deriving (Eq, Ord, Show)

-- | Comparison operators are like 'BinOp's, but they always return a
-- boolean value.  The somewhat ugly constructor names are straight
-- out of LLVM.
data CmpOp
  = -- | All types equality.
    CmpEq PrimType
  | -- | Unsigned less than.
    CmpUlt IntType
  | -- | Unsigned less than or equal.
    CmpUle IntType
  | -- | Signed less than.
    CmpSlt IntType
  | -- | Signed less than or equal.
    CmpSle IntType
  | -- Comparison operators for floating-point values.  TODO: extend
    -- this to handle NaNs and such, like the LLVM fcmp instruction.

    -- | Floating-point less than.
    FCmpLt FloatType
  | -- | Floating-point less than or equal.
    FCmpLe FloatType
  | -- Boolean comparison.

    -- | Boolean less than.
    CmpLlt
  | -- | Boolean less than or equal.
    CmpLle
  deriving (Eq, Ord, Show)

-- | Conversion operators try to generalise the @from t0 x to t1@
-- instructions from LLVM.
data ConvOp
  = -- | Zero-extend the former integer type to the latter.
    -- If the new type is smaller, the result is a
    -- truncation.
    ZExt IntType IntType
  | -- | Sign-extend the former integer type to the latter.
    -- If the new type is smaller, the result is a
    -- truncation.
    SExt IntType IntType
  | -- | Convert value of the former floating-point type to
    -- the latter.  If the new type is smaller, the result
    -- is a truncation.
    FPConv FloatType FloatType
  | -- | Convert a floating-point value to the nearest
    -- unsigned integer (rounding towards zero).
    FPToUI FloatType IntType
  | -- | Convert a floating-point value to the nearest
    -- signed integer (rounding towards zero).
    FPToSI FloatType IntType
  | -- | Convert an unsigned integer to a floating-point value.
    UIToFP IntType FloatType
  | -- | Convert a signed integer to a floating-point value.
    SIToFP IntType FloatType
  | -- | Convert floating point number to its bitwise representation.
    FPToBits FloatType
  | -- | Convert bitwise representation to a floating point number.
    BitsToFP FloatType
  | -- | Convert an integer to a boolean value.  Zero
    -- becomes false; anything else is true.
    IToB IntType
  | -- | Convert a boolean to an integer.  True is converted
    -- to 1 and False to 0.
    BToI IntType
  | -- | Convert a float to a boolean value.  Zero becomes false;
    -- | anything else is true.
    FToB FloatType
  | -- | Convert a boolean to a float.  True is converted
    -- to 1 and False to 0.
    BToF FloatType
  deriving (Eq, Ord, Show)

-- | A list of all unary operators for all types.
allUnOps :: [UnOp]
allUnOps =
  map Neg [minBound .. maxBound]
    ++ map Complement [minBound .. maxBound]
    ++ map Abs [minBound .. maxBound]
    ++ map FAbs [minBound .. maxBound]
    ++ map SSignum [minBound .. maxBound]
    ++ map USignum [minBound .. maxBound]
    ++ map FSignum [minBound .. maxBound]

-- | A list of all binary operators for all types.
allBinOps :: [BinOp]
allBinOps =
  concat
    [ Add <$> allIntTypes <*> [OverflowWrap, OverflowUndef],
      map FAdd allFloatTypes,
      Sub <$> allIntTypes <*> [OverflowWrap, OverflowUndef],
      map FSub allFloatTypes,
      Mul <$> allIntTypes <*> [OverflowWrap, OverflowUndef],
      map FMul allFloatTypes,
      UDiv <$> allIntTypes <*> [Unsafe, Safe],
      UDivUp <$> allIntTypes <*> [Unsafe, Safe],
      SDiv <$> allIntTypes <*> [Unsafe, Safe],
      SDivUp <$> allIntTypes <*> [Unsafe, Safe],
      map FDiv allFloatTypes,
      map FMod allFloatTypes,
      UMod <$> allIntTypes <*> [Unsafe, Safe],
      SMod <$> allIntTypes <*> [Unsafe, Safe],
      SQuot <$> allIntTypes <*> [Unsafe, Safe],
      SRem <$> allIntTypes <*> [Unsafe, Safe],
      map SMin allIntTypes,
      map UMin allIntTypes,
      map FMin allFloatTypes,
      map SMax allIntTypes,
      map UMax allIntTypes,
      map FMax allFloatTypes,
      map Shl allIntTypes,
      map LShr allIntTypes,
      map AShr allIntTypes,
      map And allIntTypes,
      map Or allIntTypes,
      map Xor allIntTypes,
      map Pow allIntTypes,
      map FPow allFloatTypes,
      [LogAnd, LogOr]
    ]

-- | A list of all comparison operators for all types.
allCmpOps :: [CmpOp]
allCmpOps =
  concat
    [ map CmpEq allPrimTypes,
      map CmpUlt allIntTypes,
      map CmpUle allIntTypes,
      map CmpSlt allIntTypes,
      map CmpSle allIntTypes,
      map FCmpLt allFloatTypes,
      map FCmpLe allFloatTypes,
      [CmpLlt, CmpLle]
    ]

-- | A list of all conversion operators for all types.
allConvOps :: [ConvOp]
allConvOps =
  concat
    [ ZExt <$> allIntTypes <*> allIntTypes,
      SExt <$> allIntTypes <*> allIntTypes,
      FPConv <$> allFloatTypes <*> allFloatTypes,
      FPToUI <$> allFloatTypes <*> allIntTypes,
      FPToSI <$> allFloatTypes <*> allIntTypes,
      UIToFP <$> allIntTypes <*> allFloatTypes,
      SIToFP <$> allIntTypes <*> allFloatTypes,
      FPToBits <$> allFloatTypes,
      BitsToFP <$> allFloatTypes,
      IToB <$> allIntTypes,
      BToI <$> allIntTypes,
      FToB <$> allFloatTypes,
      BToF <$> allFloatTypes
    ]

-- | Apply an 'UnOp' to an operand.  Returns 'Nothing' if the
-- application is mistyped.
doUnOp :: UnOp -> PrimValue -> Maybe PrimValue
doUnOp (Neg _) (BoolValue b) = Just $ BoolValue $ not b
doUnOp (Neg _) (FloatValue v) = Just $ FloatValue $ doFNeg v
doUnOp (Neg _) (IntValue v) = Just $ IntValue $ doIntNeg v
doUnOp Complement {} (IntValue v) = Just $ IntValue $ doComplement v
doUnOp Abs {} (IntValue v) = Just $ IntValue $ doAbs v
doUnOp FAbs {} (FloatValue v) = Just $ FloatValue $ doFAbs v
doUnOp SSignum {} (IntValue v) = Just $ IntValue $ doSSignum v
doUnOp USignum {} (IntValue v) = Just $ IntValue $ doUSignum v
doUnOp FSignum {} (FloatValue v) = Just $ FloatValue $ doFSignum v
doUnOp _ _ = Nothing

doFNeg :: FloatValue -> FloatValue
doFNeg (Float16Value x) = Float16Value $ negate x
doFNeg (Float32Value x) = Float32Value $ negate x
doFNeg (Float64Value x) = Float64Value $ negate x

doIntNeg :: IntValue -> IntValue
doIntNeg (Int8Value x) = Int8Value $ -x
doIntNeg (Int16Value x) = Int16Value $ -x
doIntNeg (Int32Value x) = Int32Value $ -x
doIntNeg (Int64Value x) = Int64Value $ -x

-- | E.g., @~(~1) = 1@.
doComplement :: IntValue -> IntValue
doComplement v = intValue (intValueType v) $ complement $ intToInt64 v

-- | @abs(-2) = 2@.
doAbs :: IntValue -> IntValue
doAbs v = intValue (intValueType v) $ abs $ intToInt64 v

-- | @abs(-2.0) = 2.0@.
doFAbs :: FloatValue -> FloatValue
doFAbs (Float16Value x) = Float16Value $ abs x
doFAbs (Float32Value x) = Float32Value $ abs x
doFAbs (Float64Value x) = Float64Value $ abs x

-- | @ssignum(-2)@ = -1.
doSSignum :: IntValue -> IntValue
doSSignum v = intValue (intValueType v) $ signum $ intToInt64 v

-- | @usignum(-2)@ = -1.
doUSignum :: IntValue -> IntValue
doUSignum v = intValue (intValueType v) $ signum $ intToWord64 v

-- | @fsignum(-2.0)@ = -1.0.
doFSignum :: FloatValue -> FloatValue
doFSignum (Float16Value v) = Float16Value $ signum v
doFSignum (Float32Value v) = Float32Value $ signum v
doFSignum (Float64Value v) = Float64Value $ signum v

-- | Apply a 'BinOp' to an operand.  Returns 'Nothing' if the
-- application is mistyped, or outside the domain (e.g. division by
-- zero).
doBinOp :: BinOp -> PrimValue -> PrimValue -> Maybe PrimValue
doBinOp Add {} = doIntBinOp doAdd
doBinOp FAdd {} = doFloatBinOp (+) (+) (+)
doBinOp Sub {} = doIntBinOp doSub
doBinOp FSub {} = doFloatBinOp (-) (-) (-)
doBinOp Mul {} = doIntBinOp doMul
doBinOp FMul {} = doFloatBinOp (*) (*) (*)
doBinOp UDiv {} = doRiskyIntBinOp doUDiv
doBinOp UDivUp {} = doRiskyIntBinOp doUDivUp
doBinOp SDiv {} = doRiskyIntBinOp doSDiv
doBinOp SDivUp {} = doRiskyIntBinOp doSDivUp
doBinOp FDiv {} = doFloatBinOp (/) (/) (/)
doBinOp FMod {} = doFloatBinOp mod' mod' mod'
doBinOp UMod {} = doRiskyIntBinOp doUMod
doBinOp SMod {} = doRiskyIntBinOp doSMod
doBinOp SQuot {} = doRiskyIntBinOp doSQuot
doBinOp SRem {} = doRiskyIntBinOp doSRem
doBinOp SMin {} = doIntBinOp doSMin
doBinOp UMin {} = doIntBinOp doUMin
doBinOp FMin {} = doFloatBinOp fmin fmin fmin
  where
    fmin x y
      | isNaN x = y
      | isNaN y = x
      | otherwise = min x y
doBinOp SMax {} = doIntBinOp doSMax
doBinOp UMax {} = doIntBinOp doUMax
doBinOp FMax {} = doFloatBinOp fmax fmax fmax
  where
    fmax x y
      | isNaN x = y
      | isNaN y = x
      | otherwise = max x y
doBinOp Shl {} = doIntBinOp doShl
doBinOp LShr {} = doIntBinOp doLShr
doBinOp AShr {} = doIntBinOp doAShr
doBinOp And {} = doIntBinOp doAnd
doBinOp Or {} = doIntBinOp doOr
doBinOp Xor {} = doIntBinOp doXor
doBinOp Pow {} = doRiskyIntBinOp doPow
doBinOp FPow {} = doFloatBinOp (**) (**) (**)
doBinOp LogAnd {} = doBoolBinOp (&&)
doBinOp LogOr {} = doBoolBinOp (||)

doIntBinOp ::
  (IntValue -> IntValue -> IntValue) ->
  PrimValue ->
  PrimValue ->
  Maybe PrimValue
doIntBinOp f (IntValue v1) (IntValue v2) =
  Just $ IntValue $ f v1 v2
doIntBinOp _ _ _ = Nothing

doRiskyIntBinOp ::
  (IntValue -> IntValue -> Maybe IntValue) ->
  PrimValue ->
  PrimValue ->
  Maybe PrimValue
doRiskyIntBinOp f (IntValue v1) (IntValue v2) =
  IntValue <$> f v1 v2
doRiskyIntBinOp _ _ _ = Nothing

doFloatBinOp ::
  (Half -> Half -> Half) ->
  (Float -> Float -> Float) ->
  (Double -> Double -> Double) ->
  PrimValue ->
  PrimValue ->
  Maybe PrimValue
doFloatBinOp f16 _ _ (FloatValue (Float16Value v1)) (FloatValue (Float16Value v2)) =
  Just $ FloatValue $ Float16Value $ f16 v1 v2
doFloatBinOp _ f32 _ (FloatValue (Float32Value v1)) (FloatValue (Float32Value v2)) =
  Just $ FloatValue $ Float32Value $ f32 v1 v2
doFloatBinOp _ _ f64 (FloatValue (Float64Value v1)) (FloatValue (Float64Value v2)) =
  Just $ FloatValue $ Float64Value $ f64 v1 v2
doFloatBinOp _ _ _ _ _ = Nothing

doBoolBinOp ::
  (Bool -> Bool -> Bool) ->
  PrimValue ->
  PrimValue ->
  Maybe PrimValue
doBoolBinOp f (BoolValue v1) (BoolValue v2) =
  Just $ BoolValue $ f v1 v2
doBoolBinOp _ _ _ = Nothing

-- | Integer addition.
doAdd :: IntValue -> IntValue -> IntValue
doAdd v1 v2 = intValue (intValueType v1) $ intToInt64 v1 + intToInt64 v2

-- | Integer subtraction.
doSub :: IntValue -> IntValue -> IntValue
doSub v1 v2 = intValue (intValueType v1) $ intToInt64 v1 - intToInt64 v2

-- | Integer multiplication.
doMul :: IntValue -> IntValue -> IntValue
doMul v1 v2 = intValue (intValueType v1) $ intToInt64 v1 * intToInt64 v2

-- | Unsigned integer division.  Rounds towards negativity infinity.
-- Note: this is different from LLVM.
doUDiv :: IntValue -> IntValue -> Maybe IntValue
doUDiv v1 v2
  | zeroIshInt v2 = Nothing
  | otherwise =
      Just . intValue (intValueType v1) $
        intToWord64 v1 `div` intToWord64 v2

-- | Unsigned integer division.  Rounds towards positive infinity.
doUDivUp :: IntValue -> IntValue -> Maybe IntValue
doUDivUp v1 v2
  | zeroIshInt v2 = Nothing
  | otherwise =
      Just . intValue (intValueType v1) $
        (intToWord64 v1 + intToWord64 v2 - 1) `div` intToWord64 v2

-- | Signed integer division.  Rounds towards negativity infinity.
-- Note: this is different from LLVM.
doSDiv :: IntValue -> IntValue -> Maybe IntValue
doSDiv v1 v2
  | zeroIshInt v2 = Nothing
  | otherwise =
      Just $
        intValue (intValueType v1) $
          intToInt64 v1 `div` intToInt64 v2

-- | Signed integer division.  Rounds towards positive infinity.
doSDivUp :: IntValue -> IntValue -> Maybe IntValue
doSDivUp v1 v2
  | zeroIshInt v2 = Nothing
  | otherwise =
      Just . intValue (intValueType v1) $
        (intToInt64 v1 + intToInt64 v2 - 1) `div` intToInt64 v2

-- | Unsigned integer modulus; the countepart to 'UDiv'.
doUMod :: IntValue -> IntValue -> Maybe IntValue
doUMod v1 v2
  | zeroIshInt v2 = Nothing
  | otherwise = Just $ intValue (intValueType v1) $ intToWord64 v1 `mod` intToWord64 v2

-- | Signed integer modulus; the countepart to 'SDiv'.
doSMod :: IntValue -> IntValue -> Maybe IntValue
doSMod v1 v2
  | zeroIshInt v2 = Nothing
  | otherwise = Just $ intValue (intValueType v1) $ intToInt64 v1 `mod` intToInt64 v2

-- | Signed integer division.  Rounds towards zero.
-- This corresponds to the @sdiv@ instruction in LLVM.
doSQuot :: IntValue -> IntValue -> Maybe IntValue
doSQuot v1 v2
  | zeroIshInt v2 = Nothing
  | otherwise = Just $ intValue (intValueType v1) $ intToInt64 v1 `quot` intToInt64 v2

-- | Signed integer division.  Rounds towards zero.
-- This corresponds to the @srem@ instruction in LLVM.
doSRem :: IntValue -> IntValue -> Maybe IntValue
doSRem v1 v2
  | zeroIshInt v2 = Nothing
  | otherwise = Just $ intValue (intValueType v1) $ intToInt64 v1 `rem` intToInt64 v2

-- | Minimum of two signed integers.
doSMin :: IntValue -> IntValue -> IntValue
doSMin v1 v2 = intValue (intValueType v1) $ intToInt64 v1 `min` intToInt64 v2

-- | Minimum of two unsigned integers.
doUMin :: IntValue -> IntValue -> IntValue
doUMin v1 v2 = intValue (intValueType v1) $ intToWord64 v1 `min` intToWord64 v2

-- | Maximum of two signed integers.
doSMax :: IntValue -> IntValue -> IntValue
doSMax v1 v2 = intValue (intValueType v1) $ intToInt64 v1 `max` intToInt64 v2

-- | Maximum of two unsigned integers.
doUMax :: IntValue -> IntValue -> IntValue
doUMax v1 v2 = intValue (intValueType v1) $ intToWord64 v1 `max` intToWord64 v2

-- | Left-shift.
doShl :: IntValue -> IntValue -> IntValue
doShl v1 v2 = intValue (intValueType v1) $ intToInt64 v1 `shift` intToInt v2

-- | Logical right-shift, zero-extended.
doLShr :: IntValue -> IntValue -> IntValue
doLShr v1 v2 = intValue (intValueType v1) $ intToWord64 v1 `shift` negate (intToInt v2)

-- | Arithmetic right-shift, sign-extended.
doAShr :: IntValue -> IntValue -> IntValue
doAShr v1 v2 = intValue (intValueType v1) $ intToInt64 v1 `shift` negate (intToInt v2)

-- | Bitwise and.
doAnd :: IntValue -> IntValue -> IntValue
doAnd v1 v2 = intValue (intValueType v1) $ intToWord64 v1 .&. intToWord64 v2

-- | Bitwise or.
doOr :: IntValue -> IntValue -> IntValue
doOr v1 v2 = intValue (intValueType v1) $ intToWord64 v1 .|. intToWord64 v2

-- | Bitwise exclusive-or.
doXor :: IntValue -> IntValue -> IntValue
doXor v1 v2 = intValue (intValueType v1) $ intToWord64 v1 `xor` intToWord64 v2

-- | Signed integer exponentatation.
doPow :: IntValue -> IntValue -> Maybe IntValue
doPow v1 v2
  | negativeIshInt v2 = Nothing
  | otherwise = Just $ intValue (intValueType v1) $ intToInt64 v1 ^ intToInt64 v2

-- | Apply a 'ConvOp' to an operand.  Returns 'Nothing' if the
-- application is mistyped.
doConvOp :: ConvOp -> PrimValue -> Maybe PrimValue
doConvOp (ZExt _ to) (IntValue v) = Just $ IntValue $ doZExt v to
doConvOp (SExt _ to) (IntValue v) = Just $ IntValue $ doSExt v to
doConvOp (FPConv _ to) (FloatValue v) = Just $ FloatValue $ doFPConv v to
doConvOp (FPToUI _ to) (FloatValue v) = Just $ IntValue $ doFPToUI v to
doConvOp (FPToSI _ to) (FloatValue v) = Just $ IntValue $ doFPToSI v to
doConvOp (UIToFP _ to) (IntValue v) = Just $ FloatValue $ doUIToFP v to
doConvOp (SIToFP _ to) (IntValue v) = Just $ FloatValue $ doSIToFP v to
doConvOp FPToBits {} (FloatValue (Float16Value x)) =
  Just $ IntValue $ Int16Value $ fromIntegral $ halfToWord x
doConvOp FPToBits {} (FloatValue (Float32Value x)) =
  Just $ IntValue $ Int32Value $ fromIntegral $ floatToWord x
doConvOp FPToBits {} (FloatValue (Float64Value x)) =
  Just $ IntValue $ Int64Value $ fromIntegral $ doubleToWord x
doConvOp BitsToFP {} (IntValue (Int16Value x)) =
  Just $ FloatValue $ Float16Value $ wordToHalf $ fromIntegral x
doConvOp BitsToFP {} (IntValue (Int32Value x)) =
  Just $ FloatValue $ Float32Value $ wordToFloat $ fromIntegral x
doConvOp BitsToFP {} (IntValue (Int64Value x)) =
  Just $ FloatValue $ Float64Value $ wordToDouble $ fromIntegral x
doConvOp (IToB _) (IntValue v) = Just $ BoolValue $ intToInt64 v /= 0
doConvOp (BToI to) (BoolValue v) = Just $ IntValue $ intValue to $ if v then 1 else 0 :: Int
doConvOp (FToB _) (FloatValue v) = Just $ BoolValue $ floatToDouble v /= 0
doConvOp (BToF to) (BoolValue v) = Just $ FloatValue $ floatValue to $ if v then 1 else 0 :: Double
doConvOp _ _ = Nothing

-- | The integer type with the same size as the given floating point type.
matchingInt :: FloatType -> IntType
matchingInt Float16 = Int16
matchingInt Float32 = Int32
matchingInt Float64 = Int64

-- | Turn the conversion the other way around.  Note that most
-- conversions are lossy, so there is no guarantee the value will
-- round-trip.
flipConvOp :: ConvOp -> ConvOp
flipConvOp (ZExt from to) = ZExt to from
flipConvOp (SExt from to) = SExt to from
flipConvOp (FPConv from to) = FPConv to from
flipConvOp (FPToUI from to) = UIToFP to from
flipConvOp (FPToSI from to) = SIToFP to from
flipConvOp (UIToFP from to) = FPToSI to from
flipConvOp (SIToFP from to) = FPToSI to from
flipConvOp (FPToBits from) = BitsToFP from
flipConvOp (BitsToFP to) = FPToBits to
flipConvOp (IToB from) = BToI from
flipConvOp (BToI to) = IToB to
flipConvOp (FToB from) = BToF from
flipConvOp (BToF to) = FToB to

-- | Zero-extend the given integer value to the size of the given
-- type.  If the type is smaller than the given value, the result is a
-- truncation.
doZExt :: IntValue -> IntType -> IntValue
doZExt (Int8Value x) t = intValue t $ toInteger (fromIntegral x :: Word8)
doZExt (Int16Value x) t = intValue t $ toInteger (fromIntegral x :: Word16)
doZExt (Int32Value x) t = intValue t $ toInteger (fromIntegral x :: Word32)
doZExt (Int64Value x) t = intValue t $ toInteger (fromIntegral x :: Word64)

-- | Sign-extend the given integer value to the size of the given
-- type.  If the type is smaller than the given value, the result is a
-- truncation.
doSExt :: IntValue -> IntType -> IntValue
doSExt (Int8Value x) t = intValue t $ toInteger x
doSExt (Int16Value x) t = intValue t $ toInteger x
doSExt (Int32Value x) t = intValue t $ toInteger x
doSExt (Int64Value x) t = intValue t $ toInteger x

-- | Convert the former floating-point type to the latter.
doFPConv :: FloatValue -> FloatType -> FloatValue
doFPConv v Float16 = Float16Value $ floatToHalf v
doFPConv v Float32 = Float32Value $ floatToFloat v
doFPConv v Float64 = Float64Value $ floatToDouble v

-- | Convert a floating-point value to the nearest
-- unsigned integer (rounding towards zero).
doFPToUI :: FloatValue -> IntType -> IntValue
doFPToUI v t = intValue t (truncate $ floatToDouble v :: Word64)

-- | Convert a floating-point value to the nearest
-- signed integer (rounding towards zero).
doFPToSI :: FloatValue -> IntType -> IntValue
doFPToSI v t = intValue t (truncate $ floatToDouble v :: Word64)

-- | Convert an unsigned integer to a floating-point value.
doUIToFP :: IntValue -> FloatType -> FloatValue
doUIToFP v t = floatValue t $ intToWord64 v

-- | Convert a signed integer to a floating-point value.
doSIToFP :: IntValue -> FloatType -> FloatValue
doSIToFP v t = floatValue t $ intToInt64 v

-- | Apply a 'CmpOp' to an operand.  Returns 'Nothing' if the
-- application is mistyped.
doCmpOp :: CmpOp -> PrimValue -> PrimValue -> Maybe Bool
doCmpOp CmpEq {} v1 v2 = Just $ doCmpEq v1 v2
doCmpOp CmpUlt {} (IntValue v1) (IntValue v2) = Just $ doCmpUlt v1 v2
doCmpOp CmpUle {} (IntValue v1) (IntValue v2) = Just $ doCmpUle v1 v2
doCmpOp CmpSlt {} (IntValue v1) (IntValue v2) = Just $ doCmpSlt v1 v2
doCmpOp CmpSle {} (IntValue v1) (IntValue v2) = Just $ doCmpSle v1 v2
doCmpOp FCmpLt {} (FloatValue v1) (FloatValue v2) = Just $ doFCmpLt v1 v2
doCmpOp FCmpLe {} (FloatValue v1) (FloatValue v2) = Just $ doFCmpLe v1 v2
doCmpOp CmpLlt {} (BoolValue v1) (BoolValue v2) = Just $ not v1 && v2
doCmpOp CmpLle {} (BoolValue v1) (BoolValue v2) = Just $ not (v1 && not v2)
doCmpOp _ _ _ = Nothing

-- | Compare any two primtive values for exact equality.
doCmpEq :: PrimValue -> PrimValue -> Bool
doCmpEq (FloatValue (Float32Value v1)) (FloatValue (Float32Value v2)) = v1 == v2
doCmpEq (FloatValue (Float64Value v1)) (FloatValue (Float64Value v2)) = v1 == v2
doCmpEq v1 v2 = v1 == v2

-- | Unsigned less than.
doCmpUlt :: IntValue -> IntValue -> Bool
doCmpUlt v1 v2 = intToWord64 v1 < intToWord64 v2

-- | Unsigned less than or equal.
doCmpUle :: IntValue -> IntValue -> Bool
doCmpUle v1 v2 = intToWord64 v1 <= intToWord64 v2

-- | Signed less than.
doCmpSlt :: IntValue -> IntValue -> Bool
doCmpSlt = (<)

-- | Signed less than or equal.
doCmpSle :: IntValue -> IntValue -> Bool
doCmpSle = (<=)

-- | Floating-point less than.
doFCmpLt :: FloatValue -> FloatValue -> Bool
doFCmpLt = (<)

-- | Floating-point less than or equal.
doFCmpLe :: FloatValue -> FloatValue -> Bool
doFCmpLe = (<=)

-- | Translate an t'IntValue' to 'Word64'.  This is guaranteed to fit.
intToWord64 :: IntValue -> Word64
intToWord64 (Int8Value v) = fromIntegral (fromIntegral v :: Word8)
intToWord64 (Int16Value v) = fromIntegral (fromIntegral v :: Word16)
intToWord64 (Int32Value v) = fromIntegral (fromIntegral v :: Word32)
intToWord64 (Int64Value v) = fromIntegral (fromIntegral v :: Word64)

-- | Translate an t'IntValue' to t'Int64'.  This is guaranteed to fit.
intToInt64 :: IntValue -> Int64
intToInt64 (Int8Value v) = fromIntegral v
intToInt64 (Int16Value v) = fromIntegral v
intToInt64 (Int32Value v) = fromIntegral v
intToInt64 (Int64Value v) = fromIntegral v

-- | Careful - there is no guarantee this will fit.
intToInt :: IntValue -> Int
intToInt = fromIntegral . intToInt64

floatToDouble :: FloatValue -> Double
floatToDouble (Float16Value v)
  | isInfinite v, v > 0 = 1 / 0
  | isInfinite v, v < 0 = -1 / 0
  | isNaN v = 0 / 0
  | otherwise = fromRational $ toRational v
floatToDouble (Float32Value v)
  | isInfinite v, v > 0 = 1 / 0
  | isInfinite v, v < 0 = -1 / 0
  | isNaN v = 0 / 0
  | otherwise = fromRational $ toRational v
floatToDouble (Float64Value v) = v

floatToFloat :: FloatValue -> Float
floatToFloat (Float16Value v)
  | isInfinite v, v > 0 = 1 / 0
  | isInfinite v, v < 0 = -1 / 0
  | isNaN v = 0 / 0
  | otherwise = fromRational $ toRational v
floatToFloat (Float32Value v) = v
floatToFloat (Float64Value v)
  | isInfinite v, v > 0 = 1 / 0
  | isInfinite v, v < 0 = -1 / 0
  | isNaN v = 0 / 0
  | otherwise = fromRational $ toRational v

floatToHalf :: FloatValue -> Half
floatToHalf (Float16Value v) = v
floatToHalf (Float32Value v)
  | isInfinite v, v > 0 = 1 / 0
  | isInfinite v, v < 0 = -1 / 0
  | isNaN v = 0 / 0
  | otherwise = fromRational $ toRational v
floatToHalf (Float64Value v)
  | isInfinite v, v > 0 = 1 / 0
  | isInfinite v, v < 0 = -1 / 0
  | isNaN v = 0 / 0
  | otherwise = fromRational $ toRational v

-- | The result type of a binary operator.
binOpType :: BinOp -> PrimType
binOpType (Add t _) = IntType t
binOpType (Sub t _) = IntType t
binOpType (Mul t _) = IntType t
binOpType (SDiv t _) = IntType t
binOpType (SDivUp t _) = IntType t
binOpType (SMod t _) = IntType t
binOpType (SQuot t _) = IntType t
binOpType (SRem t _) = IntType t
binOpType (UDiv t _) = IntType t
binOpType (UDivUp t _) = IntType t
binOpType (UMod t _) = IntType t
binOpType (SMin t) = IntType t
binOpType (UMin t) = IntType t
binOpType (FMin t) = FloatType t
binOpType (SMax t) = IntType t
binOpType (UMax t) = IntType t
binOpType (FMax t) = FloatType t
binOpType (Shl t) = IntType t
binOpType (LShr t) = IntType t
binOpType (AShr t) = IntType t
binOpType (And t) = IntType t
binOpType (Or t) = IntType t
binOpType (Xor t) = IntType t
binOpType (Pow t) = IntType t
binOpType (FPow t) = FloatType t
binOpType LogAnd = Bool
binOpType LogOr = Bool
binOpType (FAdd t) = FloatType t
binOpType (FSub t) = FloatType t
binOpType (FMul t) = FloatType t
binOpType (FDiv t) = FloatType t
binOpType (FMod t) = FloatType t

-- | The operand types of a comparison operator.
cmpOpType :: CmpOp -> PrimType
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
unOpType :: UnOp -> PrimType
unOpType (SSignum t) = IntType t
unOpType (USignum t) = IntType t
unOpType (Neg t) = t
unOpType (Complement t) = IntType t
unOpType (Abs t) = IntType t
unOpType (FAbs t) = FloatType t
unOpType (FSignum t) = FloatType t

-- | The input and output types of a conversion operator.
convOpType :: ConvOp -> (PrimType, PrimType)
convOpType (ZExt from to) = (IntType from, IntType to)
convOpType (SExt from to) = (IntType from, IntType to)
convOpType (FPConv from to) = (FloatType from, FloatType to)
convOpType (FPToUI from to) = (FloatType from, IntType to)
convOpType (FPToSI from to) = (FloatType from, IntType to)
convOpType (UIToFP from to) = (IntType from, FloatType to)
convOpType (SIToFP from to) = (IntType from, FloatType to)
convOpType (FPToBits from) = (FloatType from, IntType $ matchingInt from)
convOpType (BitsToFP to) = (IntType $ matchingInt to, FloatType to)
convOpType (IToB from) = (IntType from, Bool)
convOpType (BToI to) = (Bool, IntType to)
convOpType (FToB from) = (FloatType from, Bool)
convOpType (BToF to) = (Bool, FloatType to)

halfToWord :: Half -> Word16
halfToWord (Half (CUShort x)) = x

wordToHalf :: Word16 -> Half
wordToHalf = Half . CUShort

floatToWord :: Float -> Word32
floatToWord = G.runGet G.getWord32le . P.runPut . P.putFloatle

wordToFloat :: Word32 -> Float
wordToFloat = G.runGet G.getFloatle . P.runPut . P.putWord32le

doubleToWord :: Double -> Word64
doubleToWord = G.runGet G.getWord64le . P.runPut . P.putDoublele

wordToDouble :: Word64 -> Double
wordToDouble = G.runGet G.getDoublele . P.runPut . P.putWord64le

-- | @condFun t@ is the name of the ternary conditional function that
-- accepts operands of type @[Bool, t, t]@, and returns either the
-- first or second @t@ based on the truth value of the @Bool@.
condFun :: PrimType -> T.Text
condFun t = "cond_" <> prettyText t

-- | Is this the name of a condition function as per 'condFun', and
-- for which type?
isCondFun :: T.Text -> Maybe PrimType
isCondFun v = L.find (\t -> condFun t == v) allPrimTypes

-- | A mapping from names of primitive functions to their parameter
-- types, their result type, and a function for evaluating them.
primFuns ::
  M.Map
    T.Text
    ( [PrimType],
      PrimType,
      [PrimValue] -> Maybe PrimValue
    )
primFuns =
  M.fromList $
    [ f16 "sqrt16" sqrt,
      f32 "sqrt32" sqrt,
      f64 "sqrt64" sqrt,
      --
      f16 "rsqrt16" $ recip . sqrt,
      f32 "rsqrt32" $ recip . sqrt,
      f64 "rsqrt64" $ recip . sqrt,
      --
      f16 "cbrt16" $ convFloat . cbrtf . convFloat,
      f32 "cbrt32" cbrtf,
      f64 "cbrt64" cbrt,
      --
      f16 "log16" log,
      f32 "log32" log,
      f64 "log64" log,
      --
      f16 "log10_16" (logBase 10),
      f32 "log10_32" (logBase 10),
      f64 "log10_64" (logBase 10),
      --
      f16 "log1p_16" log1p,
      f32 "log1p_32" log1p,
      f64 "log1p_64" log1p,
      --
      f16 "log2_16" (logBase 2),
      f32 "log2_32" (logBase 2),
      f64 "log2_64" (logBase 2),
      --
      f16 "exp16" exp,
      f32 "exp32" exp,
      f64 "exp64" exp,
      --
      f16 "sin16" sin,
      f32 "sin32" sin,
      f64 "sin64" sin,
      --
      f16 "sinpi16" $ sin . (pi *),
      f32 "sinpi32" $ sin . (pi *),
      f64 "sinpi64" $ sin . (pi *),
      --
      f16 "sinh16" sinh,
      f32 "sinh32" sinh,
      f64 "sinh64" sinh,
      --
      f16 "cos16" cos,
      f32 "cos32" cos,
      f64 "cos64" cos,
      --
      f16 "cospi16" $ cos . (pi *),
      f32 "cospi32" $ cos . (pi *),
      f64 "cospi64" $ cos . (pi *),
      --
      f16 "cosh16" cosh,
      f32 "cosh32" cosh,
      f64 "cosh64" cosh,
      --
      f16 "tan16" tan,
      f32 "tan32" tan,
      f64 "tan64" tan,
      --
      f16 "tanpi16" $ tan . (pi *),
      f32 "tanpi32" $ tan . (pi *),
      f64 "tanpi64" $ tan . (pi *),
      --
      f16 "tanh16" tanh,
      f32 "tanh32" tanh,
      f64 "tanh64" tanh,
      --
      f16 "asin16" asin,
      f32 "asin32" asin,
      f64 "asin64" asin,
      --
      f16 "asinpi16" $ (/ pi) . asin,
      f32 "asinpi32" $ (/ pi) . asin,
      f64 "asinpi64" $ (/ pi) . asin,
      --
      f16 "asinh16" asinh,
      f32 "asinh32" asinh,
      f64 "asinh64" asinh,
      --
      f16 "acos16" acos,
      f32 "acos32" acos,
      f64 "acos64" acos,
      --
      f16 "acospi16" $ (/ pi) . acos,
      f32 "acospi32" $ (/ pi) . acos,
      f64 "acospi64" $ (/ pi) . acos,
      --
      f16 "acosh16" acosh,
      f32 "acosh32" acosh,
      f64 "acosh64" acosh,
      --
      f16 "atan16" atan,
      f32 "atan32" atan,
      f64 "atan64" atan,
      --
      f16 "atanpi16" $ (/ pi) . atan,
      f32 "atanpi32" $ (/ pi) . atan,
      f64 "atanpi64" $ (/ pi) . atan,
      --
      f16 "atanh16" atanh,
      f32 "atanh32" atanh,
      f64 "atanh64" atanh,
      --
      f16 "round16" $ convFloat . roundFloat . convFloat,
      f32 "round32" roundFloat,
      f64 "round64" roundDouble,
      --
      f16 "ceil16" $ convFloat . ceilFloat . convFloat,
      f32 "ceil32" ceilFloat,
      f64 "ceil64" ceilDouble,
      --
      f16 "floor16" $ convFloat . floorFloat . convFloat,
      f32 "floor32" floorFloat,
      f64 "floor64" floorDouble,
      --
      f16_2 "nextafter16" (\x y -> convFloat $ nextafterf (convFloat x) (convFloat y)),
      f32_2 "nextafter32" nextafterf,
      f64_2 "nextafter64" nextafter,
      --
      ( "ldexp16",
        ( [FloatType Float16, IntType Int32],
          FloatType Float16,
          \case
            [FloatValue (Float16Value x), IntValue (Int32Value y)] ->
              Just $ FloatValue $ Float16Value $ x * (2 ** fromIntegral y)
            _ -> Nothing
        )
      ),
      ( "ldexp32",
        ( [FloatType Float32, IntType Int32],
          FloatType Float32,
          \case
            [FloatValue (Float32Value x), IntValue (Int32Value y)] ->
              Just $ FloatValue $ Float32Value $ ldexpf x $ fromIntegral y
            _ -> Nothing
        )
      ),
      ( "ldexp64",
        ( [FloatType Float64, IntType Int32],
          FloatType Float64,
          \case
            [FloatValue (Float64Value x), IntValue (Int32Value y)] ->
              Just $ FloatValue $ Float64Value $ ldexp x $ fromIntegral y
            _ -> Nothing
        )
      ),
      --
      f16 "gamma16" $ convFloat . tgammaf . convFloat,
      f32 "gamma32" tgammaf,
      f64 "gamma64" tgamma,
      --
      f16 "lgamma16" $ convFloat . lgammaf . convFloat,
      f32 "lgamma32" lgammaf,
      f64 "lgamma64" lgamma,
      --
      --
      f16 "erf16" $ convFloat . erff . convFloat,
      f32 "erf32" erff,
      f64 "erf64" erf,
      --
      f16 "erfc16" $ convFloat . erfcf . convFloat,
      f32 "erfc32" erfcf,
      f64 "erfc64" erfc,
      --
      f16_2 "copysign16" $ \x y -> convFloat (copysign (convFloat x) (convFloat y)),
      f32_2 "copysign32" copysignf,
      f64_2 "copysign64" copysign,
      --
      i8 "clz8" $ IntValue . Int32Value . fromIntegral . countLeadingZeros,
      i16 "clz16" $ IntValue . Int32Value . fromIntegral . countLeadingZeros,
      i32 "clz32" $ IntValue . Int32Value . fromIntegral . countLeadingZeros,
      i64 "clz64" $ IntValue . Int32Value . fromIntegral . countLeadingZeros,
      i8 "ctz8" $ IntValue . Int32Value . fromIntegral . countTrailingZeros,
      i16 "ctz16" $ IntValue . Int32Value . fromIntegral . countTrailingZeros,
      i32 "ctz32" $ IntValue . Int32Value . fromIntegral . countTrailingZeros,
      i64 "ctz64" $ IntValue . Int32Value . fromIntegral . countTrailingZeros,
      i8 "popc8" $ IntValue . Int32Value . fromIntegral . popCount,
      i16 "popc16" $ IntValue . Int32Value . fromIntegral . popCount,
      i32 "popc32" $ IntValue . Int32Value . fromIntegral . popCount,
      i64 "popc64" $ IntValue . Int32Value . fromIntegral . popCount,
      i8_3 "umad_hi8" umad_hi8,
      i16_3 "umad_hi16" umad_hi16,
      i32_3 "umad_hi32" umad_hi32,
      i64_3 "umad_hi64" umad_hi64,
      i8_2 "umul_hi8" umul_hi8,
      i16_2 "umul_hi16" umul_hi16,
      i32_2 "umul_hi32" umul_hi32,
      i64_2 "umul_hi64" umul_hi64,
      i8_3 "smad_hi8" smad_hi8,
      i16_3 "smad_hi16" smad_hi16,
      i32_3 "smad_hi32" smad_hi32,
      i64_3 "smad_hi64" smad_hi64,
      i8_2 "smul_hi8" smul_hi8,
      i16_2 "smul_hi16" smul_hi16,
      i32_2 "smul_hi32" smul_hi32,
      i64_2 "smul_hi64" smul_hi64,
      --
      ( "atan2_16",
        ( [FloatType Float16, FloatType Float16],
          FloatType Float16,
          \case
            [FloatValue (Float16Value x), FloatValue (Float16Value y)] ->
              Just $ FloatValue $ Float16Value $ atan2 x y
            _ -> Nothing
        )
      ),
      ( "atan2_32",
        ( [FloatType Float32, FloatType Float32],
          FloatType Float32,
          \case
            [FloatValue (Float32Value x), FloatValue (Float32Value y)] ->
              Just $ FloatValue $ Float32Value $ atan2 x y
            _ -> Nothing
        )
      ),
      ( "atan2_64",
        ( [FloatType Float64, FloatType Float64],
          FloatType Float64,
          \case
            [FloatValue (Float64Value x), FloatValue (Float64Value y)] ->
              Just $ FloatValue $ Float64Value $ atan2 x y
            _ -> Nothing
        )
      ),
      ( "atan2pi_16",
        ( [FloatType Float16, FloatType Float16],
          FloatType Float16,
          \case
            [FloatValue (Float16Value x), FloatValue (Float16Value y)] ->
              Just $ FloatValue $ Float16Value $ atan2 x y / pi
            _ -> Nothing
        )
      ),
      ( "atan2pi_32",
        ( [FloatType Float32, FloatType Float32],
          FloatType Float32,
          \case
            [FloatValue (Float32Value x), FloatValue (Float32Value y)] ->
              Just $ FloatValue $ Float32Value $ atan2 x y / pi
            _ -> Nothing
        )
      ),
      ( "atan2pi_64",
        ( [FloatType Float64, FloatType Float64],
          FloatType Float64,
          \case
            [FloatValue (Float64Value x), FloatValue (Float64Value y)] ->
              Just $ FloatValue $ Float64Value $ atan2 x y / pi
            _ -> Nothing
        )
      ),
      --
      ( "hypot16",
        ( [FloatType Float16, FloatType Float16],
          FloatType Float16,
          \case
            [FloatValue (Float16Value x), FloatValue (Float16Value y)] ->
              Just $ FloatValue $ Float16Value $ convFloat $ hypotf (convFloat x) (convFloat y)
            _ -> Nothing
        )
      ),
      ( "hypot32",
        ( [FloatType Float32, FloatType Float32],
          FloatType Float32,
          \case
            [FloatValue (Float32Value x), FloatValue (Float32Value y)] ->
              Just $ FloatValue $ Float32Value $ hypotf x y
            _ -> Nothing
        )
      ),
      ( "hypot64",
        ( [FloatType Float64, FloatType Float64],
          FloatType Float64,
          \case
            [FloatValue (Float64Value x), FloatValue (Float64Value y)] ->
              Just $ FloatValue $ Float64Value $ hypot x y
            _ -> Nothing
        )
      ),
      ( "isinf16",
        ( [FloatType Float16],
          Bool,
          \case
            [FloatValue (Float16Value x)] -> Just $ BoolValue $ isInfinite x
            _ -> Nothing
        )
      ),
      ( "isinf32",
        ( [FloatType Float32],
          Bool,
          \case
            [FloatValue (Float32Value x)] -> Just $ BoolValue $ isInfinite x
            _ -> Nothing
        )
      ),
      ( "isinf64",
        ( [FloatType Float64],
          Bool,
          \case
            [FloatValue (Float64Value x)] -> Just $ BoolValue $ isInfinite x
            _ -> Nothing
        )
      ),
      ( "isnan16",
        ( [FloatType Float16],
          Bool,
          \case
            [FloatValue (Float16Value x)] -> Just $ BoolValue $ isNaN x
            _ -> Nothing
        )
      ),
      ( "isnan32",
        ( [FloatType Float32],
          Bool,
          \case
            [FloatValue (Float32Value x)] -> Just $ BoolValue $ isNaN x
            _ -> Nothing
        )
      ),
      ( "isnan64",
        ( [FloatType Float64],
          Bool,
          \case
            [FloatValue (Float64Value x)] -> Just $ BoolValue $ isNaN x
            _ -> Nothing
        )
      ),
      f16_3 "lerp16" (\v0 v1 t -> v0 + (v1 - v0) * max 0 (min 1 t)),
      f32_3 "lerp32" (\v0 v1 t -> v0 + (v1 - v0) * max 0 (min 1 t)),
      f64_3 "lerp64" (\v0 v1 t -> v0 + (v1 - v0) * max 0 (min 1 t)),
      f16_3 "mad16" (\a b c -> a * b + c),
      f32_3 "mad32" (\a b c -> a * b + c),
      f64_3 "mad64" (\a b c -> a * b + c),
      f16_3 "fma16" (\a b c -> a * b + c),
      f32_3 "fma32" (\a b c -> a * b + c),
      f64_3 "fma64" (\a b c -> a * b + c)
    ]
      <> [ ( condFun t,
             ( [Bool, t, t],
               t,
               \case
                 [BoolValue b, tv, fv] ->
                   Just $ if b then tv else fv
                 _ -> Nothing
             )
           )
           | t <- allPrimTypes
         ]
  where
    i8 s f = (s, ([IntType Int8], IntType Int32, i8PrimFun f))
    i16 s f = (s, ([IntType Int16], IntType Int32, i16PrimFun f))
    i32 s f = (s, ([IntType Int32], IntType Int32, i32PrimFun f))
    i64 s f = (s, ([IntType Int64], IntType Int32, i64PrimFun f))
    f16 s f = (s, ([FloatType Float16], FloatType Float16, f16PrimFun f))
    f32 s f = (s, ([FloatType Float32], FloatType Float32, f32PrimFun f))
    f64 s f = (s, ([FloatType Float64], FloatType Float64, f64PrimFun f))
    t_2 t s f = (s, ([t, t], t, f))
    t_3 t s f = (s, ([t, t, t], t, f))
    f16_2 s f = t_2 (FloatType Float16) s (f16PrimFun2 f)
    f32_2 s f = t_2 (FloatType Float32) s (f32PrimFun2 f)
    f64_2 s f = t_2 (FloatType Float64) s (f64PrimFun2 f)
    f16_3 s f = t_3 (FloatType Float16) s (f16PrimFun3 f)
    f32_3 s f = t_3 (FloatType Float32) s (f32PrimFun3 f)
    f64_3 s f = t_3 (FloatType Float64) s (f64PrimFun3 f)
    i8_2 s f = t_2 (IntType Int8) s (i8PrimFun2 f)
    i16_2 s f = t_2 (IntType Int16) s (i16PrimFun2 f)
    i32_2 s f = t_2 (IntType Int32) s (i32PrimFun2 f)
    i64_2 s f = t_2 (IntType Int64) s (i64PrimFun2 f)
    i8_3 s f = t_3 (IntType Int8) s (i8PrimFun3 f)
    i16_3 s f = t_3 (IntType Int16) s (i16PrimFun3 f)
    i32_3 s f = t_3 (IntType Int32) s (i32PrimFun3 f)
    i64_3 s f = t_3 (IntType Int64) s (i64PrimFun3 f)

    i8PrimFun f [IntValue (Int8Value x)] = Just $ f x
    i8PrimFun _ _ = Nothing

    i16PrimFun f [IntValue (Int16Value x)] = Just $ f x
    i16PrimFun _ _ = Nothing

    i32PrimFun f [IntValue (Int32Value x)] = Just $ f x
    i32PrimFun _ _ = Nothing

    i64PrimFun f [IntValue (Int64Value x)] = Just $ f x
    i64PrimFun _ _ = Nothing

    f16PrimFun f [FloatValue (Float16Value x)] =
      Just $ FloatValue $ Float16Value $ f x
    f16PrimFun _ _ = Nothing

    f32PrimFun f [FloatValue (Float32Value x)] =
      Just $ FloatValue $ Float32Value $ f x
    f32PrimFun _ _ = Nothing

    f64PrimFun f [FloatValue (Float64Value x)] =
      Just $ FloatValue $ Float64Value $ f x
    f64PrimFun _ _ = Nothing

    f16PrimFun2
      f
      [ FloatValue (Float16Value a),
        FloatValue (Float16Value b)
        ] =
        Just $ FloatValue $ Float16Value $ f a b
    f16PrimFun2 _ _ = Nothing

    f32PrimFun2
      f
      [ FloatValue (Float32Value a),
        FloatValue (Float32Value b)
        ] =
        Just $ FloatValue $ Float32Value $ f a b
    f32PrimFun2 _ _ = Nothing

    f64PrimFun2
      f
      [ FloatValue (Float64Value a),
        FloatValue (Float64Value b)
        ] =
        Just $ FloatValue $ Float64Value $ f a b
    f64PrimFun2 _ _ = Nothing

    f16PrimFun3
      f
      [ FloatValue (Float16Value a),
        FloatValue (Float16Value b),
        FloatValue (Float16Value c)
        ] =
        Just $ FloatValue $ Float16Value $ f a b c
    f16PrimFun3 _ _ = Nothing

    f32PrimFun3
      f
      [ FloatValue (Float32Value a),
        FloatValue (Float32Value b),
        FloatValue (Float32Value c)
        ] =
        Just $ FloatValue $ Float32Value $ f a b c
    f32PrimFun3 _ _ = Nothing

    f64PrimFun3
      f
      [ FloatValue (Float64Value a),
        FloatValue (Float64Value b),
        FloatValue (Float64Value c)
        ] =
        Just $ FloatValue $ Float64Value $ f a b c
    f64PrimFun3 _ _ = Nothing

    i8PrimFun2
      f
      [IntValue (Int8Value a), IntValue (Int8Value b)] =
        Just $ IntValue $ Int8Value $ f a b
    i8PrimFun2 _ _ = Nothing

    i16PrimFun2
      f
      [IntValue (Int16Value a), IntValue (Int16Value b)] =
        Just $ IntValue $ Int16Value $ f a b
    i16PrimFun2 _ _ = Nothing

    i32PrimFun2
      f
      [IntValue (Int32Value a), IntValue (Int32Value b)] =
        Just $ IntValue $ Int32Value $ f a b
    i32PrimFun2 _ _ = Nothing

    i64PrimFun2
      f
      [IntValue (Int64Value a), IntValue (Int64Value b)] =
        Just $ IntValue $ Int64Value $ f a b
    i64PrimFun2 _ _ = Nothing

    i8PrimFun3
      f
      [IntValue (Int8Value a), IntValue (Int8Value b), IntValue (Int8Value c)] =
        Just $ IntValue $ Int8Value $ f a b c
    i8PrimFun3 _ _ = Nothing

    i16PrimFun3
      f
      [IntValue (Int16Value a), IntValue (Int16Value b), IntValue (Int16Value c)] =
        Just $ IntValue $ Int16Value $ f a b c
    i16PrimFun3 _ _ = Nothing

    i32PrimFun3
      f
      [IntValue (Int32Value a), IntValue (Int32Value b), IntValue (Int32Value c)] =
        Just $ IntValue $ Int32Value $ f a b c
    i32PrimFun3 _ _ = Nothing

    i64PrimFun3
      f
      [IntValue (Int64Value a), IntValue (Int64Value b), IntValue (Int64Value c)] =
        Just $ IntValue $ Int64Value $ f a b c
    i64PrimFun3 _ _ = Nothing

-- | Is the given value kind of zero?
zeroIsh :: PrimValue -> Bool
zeroIsh (IntValue k) = zeroIshInt k
zeroIsh (FloatValue (Float16Value k)) = k == 0
zeroIsh (FloatValue (Float32Value k)) = k == 0
zeroIsh (FloatValue (Float64Value k)) = k == 0
zeroIsh (BoolValue False) = True
zeroIsh _ = False

-- | Is the given value kind of one?
oneIsh :: PrimValue -> Bool
oneIsh (IntValue k) = oneIshInt k
oneIsh (FloatValue (Float16Value k)) = k == 1
oneIsh (FloatValue (Float32Value k)) = k == 1
oneIsh (FloatValue (Float64Value k)) = k == 1
oneIsh (BoolValue True) = True
oneIsh _ = False

-- | Is the given value kind of negative?
negativeIsh :: PrimValue -> Bool
negativeIsh (IntValue k) = negativeIshInt k
negativeIsh (FloatValue (Float16Value k)) = k < 0
negativeIsh (FloatValue (Float32Value k)) = k < 0
negativeIsh (FloatValue (Float64Value k)) = k < 0
negativeIsh (BoolValue _) = False
negativeIsh UnitValue = False

-- | Is the given integer value kind of zero?
zeroIshInt :: IntValue -> Bool
zeroIshInt (Int8Value k) = k == 0
zeroIshInt (Int16Value k) = k == 0
zeroIshInt (Int32Value k) = k == 0
zeroIshInt (Int64Value k) = k == 0

-- | Is the given integer value kind of one?
oneIshInt :: IntValue -> Bool
oneIshInt (Int8Value k) = k == 1
oneIshInt (Int16Value k) = k == 1
oneIshInt (Int32Value k) = k == 1
oneIshInt (Int64Value k) = k == 1

-- | Is the given integer value kind of negative?
negativeIshInt :: IntValue -> Bool
negativeIshInt (Int8Value k) = k < 0
negativeIshInt (Int16Value k) = k < 0
negativeIshInt (Int32Value k) = k < 0
negativeIshInt (Int64Value k) = k < 0

-- | The size of a value of a given integer type in eight-bit bytes.
intByteSize :: (Num a) => IntType -> a
intByteSize Int8 = 1
intByteSize Int16 = 2
intByteSize Int32 = 4
intByteSize Int64 = 8

-- | The size of a value of a given floating-point type in eight-bit bytes.
floatByteSize :: (Num a) => FloatType -> a
floatByteSize Float16 = 2
floatByteSize Float32 = 4
floatByteSize Float64 = 8

-- | The size of a value of a given primitive type in eight-bit bytes.
--
-- Warning: note that this is 0 for 'Unit', but a 'Unit' takes up a
-- byte in the binary data format.
primByteSize :: (Num a) => PrimType -> a
primByteSize (IntType t) = intByteSize t
primByteSize (FloatType t) = floatByteSize t
primByteSize Bool = 1
primByteSize Unit = 0

-- | The size of a value of a given primitive type in bits.
primBitSize :: PrimType -> Int
primBitSize = (* 8) . primByteSize

-- | True if the given binary operator is commutative.
commutativeBinOp :: BinOp -> Bool
commutativeBinOp Add {} = True
commutativeBinOp FAdd {} = True
commutativeBinOp Mul {} = True
commutativeBinOp FMul {} = True
commutativeBinOp And {} = True
commutativeBinOp Or {} = True
commutativeBinOp Xor {} = True
commutativeBinOp LogOr {} = True
commutativeBinOp LogAnd {} = True
commutativeBinOp SMax {} = True
commutativeBinOp SMin {} = True
commutativeBinOp UMax {} = True
commutativeBinOp UMin {} = True
commutativeBinOp FMax {} = True
commutativeBinOp FMin {} = True
commutativeBinOp _ = False

-- | True if the given binary operator is associative.
associativeBinOp :: BinOp -> Bool
associativeBinOp Add {} = True
associativeBinOp Mul {} = True
associativeBinOp And {} = True
associativeBinOp Or {} = True
associativeBinOp Xor {} = True
associativeBinOp LogOr {} = True
associativeBinOp LogAnd {} = True
associativeBinOp SMax {} = True
associativeBinOp SMin {} = True
associativeBinOp UMax {} = True
associativeBinOp UMin {} = True
associativeBinOp FMax {} = True
associativeBinOp FMin {} = True
associativeBinOp _ = False

-- Prettyprinting instances

instance Pretty BinOp where
  pretty (Add t OverflowWrap) = taggedI "add" t
  pretty (Add t OverflowUndef) = taggedI "add_nw" t
  pretty (Sub t OverflowWrap) = taggedI "sub" t
  pretty (Sub t OverflowUndef) = taggedI "sub_nw" t
  pretty (Mul t OverflowWrap) = taggedI "mul" t
  pretty (Mul t OverflowUndef) = taggedI "mul_nw" t
  pretty (FAdd t) = taggedF "fadd" t
  pretty (FSub t) = taggedF "fsub" t
  pretty (FMul t) = taggedF "fmul" t
  pretty (UDiv t Safe) = taggedI "udiv_safe" t
  pretty (UDiv t Unsafe) = taggedI "udiv" t
  pretty (UDivUp t Safe) = taggedI "udiv_up_safe" t
  pretty (UDivUp t Unsafe) = taggedI "udiv_up" t
  pretty (UMod t Safe) = taggedI "umod_safe" t
  pretty (UMod t Unsafe) = taggedI "umod" t
  pretty (SDiv t Safe) = taggedI "sdiv_safe" t
  pretty (SDiv t Unsafe) = taggedI "sdiv" t
  pretty (SDivUp t Safe) = taggedI "sdiv_up_safe" t
  pretty (SDivUp t Unsafe) = taggedI "sdiv_up" t
  pretty (SMod t Safe) = taggedI "smod_safe" t
  pretty (SMod t Unsafe) = taggedI "smod" t
  pretty (SQuot t Safe) = taggedI "squot_safe" t
  pretty (SQuot t Unsafe) = taggedI "squot" t
  pretty (SRem t Safe) = taggedI "srem_safe" t
  pretty (SRem t Unsafe) = taggedI "srem" t
  pretty (FDiv t) = taggedF "fdiv" t
  pretty (FMod t) = taggedF "fmod" t
  pretty (SMin t) = taggedI "smin" t
  pretty (UMin t) = taggedI "umin" t
  pretty (FMin t) = taggedF "fmin" t
  pretty (SMax t) = taggedI "smax" t
  pretty (UMax t) = taggedI "umax" t
  pretty (FMax t) = taggedF "fmax" t
  pretty (Shl t) = taggedI "shl" t
  pretty (LShr t) = taggedI "lshr" t
  pretty (AShr t) = taggedI "ashr" t
  pretty (And t) = taggedI "and" t
  pretty (Or t) = taggedI "or" t
  pretty (Xor t) = taggedI "xor" t
  pretty (Pow t) = taggedI "pow" t
  pretty (FPow t) = taggedF "fpow" t
  pretty LogAnd = "logand"
  pretty LogOr = "logor"

instance Pretty CmpOp where
  pretty (CmpEq t) = "eq_" <> pretty t
  pretty (CmpUlt t) = taggedI "ult" t
  pretty (CmpUle t) = taggedI "ule" t
  pretty (CmpSlt t) = taggedI "slt" t
  pretty (CmpSle t) = taggedI "sle" t
  pretty (FCmpLt t) = taggedF "lt" t
  pretty (FCmpLe t) = taggedF "le" t
  pretty CmpLlt = "llt"
  pretty CmpLle = "lle"

instance Pretty ConvOp where
  pretty op = convOp (convOpFun op) from to
    where
      (from, to) = convOpType op

instance Pretty UnOp where
  pretty (Neg t) = "neg_" <> pretty t
  pretty (Abs t) = taggedI "abs" t
  pretty (FAbs t) = taggedF "fabs" t
  pretty (SSignum t) = taggedI "ssignum" t
  pretty (USignum t) = taggedI "usignum" t
  pretty (FSignum t) = taggedF "fsignum" t
  pretty (Complement t) = taggedI "complement" t

-- | The human-readable name for a 'ConvOp'.  This is used to expose
-- the 'ConvOp' in the @intrinsics@ module of a Futhark program.
convOpFun :: ConvOp -> String
convOpFun ZExt {} = "zext"
convOpFun SExt {} = "sext"
convOpFun FPConv {} = "fpconv"
convOpFun FPToUI {} = "fptoui"
convOpFun FPToSI {} = "fptosi"
convOpFun UIToFP {} = "uitofp"
convOpFun SIToFP {} = "sitofp"
convOpFun FPToBits {} = "fptobits"
convOpFun BitsToFP {} = "bitstofp"
convOpFun IToB {} = "itob"
convOpFun BToI {} = "btoi"
convOpFun FToB {} = "ftob"
convOpFun BToF {} = "btof"

taggedI :: String -> IntType -> Doc a
taggedI s Int8 = pretty $ s ++ "8"
taggedI s Int16 = pretty $ s ++ "16"
taggedI s Int32 = pretty $ s ++ "32"
taggedI s Int64 = pretty $ s ++ "64"

taggedF :: String -> FloatType -> Doc a
taggedF s Float16 = pretty $ s ++ "16"
taggedF s Float32 = pretty $ s ++ "32"
taggedF s Float64 = pretty $ s ++ "64"

convOp :: (Pretty from, Pretty to) => String -> from -> to -> Doc a
convOp s from to = pretty s <> "_" <> pretty from <> "_" <> pretty to

-- | True if signed.  Only makes a difference for integer types.
prettySigned :: Bool -> PrimType -> T.Text
prettySigned True (IntType it) = T.cons 'u' (T.drop 1 (prettyText it))
prettySigned _ t = prettyText t

umul_hi8 :: Int8 -> Int8 -> Int8
umul_hi8 a b =
  let a' = fromIntegral (fromIntegral a :: Word8) :: Word64
      b' = fromIntegral (fromIntegral b :: Word8) :: Word64
   in fromIntegral (shiftR (a' * b') 8)

umul_hi16 :: Int16 -> Int16 -> Int16
umul_hi16 a b =
  let a' = fromIntegral (fromIntegral a :: Word16) :: Word64
      b' = fromIntegral (fromIntegral b :: Word16) :: Word64
   in fromIntegral (shiftR (a' * b') 16)

umul_hi32 :: Int32 -> Int32 -> Int32
umul_hi32 a b =
  let a' = fromIntegral (fromIntegral a :: Word32) :: Word64
      b' = fromIntegral (fromIntegral b :: Word32) :: Word64
   in fromIntegral (shiftR (a' * b') 32)

umul_hi64 :: Int64 -> Int64 -> Int64
umul_hi64 a b =
  let a' = toInteger (fromIntegral a :: Word64)
      b' = toInteger (fromIntegral b :: Word64)
   in fromIntegral (shiftR (a' * b') 64)

umad_hi8 :: Int8 -> Int8 -> Int8 -> Int8
umad_hi8 a b c = umul_hi8 a b + c

umad_hi16 :: Int16 -> Int16 -> Int16 -> Int16
umad_hi16 a b c = umul_hi16 a b + c

umad_hi32 :: Int32 -> Int32 -> Int32 -> Int32
umad_hi32 a b c = umul_hi32 a b + c

umad_hi64 :: Int64 -> Int64 -> Int64 -> Int64
umad_hi64 a b c = umul_hi64 a b + c

smul_hi8 :: Int8 -> Int8 -> Int8
smul_hi8 a b =
  let a' = fromIntegral a :: Int64
      b' = fromIntegral b :: Int64
   in fromIntegral (shiftR (a' * b') 8)

smul_hi16 :: Int16 -> Int16 -> Int16
smul_hi16 a b =
  let a' = fromIntegral a :: Int64
      b' = fromIntegral b :: Int64
   in fromIntegral (shiftR (a' * b') 16)

smul_hi32 :: Int32 -> Int32 -> Int32
smul_hi32 a b =
  let a' = fromIntegral a :: Int64
      b' = fromIntegral b :: Int64
   in fromIntegral (shiftR (a' * b') 32)

smul_hi64 :: Int64 -> Int64 -> Int64
smul_hi64 a b =
  let a' = toInteger a
      b' = toInteger b
   in fromIntegral (shiftR (a' * b') 64)

smad_hi8 :: Int8 -> Int8 -> Int8 -> Int8
smad_hi8 a b c = smul_hi8 a b + c

smad_hi16 :: Int16 -> Int16 -> Int16 -> Int16
smad_hi16 a b c = smul_hi16 a b + c

smad_hi32 :: Int32 -> Int32 -> Int32 -> Int32
smad_hi32 a b c = smul_hi32 a b + c

smad_hi64 :: Int64 -> Int64 -> Int64 -> Int64
smad_hi64 a b c = smul_hi64 a b + c
