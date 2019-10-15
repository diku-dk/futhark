{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
-- | Definitions of primitive types, the values that inhabit these
-- types, and operations on these values.  A primitive value can also
-- be called a scalar.
--
-- Essentially, this module describes the subset of the (internal)
-- Futhark language that operates on primitive types.
module Futhark.Representation.Primitive
       ( -- * Types
         IntType (..), allIntTypes
       , FloatType (..), allFloatTypes
       , PrimType (..), allPrimTypes

         -- * Values
       , IntValue(..)
       , intValue, intValueType, valueIntegral
       , FloatValue(..)
       , floatValue, floatValueType
       , PrimValue(..)
       , primValueType
       , blankPrimValue

         -- * Operations
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
       , doAdd, doMul, doSDiv, doSMod
       , doPow

         -- ** Conversion Operations
       , doConvOp
       , doZExt, doSExt
       , doFPConv
       , doFPToUI, doFPToSI
       , doUIToFP, doSIToFP
       , intToInt64, intToWord64

         -- * Comparison Operations
       , doCmpOp
       , doCmpEq
       , doCmpUlt, doCmpUle
       , doCmpSlt, doCmpSle
       , doFCmpLt, doFCmpLe

        -- * Type Of
       , binOpType
       , unOpType
       , cmpOpType
       , convOpType

       -- * Primitive functions
       , primFuns

       -- * Utility
       , zeroIsh
       , oneIsh
       , negativeIsh
       , primBitSize
       , primByteSize
       , intByteSize
       , floatByteSize
       , commutativeBinOp

       -- * Prettyprinting
       , convOpFun
       , prettySigned
       )
       where

import           Control.Applicative
import           Data.Binary.IEEE754 (floatToWord, wordToFloat, doubleToWord, wordToDouble)
import           Data.Bits
import           Data.Fixed (mod') -- Weird location.
import           Data.Int            (Int16, Int32, Int64, Int8)
import qualified Data.Map as M
import           Data.Word

import           Prelude

import           Futhark.Util.Pretty
import           Futhark.Util (roundFloat, ceilFloat, floorFloat,
                               roundDouble, ceilDouble, floorDouble,
                               lgamma, lgammaf, tgamma, tgammaf)

-- | An integer type, ordered by size.  Note that signedness is not a
-- property of the type, but a property of the operations performed on
-- values of these types.
data IntType = Int8
             | Int16
             | Int32
             | Int64
             deriving (Eq, Ord, Show, Enum, Bounded)

instance Pretty IntType where
  ppr Int8  = text "i8"
  ppr Int16 = text "i16"
  ppr Int32 = text "i32"
  ppr Int64 = text "i64"

-- | A list of all integer types.
allIntTypes :: [IntType]
allIntTypes = [minBound..maxBound]

-- | A floating point type.
data FloatType = Float32
               | Float64
               deriving (Eq, Ord, Show, Enum, Bounded)

instance Pretty FloatType where
  ppr Float32 = text "f32"
  ppr Float64 = text "f64"

-- | A list of all floating-point types.
allFloatTypes :: [FloatType]
allFloatTypes = [minBound..maxBound]

-- | Low-level primitive types.
data PrimType = IntType IntType
              | FloatType FloatType
              | Bool
              | Cert
              deriving (Eq, Ord, Show)

instance Enum PrimType where
  toEnum 0 = IntType Int8
  toEnum 1 = IntType Int16
  toEnum 2 = IntType Int32
  toEnum 3 = IntType Int64
  toEnum 4 = FloatType Float32
  toEnum 5 = FloatType Float64
  toEnum 6 = Bool
  toEnum _ = Cert

  fromEnum (IntType Int8)      = 0
  fromEnum (IntType Int16)     = 1
  fromEnum (IntType Int32)     = 2
  fromEnum (IntType Int64)     = 3
  fromEnum (FloatType Float32) = 4
  fromEnum (FloatType Float64) = 5
  fromEnum Bool                = 6
  fromEnum Cert                = 7

instance Bounded PrimType where
  minBound = IntType Int8
  maxBound = Cert

instance Pretty PrimType where
  ppr (IntType t)   = ppr t
  ppr (FloatType t) = ppr t
  ppr Bool          = text "bool"
  ppr Cert          = text "cert"

-- | A list of all primitive types.
allPrimTypes :: [PrimType]
allPrimTypes = map IntType allIntTypes ++
               map FloatType allFloatTypes ++
               [Bool, Cert]

-- | An integer value.
data IntValue = Int8Value !Int8
              | Int16Value !Int16
              | Int32Value !Int32
              | Int64Value !Int64
               deriving (Eq, Ord, Show)

instance Pretty IntValue where
  ppr (Int8Value v)  = text $ show v ++ "i8"
  ppr (Int16Value v) = text $ show v ++ "i16"
  ppr (Int32Value v) = text $ show v ++ "i32"
  ppr (Int64Value v) = text $ show v ++ "i64"

-- | Create an 'IntValue' from a type and an 'Integer'.
intValue :: Integral int => IntType -> int -> IntValue
intValue Int8  = Int8Value . fromIntegral
intValue Int16 = Int16Value . fromIntegral
intValue Int32 = Int32Value . fromIntegral
intValue Int64 = Int64Value . fromIntegral

intValueType :: IntValue -> IntType
intValueType Int8Value{}  = Int8
intValueType Int16Value{} = Int16
intValueType Int32Value{} = Int32
intValueType Int64Value{} = Int64

-- | Convert an 'IntValue' to any 'Integral' type.
valueIntegral ::Integral int => IntValue -> int
valueIntegral (Int8Value  v) = fromIntegral v
valueIntegral (Int16Value v) = fromIntegral v
valueIntegral (Int32Value v) = fromIntegral v
valueIntegral (Int64Value v) = fromIntegral v

-- | A floating-point value.
data FloatValue = Float32Value !Float
                | Float64Value !Double
               deriving (Eq, Ord, Show)


instance Pretty FloatValue where
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

-- | Create a 'FloatValue' from a type and a 'Rational'.
floatValue :: Real num => FloatType -> num -> FloatValue
floatValue Float32 = Float32Value . fromRational . toRational
floatValue Float64 = Float64Value . fromRational . toRational

floatValueType :: FloatValue -> FloatType
floatValueType Float32Value{} = Float32
floatValueType Float64Value{} = Float64

-- | Non-array values.
data PrimValue = IntValue !IntValue
               | FloatValue !FloatValue
               | BoolValue !Bool
               | Checked -- ^ The only value of type @cert@.
               deriving (Eq, Ord, Show)

instance Pretty PrimValue where
  ppr (IntValue v)      = ppr v
  ppr (BoolValue True)  = text "true"
  ppr (BoolValue False) = text "false"
  ppr (FloatValue v)    = ppr v
  ppr Checked           = text "checked"

-- | The type of a basic value.
primValueType :: PrimValue -> PrimType
primValueType (IntValue v)   = IntType $ intValueType v
primValueType (FloatValue v) = FloatType $ floatValueType v
primValueType BoolValue{}    = Bool
primValueType Checked        = Cert

-- | A "blank" value of the given primitive type - this is zero, or
-- whatever is close to it.  Don't depend on this value, but use it
-- for e.g. creating arrays to be populated by do-loops.
blankPrimValue :: PrimType -> PrimValue
blankPrimValue (IntType Int8)      = IntValue $ Int8Value 0
blankPrimValue (IntType Int16)     = IntValue $ Int16Value 0
blankPrimValue (IntType Int32)     = IntValue $ Int32Value 0
blankPrimValue (IntType Int64)     = IntValue $ Int64Value 0
blankPrimValue (FloatType Float32) = FloatValue $ Float32Value 0.0
blankPrimValue (FloatType Float64) = FloatValue $ Float64Value 0.0
blankPrimValue Bool                = BoolValue False
blankPrimValue Cert                = Checked

-- | Various unary operators.  It is a bit ad-hoc what is a unary
-- operator and what is a built-in function.  Perhaps these should all
-- go away eventually.
data UnOp = Not -- ^ E.g., @! True == False@.
          | Complement IntType -- ^ E.g., @~(~1) = 1@.
          | Abs IntType -- ^ @abs(-2) = 2@.
          | FAbs FloatType -- ^ @fabs(-2.0) = 2.0@.
          | SSignum IntType -- ^ Signed sign function: @ssignum(-2)@ = -1.
          | USignum IntType -- ^ Unsigned sign function: @usignum(2)@ = 1.
             deriving (Eq, Ord, Show)

-- | Binary operators.  These correspond closely to the binary operators in
-- LLVM.  Most are parametrised by their expected input and output
-- types.
data BinOp = Add IntType -- ^ Integer addition.
           | FAdd FloatType -- ^ Floating-point addition.

           | Sub IntType -- ^ Integer subtraction.
           | FSub FloatType -- ^ Floating-point subtraction.

           | Mul IntType -- ^ Integer multiplication.
           | FMul FloatType -- ^ Floating-point multiplication.

           | UDiv IntType
             -- ^ Unsigned integer division.  Rounds towards
             -- negativity infinity.  Note: this is different
             -- from LLVM.
           | SDiv IntType
             -- ^ Signed integer division.  Rounds towards
             -- negativity infinity.  Note: this is different
             -- from LLVM.
           | FDiv FloatType -- ^ Floating-point division.
           | FMod FloatType -- ^ Floating-point modulus.

           | UMod IntType
             -- ^ Unsigned integer modulus; the countepart to 'UDiv'.
           | SMod IntType
             -- ^ Signed integer modulus; the countepart to 'SDiv'.

           | SQuot IntType
             -- ^ Signed integer division.  Rounds towards zero.
             -- This corresponds to the @sdiv@ instruction in LLVM.
           | SRem IntType
             -- ^ Signed integer division.  Rounds towards zero.
             -- This corresponds to the @srem@ instruction in LLVM.

           | SMin IntType
             -- ^ Returns the smallest of two signed integers.
           | UMin IntType
             -- ^ Returns the smallest of two unsigned integers.
           | FMin FloatType
             -- ^ Returns the smallest of two floating-point numbers.
           | SMax IntType
             -- ^ Returns the greatest of two signed integers.
           | UMax IntType
             -- ^ Returns the greatest of two unsigned integers.
           | FMax FloatType
             -- ^ Returns the greatest of two floating-point numbers.

           | Shl IntType -- ^ Left-shift.
           | LShr IntType -- ^ Logical right-shift, zero-extended.
           | AShr IntType -- ^ Arithmetic right-shift, sign-extended.

           | And IntType -- ^ Bitwise and.
           | Or IntType -- ^ Bitwise or.
           | Xor IntType -- ^ Bitwise exclusive-or.

           | Pow IntType -- ^ Integer exponentiation.
           | FPow FloatType -- ^ Floating-point exponentiation.

           | LogAnd -- ^ Boolean and - not short-circuiting.
           | LogOr -- ^ Boolean or - not short-circuiting.
             deriving (Eq, Ord, Show)

-- | Comparison operators are like 'BinOp's, but they return 'Bool's.
-- The somewhat ugly constructor names are straight out of LLVM.
data CmpOp = CmpEq PrimType -- ^ All types equality.
           | CmpUlt IntType -- ^ Unsigned less than.
           | CmpUle IntType -- ^ Unsigned less than or equal.
           | CmpSlt IntType -- ^ Signed less than.
           | CmpSle IntType -- ^ Signed less than or equal.

             -- Comparison operators for floating-point values.  TODO: extend
             -- this to handle NaNs and such, like the LLVM fcmp instruction.
           | FCmpLt FloatType -- ^ Floating-point less than.
           | FCmpLe FloatType -- ^ Floating-point less than or equal.

           -- Boolean comparison.
           | CmpLlt -- ^ Boolean less than.
           | CmpLle -- ^ Boolean less than or equal.
             deriving (Eq, Ord, Show)

-- | Conversion operators try to generalise the @from t0 x to t1@
-- instructions from LLVM.
data ConvOp = ZExt IntType IntType
              -- ^ Zero-extend the former integer type to the latter.
              -- If the new type is smaller, the result is a
              -- truncation.
            | SExt IntType IntType
              -- ^ Sign-extend the former integer type to the latter.
              -- If the new type is smaller, the result is a
              -- truncation.
            | FPConv FloatType FloatType
              -- ^ Convert value of the former floating-point type to
              -- the latter.  If the new type is smaller, the result
              -- is a truncation.
            | FPToUI FloatType IntType
              -- ^ Convert a floating-point value to the nearest
              -- unsigned integer (rounding towards zero).
            | FPToSI FloatType IntType
              -- ^ Convert a floating-point value to the nearest
              -- signed integer (rounding towards zero).
            | UIToFP IntType FloatType
              -- ^ Convert an unsigned integer to a floating-point value.
            | SIToFP IntType FloatType
              -- ^ Convert a signed integer to a floating-point value.
            | IToB IntType
              -- ^ Convert an integer to a boolean value.  Zero
              -- becomes false; anything else is true.
            | BToI IntType
              -- ^ Convert a boolean to an integer.  True is converted
              -- to 1 and False to 0.
             deriving (Eq, Ord, Show)

-- | A list of all unary operators for all types.
allUnOps :: [UnOp]
allUnOps = Not :
           map Complement [minBound..maxBound] ++
           map Abs [minBound..maxBound] ++
           map FAbs [minBound..maxBound] ++
           map SSignum [minBound..maxBound] ++
           map USignum [minBound..maxBound]

-- | A list of all binary operators for all types.
allBinOps :: [BinOp]
allBinOps = concat [ map Add allIntTypes
                   , map FAdd allFloatTypes
                   , map Sub allIntTypes
                   , map FSub allFloatTypes
                   , map Mul allIntTypes
                   , map FMul allFloatTypes
                   , map UDiv allIntTypes
                   , map SDiv allIntTypes
                   , map FDiv allFloatTypes
                   , map FMod allFloatTypes
                   , map UMod allIntTypes
                   , map SMod allIntTypes
                   , map SQuot allIntTypes
                   , map SRem allIntTypes
                   , map SMin allIntTypes
                   , map UMin allIntTypes
                   , map FMin allFloatTypes
                   , map SMax allIntTypes
                   , map UMax allIntTypes
                   , map FMax allFloatTypes
                   , map Shl allIntTypes
                   , map LShr allIntTypes
                   , map AShr allIntTypes
                   , map And allIntTypes
                   , map Or allIntTypes
                   , map Xor allIntTypes
                   , map Pow allIntTypes
                   , map FPow allFloatTypes
                   , [LogAnd, LogOr]
                   ]

-- | A list of all comparison operators for all types.
allCmpOps :: [CmpOp]
allCmpOps = concat [ map CmpEq allPrimTypes
                   , map CmpUlt allIntTypes
                   , map CmpUle allIntTypes
                   , map CmpSlt allIntTypes
                   , map CmpSle allIntTypes
                   , map FCmpLt allFloatTypes
                   , map FCmpLe allFloatTypes
                   ]

-- | A list of all conversion operators for all types.
allConvOps :: [ConvOp]
allConvOps = concat [ ZExt <$> allIntTypes <*> allIntTypes
                    , SExt <$> allIntTypes <*> allIntTypes
                    , FPConv <$> allFloatTypes <*> allFloatTypes
                    , FPToUI <$> allFloatTypes <*> allIntTypes
                    , FPToSI <$> allFloatTypes <*> allIntTypes
                    , UIToFP <$> allIntTypes <*> allFloatTypes
                    , SIToFP <$> allIntTypes <*> allFloatTypes
                    , IToB <$> allIntTypes
                    , BToI <$> allIntTypes
                    ]

doUnOp :: UnOp -> PrimValue -> Maybe PrimValue
doUnOp Not (BoolValue b)         = Just $ BoolValue $ not b
doUnOp Complement{} (IntValue v) = Just $ IntValue $ doComplement v
doUnOp Abs{} (IntValue v)        = Just $ IntValue $ doAbs v
doUnOp FAbs{} (FloatValue v)     = Just $ FloatValue $ doFAbs v
doUnOp SSignum{} (IntValue v)    = Just $ IntValue $ doSSignum v
doUnOp USignum{} (IntValue v)    = Just $ IntValue $ doUSignum v
doUnOp _ _                       = Nothing

-- | E.g., @~(~1) = 1@.
doComplement :: IntValue -> IntValue
doComplement v = intValue (intValueType v) $ complement $ intToInt64 v

-- | @abs(-2) = 2@.
doAbs :: IntValue -> IntValue
doAbs v = intValue (intValueType v) $ abs $ intToInt64 v

-- | @abs(-2.0) = 2.0@.
doFAbs :: FloatValue -> FloatValue
doFAbs v = floatValue (floatValueType v) $ abs $ floatToDouble v

-- | @ssignum(-2)@ = -1.
doSSignum :: IntValue -> IntValue
doSSignum v = intValue (intValueType v) $ signum $ intToInt64 v

-- | @usignum(-2)@ = -1.
doUSignum :: IntValue -> IntValue
doUSignum v = intValue (intValueType v) $ signum $ intToWord64 v

doBinOp :: BinOp -> PrimValue -> PrimValue -> Maybe PrimValue
doBinOp Add{}    = doIntBinOp doAdd
doBinOp FAdd{}   = doFloatBinOp (+) (+)
doBinOp Sub{}    = doIntBinOp doSub
doBinOp FSub{}   = doFloatBinOp (-) (-)
doBinOp Mul{}    = doIntBinOp doMul
doBinOp FMul{}   = doFloatBinOp (*) (*)
doBinOp UDiv{}   = doRiskyIntBinOp doUDiv
doBinOp SDiv{}   = doRiskyIntBinOp doSDiv
doBinOp FDiv{}   = doFloatBinOp (/) (/)
doBinOp FMod{}   = doFloatBinOp mod' mod'
doBinOp UMod{}   = doRiskyIntBinOp doUMod
doBinOp SMod{}   = doRiskyIntBinOp doSMod
doBinOp SQuot{}  = doRiskyIntBinOp doSQuot
doBinOp SRem{}   = doRiskyIntBinOp doSRem
doBinOp SMin{}   = doIntBinOp doSMin
doBinOp UMin{}   = doIntBinOp doUMin
doBinOp FMin{}   = doFloatBinOp min min
doBinOp SMax{}   = doIntBinOp doSMax
doBinOp UMax{}   = doIntBinOp doUMax
doBinOp FMax{}   = doFloatBinOp max max
doBinOp Shl{}    = doIntBinOp doShl
doBinOp LShr{}   = doIntBinOp doLShr
doBinOp AShr{}   = doIntBinOp doAShr
doBinOp And{}    = doIntBinOp doAnd
doBinOp Or{}     = doIntBinOp doOr
doBinOp Xor{}    = doIntBinOp doXor
doBinOp Pow{}    = doRiskyIntBinOp doPow
doBinOp FPow{}   = doFloatBinOp (**) (**)
doBinOp LogAnd{} = doBoolBinOp (&&)
doBinOp LogOr{}  = doBoolBinOp (||)

doIntBinOp :: (IntValue -> IntValue -> IntValue) -> PrimValue -> PrimValue
           -> Maybe PrimValue
doIntBinOp f (IntValue v1) (IntValue v2) =
  Just $ IntValue $ f v1 v2
doIntBinOp _ _ _ = Nothing

doRiskyIntBinOp :: (IntValue -> IntValue -> Maybe IntValue) -> PrimValue -> PrimValue
           -> Maybe PrimValue
doRiskyIntBinOp f (IntValue v1) (IntValue v2) =
  IntValue <$> f v1 v2
doRiskyIntBinOp _ _ _ = Nothing

doFloatBinOp :: (Float -> Float -> Float)
             -> (Double -> Double -> Double)
             -> PrimValue -> PrimValue
             -> Maybe PrimValue
doFloatBinOp f32 _ (FloatValue (Float32Value v1)) (FloatValue (Float32Value v2)) =
  Just $ FloatValue $ Float32Value $ f32 v1 v2
doFloatBinOp _ f64 (FloatValue (Float64Value v1)) (FloatValue (Float64Value v2)) =
  Just $ FloatValue $ Float64Value $ f64 v1 v2
doFloatBinOp _ _ _ _ = Nothing

doBoolBinOp :: (Bool -> Bool -> Bool) -> PrimValue -> PrimValue
            -> Maybe PrimValue
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

-- | Unsigned integer division.  Rounds towards
-- negativity infinity.  Note: this is different
-- from LLVM.
doUDiv :: IntValue -> IntValue -> Maybe IntValue
doUDiv v1 v2
  | zeroIshInt v2 = Nothing
  | otherwise = Just $ intValue (intValueType v1) $ intToWord64 v1 `div` intToWord64 v2

-- | Signed integer division.  Rounds towards
-- negativity infinity.  Note: this is different
-- from LLVM.
doSDiv :: IntValue -> IntValue -> Maybe IntValue
doSDiv v1 v2
  | zeroIshInt v2 = Nothing
  | otherwise = Just $ intValue (intValueType v1) $ intToInt64 v1 `div` intToInt64 v2

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
  | otherwise         = Just $ intValue (intValueType v1) $ intToInt64 v1 ^ intToInt64 v2

doConvOp :: ConvOp -> PrimValue -> Maybe PrimValue
doConvOp (ZExt _ to) (IntValue v)     = Just $ IntValue $ doZExt v to
doConvOp (SExt _ to) (IntValue v)     = Just $ IntValue $ doSExt v to
doConvOp (FPConv _ to) (FloatValue v) = Just $ FloatValue $ doFPConv v to
doConvOp (FPToUI _ to) (FloatValue v) = Just $ IntValue $ doFPToUI v to
doConvOp (FPToSI _ to) (FloatValue v) = Just $ IntValue $ doFPToSI v to
doConvOp (UIToFP _ to) (IntValue v)   = Just $ FloatValue $ doUIToFP v to
doConvOp (SIToFP _ to) (IntValue v)   = Just $ FloatValue $ doSIToFP v to
doConvOp (IToB _) (IntValue v)        = Just $ BoolValue $ intToInt64 v /= 0
doConvOp (BToI to) (BoolValue v)      = Just $ IntValue $ intValue to $ if v then 1 else 0::Int
doConvOp _ _                          = Nothing

-- | Zero-extend the given integer value to the size of the given
-- type.  If the type is smaller than the given value, the result is a
-- truncation.
doZExt :: IntValue -> IntType -> IntValue
doZExt (Int8Value x) t  = intValue t $ toInteger (fromIntegral x :: Word8)
doZExt (Int16Value x) t = intValue t $ toInteger (fromIntegral x :: Word16)
doZExt (Int32Value x) t = intValue t $ toInteger (fromIntegral x :: Word32)
doZExt (Int64Value x) t = intValue t $ toInteger (fromIntegral x :: Word64)

-- | Sign-extend the given integer value to the size of the given
-- type.  If the type is smaller than the given value, the result is a
-- truncation.
doSExt :: IntValue -> IntType -> IntValue
doSExt (Int8Value x) t  = intValue t $ toInteger x
doSExt (Int16Value x) t = intValue t $ toInteger x
doSExt (Int32Value x) t = intValue t $ toInteger x
doSExt (Int64Value x) t = intValue t $ toInteger x

-- | Convert the former floating-point type to the latter.
doFPConv :: FloatValue -> FloatType -> FloatValue
doFPConv (Float32Value v) Float32 = Float32Value v
doFPConv (Float64Value v) Float32 = Float32Value $ fromRational $ toRational v
doFPConv (Float64Value v) Float64 = Float64Value v
doFPConv (Float32Value v) Float64 = Float64Value $ fromRational $ toRational v

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

doCmpOp :: CmpOp -> PrimValue -> PrimValue -> Maybe Bool
doCmpOp CmpEq{} v1 v2                            = Just $ v1 == v2
doCmpOp CmpUlt{} (IntValue v1) (IntValue v2)     = Just $ doCmpUlt v1 v2
doCmpOp CmpUle{} (IntValue v1) (IntValue v2)     = Just $ doCmpUle v1 v2
doCmpOp CmpSlt{} (IntValue v1) (IntValue v2)     = Just $ doCmpSlt v1 v2
doCmpOp CmpSle{} (IntValue v1) (IntValue v2)     = Just $ doCmpSle v1 v2
doCmpOp FCmpLt{} (FloatValue v1) (FloatValue v2) = Just $ doFCmpLt v1 v2
doCmpOp FCmpLe{} (FloatValue v1) (FloatValue v2) = Just $ doFCmpLe v1 v2
doCmpOp CmpLlt{} (BoolValue v1) (BoolValue v2)   = Just $ not v1 && v2
doCmpOp CmpLle{} (BoolValue v1) (BoolValue v2)   = Just $ not (v1 && not v2)
doCmpOp _ _ _                                    = Nothing

-- | Compare any two primtive values for exact equality.
doCmpEq :: PrimValue -> PrimValue -> Bool
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

-- | Translate an 'IntValue' to 'Word64'.  This is guaranteed to fit.
intToWord64 :: IntValue -> Word64
intToWord64 (Int8Value v)  = fromIntegral (fromIntegral v :: Word8)
intToWord64 (Int16Value v) = fromIntegral (fromIntegral v :: Word16)
intToWord64 (Int32Value v) = fromIntegral (fromIntegral v :: Word32)
intToWord64 (Int64Value v) = fromIntegral (fromIntegral v :: Word64)

-- | Translate an 'IntValue' to 'Int64'.  This is guaranteed to fit.
intToInt64 :: IntValue -> Int64
intToInt64 (Int8Value v)  = fromIntegral v
intToInt64 (Int16Value v) = fromIntegral v
intToInt64 (Int32Value v) = fromIntegral v
intToInt64 (Int64Value v) = fromIntegral v

-- | Careful - there is no guarantee this will fit.
intToInt :: IntValue -> Int
intToInt = fromIntegral . intToInt64

floatToDouble :: FloatValue -> Double
floatToDouble (Float32Value v) = fromRational $ toRational v
floatToDouble (Float64Value v) = v

-- | The result type of a binary operator.
binOpType :: BinOp -> PrimType
binOpType (Add t)   = IntType t
binOpType (Sub t)   = IntType t
binOpType (Mul t)   = IntType t
binOpType (SDiv t)  = IntType t
binOpType (SMod t)  = IntType t
binOpType (SQuot t) = IntType t
binOpType (SRem t)  = IntType t
binOpType (UDiv t)  = IntType t
binOpType (UMod t)  = IntType t
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
unOpType (SSignum t)    = IntType t
unOpType (USignum t)    = IntType t
unOpType Not            = Bool
unOpType (Complement t) = IntType t
unOpType (Abs t)        = IntType t
unOpType (FAbs t)       = FloatType t

-- | The input and output types of a conversion operator.
convOpType :: ConvOp -> (PrimType, PrimType)
convOpType (ZExt from to) = (IntType from, IntType to)
convOpType (SExt from to) = (IntType from, IntType to)
convOpType (FPConv from to) = (FloatType from, FloatType to)
convOpType (FPToUI from to) = (FloatType from, IntType to)
convOpType (FPToSI from to) = (FloatType from, IntType to)
convOpType (UIToFP from to) = (IntType from, FloatType to)
convOpType (SIToFP from to) = (IntType from, FloatType to)
convOpType (IToB from) = (IntType from, Bool)
convOpType (BToI to) = (Bool, IntType to)

-- | A mapping from names of primitive functions to their parameter
-- types, their result type, and a function for evaluating them.
primFuns :: M.Map String ([PrimType], PrimType,
                          [PrimValue] -> Maybe PrimValue)
primFuns = M.fromList
  [ f32 "sqrt32" sqrt, f64 "sqrt64" sqrt
  , f32 "log32" log, f64 "log64" log
  , f32 "log10_32" (logBase 10), f64 "log10_64" (logBase 10)
  , f32 "log2_32" (logBase 2), f64 "log2_64" (logBase 2)
  , f32 "exp32" exp, f64 "exp64" exp
  , f32 "sin32" sin, f64 "sin64" sin
  , f32 "cos32" cos, f64 "cos64" cos
  , f32 "tan32" tan, f64 "tan64" tan
  , f32 "asin32" asin, f64 "asin64" asin
  , f32 "acos32" acos, f64 "acos64" acos
  , f32 "atan32" atan, f64 "atan64" atan
  , f32 "round32" roundFloat, f64 "round64" roundDouble
  , f32 "ceil32" ceilFloat, f64 "ceil64" ceilDouble
  , f32 "floor32" floorFloat, f64 "floor64" floorDouble
  , f32 "gamma32" tgammaf, f64 "gamma64" tgamma
  , f32 "lgamma32" lgammaf, f64 "lgamma64" lgamma

  , ("atan2_32",
     ([FloatType Float32, FloatType Float32], FloatType Float32,
      \case
        [FloatValue (Float32Value x), FloatValue (Float32Value y)] ->
          Just $ FloatValue $ Float32Value $ atan2 x y
        _ -> Nothing))
  , ("atan2_64",
     ([FloatType Float64, FloatType Float64], FloatType Float64,
       \case
         [FloatValue (Float64Value x), FloatValue (Float64Value y)] ->
           Just $ FloatValue $ Float64Value $ atan2 x y
         _ -> Nothing))

  , ("isinf32",
     ([FloatType Float32], Bool,
      \case
        [FloatValue (Float32Value x)] -> Just $ BoolValue $ isInfinite x
        _ -> Nothing))
  , ("isinf64",
     ([FloatType Float64], Bool,
      \case
        [FloatValue (Float64Value x)] -> Just $ BoolValue $ isInfinite x
        _ -> Nothing))

  , ("isnan32",
     ([FloatType Float32], Bool,
      \case
        [FloatValue (Float32Value x)] -> Just $ BoolValue $ isNaN x
        _ -> Nothing))
  , ("isnan64",
     ([FloatType Float64], Bool,
      \case
        [FloatValue (Float64Value x)] -> Just $ BoolValue $ isNaN x
        _ -> Nothing))

  , ("to_bits32",
     ([FloatType Float32], IntType Int32,
      \case
        [FloatValue (Float32Value x)] ->
          Just $ IntValue $ Int32Value $ fromIntegral $ floatToWord x
        _ -> Nothing))
  , ("to_bits64",
     ([FloatType Float64], IntType Int64,
      \case
        [FloatValue (Float64Value x)] ->
          Just $ IntValue $ Int64Value $ fromIntegral $ doubleToWord x
        _ -> Nothing))

  , ("from_bits32",
     ([IntType Int32], FloatType Float32,
      \case
        [IntValue (Int32Value x)] ->
          Just $ FloatValue $ Float32Value $ wordToFloat $ fromIntegral x
        _ -> Nothing))
  , ("from_bits64",
     ([IntType Int64], FloatType Float64,
      \case
        [IntValue (Int64Value x)] ->
          Just $ FloatValue $ Float64Value $ wordToDouble $ fromIntegral x
        _ -> Nothing))

  , ("lerp32",
     ([FloatType Float32, FloatType Float32, FloatType Float32], FloatType Float32,
      \case
        [FloatValue (Float32Value v0),
         FloatValue (Float32Value v1),
         FloatValue (Float32Value t)] ->
          Just $ FloatValue $ Float32Value $
          v0 + (v1-v0)*max 0 (min 1 t)
        _ -> Nothing))
  , ("lerp64",
     ([FloatType Float64, FloatType Float64, FloatType Float64], FloatType Float64,
      \case
        [FloatValue (Float64Value v0),
         FloatValue (Float64Value v1),
         FloatValue (Float64Value t)] ->
          Just $ FloatValue $ Float64Value $
          v0 + (v1-v0)*max 0 (min 1 t)
        _ -> Nothing))
  ]
  where f32 s f = (s, ([FloatType Float32], FloatType Float32, f32PrimFun f))
        f64 s f = (s, ([FloatType Float64], FloatType Float64, f64PrimFun f))

        f32PrimFun f [FloatValue (Float32Value x)] =
          Just $ FloatValue $ Float32Value $ f x
        f32PrimFun _ _ = Nothing

        f64PrimFun f [FloatValue (Float64Value x)] =
          Just $ FloatValue $ Float64Value $ f x
        f64PrimFun _ _ = Nothing

-- | Is the given value kind of zero?
zeroIsh :: PrimValue -> Bool
zeroIsh (IntValue k)                  = zeroIshInt k
zeroIsh (FloatValue (Float32Value k)) = k == 0
zeroIsh (FloatValue (Float64Value k)) = k == 0
zeroIsh (BoolValue False)             = True
zeroIsh _                             = False

-- | Is the given value kind of one?
oneIsh :: PrimValue -> Bool
oneIsh (IntValue (Int8Value k))      = k == 1
oneIsh (IntValue (Int16Value k))     = k == 1
oneIsh (IntValue (Int32Value k))     = k == 1
oneIsh (IntValue (Int64Value k))     = k == 1
oneIsh (FloatValue (Float32Value k)) = k == 1
oneIsh (FloatValue (Float64Value k)) = k == 1
oneIsh (BoolValue True)              = True
oneIsh _                             = False

-- | Is the given value kind of negative?
negativeIsh :: PrimValue -> Bool
negativeIsh (IntValue k)                  = negativeIshInt k
negativeIsh (FloatValue (Float32Value k)) = k < 0
negativeIsh (FloatValue (Float64Value k)) = k < 0
negativeIsh (BoolValue _)                 = False
negativeIsh Checked                       = False

-- | Is the given integer value kind of zero?
zeroIshInt :: IntValue -> Bool
zeroIshInt (Int8Value k)  = k == 0
zeroIshInt (Int16Value k) = k == 0
zeroIshInt (Int32Value k) = k == 0
zeroIshInt (Int64Value k) = k == 0

-- | Is the given integer value kind of negative?
negativeIshInt :: IntValue -> Bool
negativeIshInt (Int8Value k)  = k < 0
negativeIshInt (Int16Value k) = k < 0
negativeIshInt (Int32Value k) = k < 0
negativeIshInt (Int64Value k) = k < 0

-- | The size of a value of a given primitive type in bites.
primBitSize :: PrimType -> Int
primBitSize = (*8) . primByteSize

-- | The size of a value of a given primitive type in eight-bit bytes.
primByteSize :: Num a => PrimType -> a
primByteSize (IntType t)   = intByteSize t
primByteSize (FloatType t) = floatByteSize t
primByteSize Bool          = 1
primByteSize Cert          = 1

-- | The size of a value of a given integer type in eight-bit bytes.
intByteSize :: Num a => IntType -> a
intByteSize Int8  = 1
intByteSize Int16 = 2
intByteSize Int32 = 4
intByteSize Int64 = 8

-- | The size of a value of a given floating-point type in eight-bit bytes.
floatByteSize :: Num a => FloatType -> a
floatByteSize Float32 = 4
floatByteSize Float64 = 8

-- | True if the given binary operator is commutative.
commutativeBinOp :: BinOp -> Bool
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

-- Prettyprinting instances

instance Pretty BinOp where
  ppr (Add t)   = taggedI "add" t
  ppr (FAdd t)  = taggedF "fadd" t
  ppr (Sub t)   = taggedI "sub" t
  ppr (FSub t)  = taggedF "fsub" t
  ppr (Mul t)   = taggedI "mul" t
  ppr (FMul t)  = taggedF "fmul" t
  ppr (UDiv t)  = taggedI "udiv" t
  ppr (UMod t)  = taggedI "umod" t
  ppr (SDiv t)  = taggedI "sdiv" t
  ppr (SMod t)  = taggedI "smod" t
  ppr (SQuot t) = taggedI "squot" t
  ppr (SRem t)  = taggedI "srem" t
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

instance Pretty CmpOp where
  ppr (CmpEq t)  = text "eq_" <> ppr t
  ppr (CmpUlt t) = taggedI "ult" t
  ppr (CmpUle t) = taggedI "ule" t
  ppr (CmpSlt t) = taggedI "slt" t
  ppr (CmpSle t) = taggedI "sle" t
  ppr (FCmpLt t) = taggedF "lt" t
  ppr (FCmpLe t) = taggedF "le" t
  ppr CmpLlt = text "llt"
  ppr CmpLle = text "lle"

instance Pretty ConvOp where
  ppr op = convOp (convOpFun op) from to
    where (from, to) = convOpType op

instance Pretty UnOp where
  ppr Not            = text "!"
  ppr (Abs t)        = taggedI "abs" t
  ppr (FAbs t)       = taggedF "fabs" t
  ppr (SSignum t)    = taggedI "ssignum" t
  ppr (USignum t)    = taggedI "usignum" t
  ppr (Complement t) = taggedI "complement" t

convOpFun :: ConvOp -> String
convOpFun ZExt{}   = "zext"
convOpFun SExt{}   = "sext"
convOpFun FPConv{} = "fpconv"
convOpFun FPToUI{} = "fptoui"
convOpFun FPToSI{} = "fptosi"
convOpFun UIToFP{} = "uitofp"
convOpFun SIToFP{} = "sitofp"
convOpFun IToB{} = "itob"
convOpFun BToI{} = "btoi"

taggedI :: String -> IntType -> Doc
taggedI s Int8  = text $ s ++ "8"
taggedI s Int16 = text $ s ++ "16"
taggedI s Int32 = text $ s ++ "32"
taggedI s Int64 = text $ s ++ "64"

taggedF :: String -> FloatType -> Doc
taggedF s Float32 = text $ s ++ "32"
taggedF s Float64 = text $ s ++ "64"

convOp :: (Pretty from, Pretty to) => String -> from -> to -> Doc
convOp s from to = text s <> text "_" <> ppr from <> text "_" <> ppr to

-- | True if signed.  Only makes a difference for integer types.
prettySigned :: Bool -> PrimType -> String
prettySigned True (IntType it) = 'u' : drop 1 (pretty it)
prettySigned _ t = pretty t
