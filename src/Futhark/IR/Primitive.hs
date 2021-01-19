{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

-- | Definitions of primitive types, the values that inhabit these
-- types, and operations on these values.  A primitive value can also
-- be called a scalar.
--
-- Essentially, this module describes the subset of the (internal)
-- Futhark language that operates on primitive types.
module Futhark.IR.Primitive
  ( -- * Types
    IntType (..),
    allIntTypes,
    FloatType (..),
    allFloatTypes,
    PrimType (..),
    allPrimTypes,
    module Data.Int,

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

    -- * Prettyprinting
    convOpFun,
    prettySigned,
  )
where

import Control.Category
import qualified Data.Binary.Get as G
import qualified Data.Binary.Put as P
import Data.Bits
import Data.Fixed (mod') -- Weird location.
import Data.Int (Int16, Int32, Int64, Int8)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Word
import Futhark.Util
  ( ceilDouble,
    ceilFloat,
    floorDouble,
    floorFloat,
    lgamma,
    lgammaf,
    roundDouble,
    roundFloat,
    tgamma,
    tgammaf,
  )
import Futhark.Util.Pretty
import GHC.Generics (Generic)
import Language.SexpGrammar as Sexp
import Language.SexpGrammar.Generic
import Text.Read (readMaybe)
import Prelude hiding (id, (.))

-- | An integer type, ordered by size.  Note that signedness is not a
-- property of the type, but a property of the operations performed on
-- values of these types.
data IntType
  = Int8
  | Int16
  | Int32
  | Int64
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance SexpIso IntType where
  sexpIso =
    match $
      With (sym "i8" >>>) $
        With (sym "i16" >>>) $
          With (sym "i32" >>>) $
            With
              (sym "i64" >>>)
              End

instance Pretty IntType where
  ppr Int8 = text "i8"
  ppr Int16 = text "i16"
  ppr Int32 = text "i32"
  ppr Int64 = text "i64"

-- | A list of all integer types.
allIntTypes :: [IntType]
allIntTypes = [minBound .. maxBound]

-- | A floating point type.
data FloatType
  = Float32
  | Float64
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance SexpIso FloatType where
  sexpIso =
    match $
      With (sym "f32" >>>) $
        With
          (sym "f64" >>>)
          End

instance Pretty FloatType where
  ppr Float32 = text "f32"
  ppr Float64 = text "f64"

-- | A list of all floating-point types.
allFloatTypes :: [FloatType]
allFloatTypes = [minBound .. maxBound]

-- | Low-level primitive types.
data PrimType
  = IntType IntType
  | FloatType FloatType
  | Bool
  | Cert
  deriving (Eq, Ord, Show, Generic)

instance SexpIso PrimType where
  sexpIso =
    match $
      With (. sexpIso) $
        With (. sexpIso) $
          With (sym "bool" >>>) $
            With
              (sym "cert" >>>)
              End

instance Enum PrimType where
  toEnum 0 = IntType Int8
  toEnum 1 = IntType Int16
  toEnum 2 = IntType Int32
  toEnum 3 = IntType Int64
  toEnum 4 = FloatType Float32
  toEnum 5 = FloatType Float64
  toEnum 6 = Bool
  toEnum _ = Cert

  fromEnum (IntType Int8) = 0
  fromEnum (IntType Int16) = 1
  fromEnum (IntType Int32) = 2
  fromEnum (IntType Int64) = 3
  fromEnum (FloatType Float32) = 4
  fromEnum (FloatType Float64) = 5
  fromEnum Bool = 6
  fromEnum Cert = 7

instance Bounded PrimType where
  minBound = IntType Int8
  maxBound = Cert

instance Pretty PrimType where
  ppr (IntType t) = ppr t
  ppr (FloatType t) = ppr t
  ppr Bool = text "bool"
  ppr Cert = text "cert"

-- | A list of all primitive types.
allPrimTypes :: [PrimType]
allPrimTypes =
  map IntType allIntTypes
    ++ map FloatType allFloatTypes
    ++ [Bool, Cert]

numberIso :: (Read a, Pretty a) => T.Text -> Grammar p (T.Text :- t) (a :- t)
numberIso postfix = partialOsi (fromS postfix) (toS postfix)
  where
    toS t s = prettyText s <> t

    fromS t s
      | t `T.isSuffixOf` s,
        Just v <- readMaybe $ T.unpack $ T.dropEnd (T.length t) s =
        Right v
      | otherwise =
        Left $ expected $ "Couldn't parse " <> t

-- | An integer value.
data IntValue
  = Int8Value !Int8
  | Int16Value !Int16
  | Int32Value !Int32
  | Int64Value !Int64
  deriving (Eq, Ord, Show, Generic)

instance SexpIso IntValue where
  sexpIso =
    match $
      With (. numberIso "i8" . Sexp.symbol) $
        With (. numberIso "i16" . Sexp.symbol) $
          With (. numberIso "i32" . Sexp.symbol) $
            With
              (. numberIso "i64" . Sexp.symbol)
              End

instance Pretty IntValue where
  ppr (Int8Value v) = text $ show v ++ "i8"
  ppr (Int16Value v) = text $ show v ++ "i16"
  ppr (Int32Value v) = text $ show v ++ "i32"
  ppr (Int64Value v) = text $ show v ++ "i64"

-- | Create an t'IntValue' from a type and an 'Integer'.
intValue :: Integral int => IntType -> int -> IntValue
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
valueIntegral :: Integral int => IntValue -> int
valueIntegral (Int8Value v) = fromIntegral v
valueIntegral (Int16Value v) = fromIntegral v
valueIntegral (Int32Value v) = fromIntegral v
valueIntegral (Int64Value v) = fromIntegral v

-- | A floating-point value.
data FloatValue
  = Float32Value !Float
  | Float64Value !Double
  deriving (Show, Generic)

instance Eq FloatValue where
  Float32Value x == Float32Value y = isNaN x && isNaN y || x == y
  Float64Value x == Float64Value y = isNaN x && isNaN y || x == y
  Float32Value _ == Float64Value _ = False
  Float64Value _ == Float32Value _ = False

-- The derived Ord instance does not handle NaNs correctly.
instance Ord FloatValue where
  Float32Value x <= Float32Value y = x <= y
  Float64Value x <= Float64Value y = x <= y
  Float32Value _ <= Float64Value _ = True
  Float64Value _ <= Float32Value _ = False

  Float32Value x < Float32Value y = x < y
  Float64Value x < Float64Value y = x < y
  Float32Value _ < Float64Value _ = True
  Float64Value _ < Float32Value _ = False

  (>) = flip (<)
  (>=) = flip (<=)

instance SexpIso FloatValue where
  sexpIso =
    match $
      With (. numberIso "f32" . Sexp.symbol) $
        With
          (. numberIso "f64" . Sexp.symbol)
          End

instance Pretty FloatValue where
  ppr (Float32Value v)
    | isInfinite v, v >= 0 = text "f32.inf"
    | isInfinite v, v < 0 = text "-f32.inf"
    | isNaN v = text "f32.nan"
    | otherwise = text $ show v ++ "f32"
  ppr (Float64Value v)
    | isInfinite v, v >= 0 = text "f64.inf"
    | isInfinite v, v < 0 = text "-f64.inf"
    | isNaN v = text "f64.nan"
    | otherwise = text $ show v ++ "f64"

-- | Create a t'FloatValue' from a type and a 'Rational'.
floatValue :: Real num => FloatType -> num -> FloatValue
floatValue Float32 = Float32Value . fromRational . toRational
floatValue Float64 = Float64Value . fromRational . toRational

-- | The type of a floating-point value.
floatValueType :: FloatValue -> FloatType
floatValueType Float32Value {} = Float32
floatValueType Float64Value {} = Float64

-- | Non-array values.
data PrimValue
  = IntValue !IntValue
  | FloatValue !FloatValue
  | BoolValue !Bool
  | -- | The only value of type @cert@.
    Checked
  deriving (Eq, Ord, Show, Generic)

instance SexpIso PrimValue where
  sexpIso =
    match $
      With (. sexpIso) $
        With (. sexpIso) $
          With (. sexpIso) $
            With
              (sym "checked" >>>)
              End

instance Pretty PrimValue where
  ppr (IntValue v) = ppr v
  ppr (BoolValue True) = text "true"
  ppr (BoolValue False) = text "false"
  ppr (FloatValue v) = ppr v
  ppr Checked = text "checked"

-- | The type of a basic value.
primValueType :: PrimValue -> PrimType
primValueType (IntValue v) = IntType $ intValueType v
primValueType (FloatValue v) = FloatType $ floatValueType v
primValueType BoolValue {} = Bool
primValueType Checked = Cert

-- | A "blank" value of the given primitive type - this is zero, or
-- whatever is close to it.  Don't depend on this value, but use it
-- for e.g. creating arrays to be populated by do-loops.
blankPrimValue :: PrimType -> PrimValue
blankPrimValue (IntType Int8) = IntValue $ Int8Value 0
blankPrimValue (IntType Int16) = IntValue $ Int16Value 0
blankPrimValue (IntType Int32) = IntValue $ Int32Value 0
blankPrimValue (IntType Int64) = IntValue $ Int64Value 0
blankPrimValue (FloatType Float32) = FloatValue $ Float32Value 0.0
blankPrimValue (FloatType Float64) = FloatValue $ Float64Value 0.0
blankPrimValue Bool = BoolValue False
blankPrimValue Cert = Checked

-- | Various unary operators.  It is a bit ad-hoc what is a unary
-- operator and what is a built-in function.  Perhaps these should all
-- go away eventually.
data UnOp
  = -- | E.g., @! True == False@.
    Not
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
  deriving (Eq, Ord, Show, Generic)

instance SexpIso UnOp where
  sexpIso =
    match $
      With (. Sexp.sym "not") $
        With (. Sexp.list (Sexp.el (Sexp.sym "complement") >>> Sexp.el sexpIso)) $
          With (. Sexp.list (Sexp.el (Sexp.sym "abs") >>> Sexp.el sexpIso)) $
            With (. Sexp.list (Sexp.el (Sexp.sym "fabs") >>> Sexp.el sexpIso)) $
              With (. Sexp.list (Sexp.el (Sexp.sym "ssignum") >>> Sexp.el sexpIso)) $
                With
                  (. Sexp.list (Sexp.el (Sexp.sym "usignum") >>> Sexp.el sexpIso))
                  End

-- | What to do in case of arithmetic overflow.  Futhark's semantics
-- are that overflow does wraparound, but for generated code (like
-- address arithmetic), it can be beneficial for overflow to be
-- undefined behaviour, as it allows better optimisation of things
-- such as GPU kernels.
--
-- Note that all values of this type are considered equal for 'Eq' and
-- 'Ord'.
data Overflow = OverflowWrap | OverflowUndef
  deriving (Show, Generic)

instance SexpIso Overflow where
  sexpIso =
    match $
      With (. Sexp.sym "wrap") $
        With
          (. Sexp.sym "undef")
          End

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
data Safety = Unsafe | Safe deriving (Eq, Ord, Show, Generic)

instance SexpIso Safety where
  sexpIso =
    match $
      With (. Sexp.sym "unsafe") $
        With
          (. Sexp.sym "safe")
          End

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
  deriving (Eq, Ord, Show, Generic)

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
  deriving (Eq, Ord, Show, Generic)

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
  | -- | Convert an integer to a boolean value.  Zero
    -- becomes false; anything else is true.
    IToB IntType
  | -- | Convert a boolean to an integer.  True is converted
    -- to 1 and False to 0.
    BToI IntType
  deriving (Eq, Ord, Show, Generic)

-- | A list of all unary operators for all types.
allUnOps :: [UnOp]
allUnOps =
  Not :
  map Complement [minBound .. maxBound]
    ++ map Abs [minBound .. maxBound]
    ++ map FAbs [minBound .. maxBound]
    ++ map SSignum [minBound .. maxBound]
    ++ map USignum [minBound .. maxBound]

-- | A list of all binary operators for all types.
allBinOps :: [BinOp]
allBinOps =
  concat
    [ map (`Add` OverflowWrap) allIntTypes,
      map FAdd allFloatTypes,
      map (`Sub` OverflowWrap) allIntTypes,
      map FSub allFloatTypes,
      map (`Mul` OverflowWrap) allIntTypes,
      map FMul allFloatTypes,
      map (`UDiv` Unsafe) allIntTypes,
      map (`UDivUp` Unsafe) allIntTypes,
      map (`SDiv` Unsafe) allIntTypes,
      map (`SDivUp` Unsafe) allIntTypes,
      map FDiv allFloatTypes,
      map FMod allFloatTypes,
      map (`UMod` Unsafe) allIntTypes,
      map (`SMod` Unsafe) allIntTypes,
      map (`SQuot` Unsafe) allIntTypes,
      map (`SRem` Unsafe) allIntTypes,
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
      map FCmpLe allFloatTypes
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
      IToB <$> allIntTypes,
      BToI <$> allIntTypes
    ]

-- | Apply an 'UnOp' to an operand.  Returns 'Nothing' if the
-- application is mistyped.
doUnOp :: UnOp -> PrimValue -> Maybe PrimValue
doUnOp Not (BoolValue b) = Just $ BoolValue $ not b
doUnOp Complement {} (IntValue v) = Just $ IntValue $ doComplement v
doUnOp Abs {} (IntValue v) = Just $ IntValue $ doAbs v
doUnOp FAbs {} (FloatValue v) = Just $ FloatValue $ doFAbs v
doUnOp SSignum {} (IntValue v) = Just $ IntValue $ doSSignum v
doUnOp USignum {} (IntValue v) = Just $ IntValue $ doUSignum v
doUnOp _ _ = Nothing

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

-- | Apply a 'BinOp' to an operand.  Returns 'Nothing' if the
-- application is mistyped, or outside the domain (e.g. division by
-- zero).
doBinOp :: BinOp -> PrimValue -> PrimValue -> Maybe PrimValue
doBinOp Add {} = doIntBinOp doAdd
doBinOp FAdd {} = doFloatBinOp (+) (+)
doBinOp Sub {} = doIntBinOp doSub
doBinOp FSub {} = doFloatBinOp (-) (-)
doBinOp Mul {} = doIntBinOp doMul
doBinOp FMul {} = doFloatBinOp (*) (*)
doBinOp UDiv {} = doRiskyIntBinOp doUDiv
doBinOp UDivUp {} = doRiskyIntBinOp doUDivUp
doBinOp SDiv {} = doRiskyIntBinOp doSDiv
doBinOp SDivUp {} = doRiskyIntBinOp doSDivUp
doBinOp FDiv {} = doFloatBinOp (/) (/)
doBinOp FMod {} = doFloatBinOp mod' mod'
doBinOp UMod {} = doRiskyIntBinOp doUMod
doBinOp SMod {} = doRiskyIntBinOp doSMod
doBinOp SQuot {} = doRiskyIntBinOp doSQuot
doBinOp SRem {} = doRiskyIntBinOp doSRem
doBinOp SMin {} = doIntBinOp doSMin
doBinOp UMin {} = doIntBinOp doUMin
doBinOp FMin {} = doFloatBinOp min min
doBinOp SMax {} = doIntBinOp doSMax
doBinOp UMax {} = doIntBinOp doUMax
doBinOp FMax {} = doFloatBinOp max max
doBinOp Shl {} = doIntBinOp doShl
doBinOp LShr {} = doIntBinOp doLShr
doBinOp AShr {} = doIntBinOp doAShr
doBinOp And {} = doIntBinOp doAnd
doBinOp Or {} = doIntBinOp doOr
doBinOp Xor {} = doIntBinOp doXor
doBinOp Pow {} = doRiskyIntBinOp doPow
doBinOp FPow {} = doFloatBinOp (**) (**)
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
  (Float -> Float -> Float) ->
  (Double -> Double -> Double) ->
  PrimValue ->
  PrimValue ->
  Maybe PrimValue
doFloatBinOp f32 _ (FloatValue (Float32Value v1)) (FloatValue (Float32Value v2)) =
  Just $ FloatValue $ Float32Value $ f32 v1 v2
doFloatBinOp _ f64 (FloatValue (Float64Value v1)) (FloatValue (Float64Value v2)) =
  Just $ FloatValue $ Float64Value $ f64 v1 v2
doFloatBinOp _ _ _ _ = Nothing

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
    Just $
      intValue (intValueType v1) $
        intToWord64 v1 `div` intToWord64 v2

-- | Unsigned integer division.  Rounds towards positive infinity.
doUDivUp :: IntValue -> IntValue -> Maybe IntValue
doUDivUp v1 v2
  | zeroIshInt v2 = Nothing
  | otherwise =
    Just $
      intValue (intValueType v1) $
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
    Just $
      intValue (intValueType v1) $
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
doConvOp (IToB _) (IntValue v) = Just $ BoolValue $ intToInt64 v /= 0
doConvOp (BToI to) (BoolValue v) = Just $ IntValue $ intValue to $ if v then 1 else 0 :: Int
doConvOp _ _ = Nothing

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
floatToDouble (Float32Value v) = fromRational $ toRational v
floatToDouble (Float64Value v) = v

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
unOpType Not = Bool
unOpType (Complement t) = IntType t
unOpType (Abs t) = IntType t
unOpType (FAbs t) = FloatType t

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

floatToWord :: Float -> Word32
floatToWord = G.runGet G.getWord32le . P.runPut . P.putFloatle

wordToFloat :: Word32 -> Float
wordToFloat = G.runGet G.getFloatle . P.runPut . P.putWord32le

doubleToWord :: Double -> Word64
doubleToWord = G.runGet G.getWord64le . P.runPut . P.putDoublele

wordToDouble :: Word64 -> Double
wordToDouble = G.runGet G.getDoublele . P.runPut . P.putWord64le

-- | A mapping from names of primitive functions to their parameter
-- types, their result type, and a function for evaluating them.
primFuns ::
  M.Map
    String
    ( [PrimType],
      PrimType,
      [PrimValue] -> Maybe PrimValue
    )
primFuns =
  M.fromList
    [ f32 "sqrt32" sqrt,
      f64 "sqrt64" sqrt,
      f32 "log32" log,
      f64 "log64" log,
      f32 "log10_32" (logBase 10),
      f64 "log10_64" (logBase 10),
      f32 "log2_32" (logBase 2),
      f64 "log2_64" (logBase 2),
      f32 "exp32" exp,
      f64 "exp64" exp,
      f32 "sin32" sin,
      f64 "sin64" sin,
      f32 "sinh32" sinh,
      f64 "sinh64" sinh,
      f32 "cos32" cos,
      f64 "cos64" cos,
      f32 "cosh32" cosh,
      f64 "cosh64" cosh,
      f32 "tan32" tan,
      f64 "tan64" tan,
      f32 "tanh32" tanh,
      f64 "tanh64" tanh,
      f32 "asin32" asin,
      f64 "asin64" asin,
      f32 "asinh32" asinh,
      f64 "asinh64" asinh,
      f32 "acos32" acos,
      f64 "acos64" acos,
      f32 "acosh32" acosh,
      f64 "acosh64" acosh,
      f32 "atan32" atan,
      f64 "atan64" atan,
      f32 "atanh32" atanh,
      f64 "atanh64" atanh,
      f32 "round32" roundFloat,
      f64 "round64" roundDouble,
      f32 "ceil32" ceilFloat,
      f64 "ceil64" ceilDouble,
      f32 "floor32" floorFloat,
      f64 "floor64" floorDouble,
      f32 "gamma32" tgammaf,
      f64 "gamma64" tgamma,
      f32 "lgamma32" lgammaf,
      f64 "lgamma64" lgamma,
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
      ( "mad_hi8",
        ( [IntType Int8, IntType Int8, IntType Int8],
          IntType Int8,
          \case
            [IntValue (Int8Value a), IntValue (Int8Value b), IntValue (Int8Value c)] ->
              Just $ IntValue . Int8Value $ mad_hi8 (Int8Value a) (Int8Value b) c
            _ -> Nothing
        )
      ),
      ( "mad_hi16",
        ( [IntType Int16, IntType Int16, IntType Int16],
          IntType Int16,
          \case
            [IntValue (Int16Value a), IntValue (Int16Value b), IntValue (Int16Value c)] ->
              Just $ IntValue . Int16Value $ mad_hi16 (Int16Value a) (Int16Value b) c
            _ -> Nothing
        )
      ),
      ( "mad_hi32",
        ( [IntType Int32, IntType Int32, IntType Int32],
          IntType Int32,
          \case
            [IntValue (Int32Value a), IntValue (Int32Value b), IntValue (Int32Value c)] ->
              Just $ IntValue . Int32Value $ mad_hi32 (Int32Value a) (Int32Value b) c
            _ -> Nothing
        )
      ),
      ( "mad_hi64",
        ( [IntType Int64, IntType Int64, IntType Int64],
          IntType Int64,
          \case
            [IntValue (Int64Value a), IntValue (Int64Value b), IntValue (Int64Value c)] ->
              Just $ IntValue . Int64Value $ mad_hi64 (Int64Value a) (Int64Value b) c
            _ -> Nothing
        )
      ),
      ( "mul_hi8",
        ( [IntType Int8, IntType Int8],
          IntType Int8,
          \case
            [IntValue (Int8Value a), IntValue (Int8Value b)] ->
              Just $ IntValue . Int8Value $ mul_hi8 (Int8Value a) (Int8Value b)
            _ -> Nothing
        )
      ),
      ( "mul_hi16",
        ( [IntType Int16, IntType Int16],
          IntType Int16,
          \case
            [IntValue (Int16Value a), IntValue (Int16Value b)] ->
              Just $ IntValue . Int16Value $ mul_hi16 (Int16Value a) (Int16Value b)
            _ -> Nothing
        )
      ),
      ( "mul_hi32",
        ( [IntType Int32, IntType Int32],
          IntType Int32,
          \case
            [IntValue (Int32Value a), IntValue (Int32Value b)] ->
              Just $ IntValue . Int32Value $ mul_hi32 (Int32Value a) (Int32Value b)
            _ -> Nothing
        )
      ),
      ( "mul_hi64",
        ( [IntType Int64, IntType Int64],
          IntType Int64,
          \case
            [IntValue (Int64Value a), IntValue (Int64Value b)] ->
              Just $ IntValue . Int64Value $ mul_hi64 (Int64Value a) (Int64Value b)
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
      ( "to_bits32",
        ( [FloatType Float32],
          IntType Int32,
          \case
            [FloatValue (Float32Value x)] ->
              Just $ IntValue $ Int32Value $ fromIntegral $ floatToWord x
            _ -> Nothing
        )
      ),
      ( "to_bits64",
        ( [FloatType Float64],
          IntType Int64,
          \case
            [FloatValue (Float64Value x)] ->
              Just $ IntValue $ Int64Value $ fromIntegral $ doubleToWord x
            _ -> Nothing
        )
      ),
      ( "from_bits32",
        ( [IntType Int32],
          FloatType Float32,
          \case
            [IntValue (Int32Value x)] ->
              Just $ FloatValue $ Float32Value $ wordToFloat $ fromIntegral x
            _ -> Nothing
        )
      ),
      ( "from_bits64",
        ( [IntType Int64],
          FloatType Float64,
          \case
            [IntValue (Int64Value x)] ->
              Just $ FloatValue $ Float64Value $ wordToDouble $ fromIntegral x
            _ -> Nothing
        )
      ),
      f32_3 "lerp32" (\v0 v1 t -> v0 + (v1 - v0) * max 0 (min 1 t)),
      f64_3 "lerp64" (\v0 v1 t -> v0 + (v1 - v0) * max 0 (min 1 t)),
      f32_3 "mad32" (\a b c -> a * b + c),
      f64_3 "mad64" (\a b c -> a * b + c),
      f32_3 "fma32" (\a b c -> a * b + c),
      f64_3 "fma64" (\a b c -> a * b + c)
    ]
  where
    i8 s f = (s, ([IntType Int8], IntType Int32, i8PrimFun f))
    i16 s f = (s, ([IntType Int16], IntType Int32, i16PrimFun f))
    i32 s f = (s, ([IntType Int32], IntType Int32, i32PrimFun f))
    i64 s f = (s, ([IntType Int64], IntType Int32, i64PrimFun f))
    f32 s f = (s, ([FloatType Float32], FloatType Float32, f32PrimFun f))
    f64 s f = (s, ([FloatType Float64], FloatType Float64, f64PrimFun f))
    f32_3 s f =
      ( s,
        ( [FloatType Float32, FloatType Float32, FloatType Float32],
          FloatType Float32,
          f32PrimFun3 f
        )
      )
    f64_3 s f =
      ( s,
        ( [FloatType Float64, FloatType Float64, FloatType Float64],
          FloatType Float64,
          f64PrimFun3 f
        )
      )

    i8PrimFun f [IntValue (Int8Value x)] =
      Just $ f x
    i8PrimFun _ _ = Nothing

    i16PrimFun f [IntValue (Int16Value x)] =
      Just $ f x
    i16PrimFun _ _ = Nothing

    i32PrimFun f [IntValue (Int32Value x)] =
      Just $ f x
    i32PrimFun _ _ = Nothing

    i64PrimFun f [IntValue (Int64Value x)] =
      Just $ f x
    i64PrimFun _ _ = Nothing

    f32PrimFun f [FloatValue (Float32Value x)] =
      Just $ FloatValue $ Float32Value $ f x
    f32PrimFun _ _ = Nothing

    f64PrimFun f [FloatValue (Float64Value x)] =
      Just $ FloatValue $ Float64Value $ f x
    f64PrimFun _ _ = Nothing

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

-- | Is the given value kind of zero?
zeroIsh :: PrimValue -> Bool
zeroIsh (IntValue k) = zeroIshInt k
zeroIsh (FloatValue (Float32Value k)) = k == 0
zeroIsh (FloatValue (Float64Value k)) = k == 0
zeroIsh (BoolValue False) = True
zeroIsh _ = False

-- | Is the given value kind of one?
oneIsh :: PrimValue -> Bool
oneIsh (IntValue k) = oneIshInt k
oneIsh (FloatValue (Float32Value k)) = k == 1
oneIsh (FloatValue (Float64Value k)) = k == 1
oneIsh (BoolValue True) = True
oneIsh _ = False

-- | Is the given value kind of negative?
negativeIsh :: PrimValue -> Bool
negativeIsh (IntValue k) = negativeIshInt k
negativeIsh (FloatValue (Float32Value k)) = k < 0
negativeIsh (FloatValue (Float64Value k)) = k < 0
negativeIsh (BoolValue _) = False
negativeIsh Checked = False

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

-- | The size of a value of a given primitive type in bites.
primBitSize :: PrimType -> Int
primBitSize = (* 8) . primByteSize

-- | The size of a value of a given primitive type in eight-bit bytes.
primByteSize :: Num a => PrimType -> a
primByteSize (IntType t) = intByteSize t
primByteSize (FloatType t) = floatByteSize t
primByteSize Bool = 1
primByteSize Cert = 1

-- | The size of a value of a given integer type in eight-bit bytes.
intByteSize :: Num a => IntType -> a
intByteSize Int8 = 1
intByteSize Int16 = 2
intByteSize Int32 = 4
intByteSize Int64 = 8

-- | The size of a value of a given floating-point type in eight-bit bytes.
floatByteSize :: Num a => FloatType -> a
floatByteSize Float32 = 4
floatByteSize Float64 = 8

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

-- SexpIso instances

instance SexpIso BinOp where
  sexpIso =
    match $
      With (. Sexp.list (Sexp.el (Sexp.sym "add") >>> Sexp.el sexpIso >>> Sexp.el sexpIso)) $
        With (. Sexp.list (Sexp.el (Sexp.sym "fadd") >>> Sexp.el sexpIso)) $
          With (. Sexp.list (Sexp.el (Sexp.sym "sub") >>> Sexp.el sexpIso >>> Sexp.el sexpIso)) $
            With (. Sexp.list (Sexp.el (Sexp.sym "fsub") >>> Sexp.el sexpIso)) $
              With (. Sexp.list (Sexp.el (Sexp.sym "mul") >>> Sexp.el sexpIso >>> Sexp.el sexpIso)) $
                With (. Sexp.list (Sexp.el (Sexp.sym "fmul") >>> Sexp.el sexpIso)) $
                  With (. Sexp.list (Sexp.el (Sexp.sym "udiv") >>> Sexp.el sexpIso >>> Sexp.el sexpIso)) $
                    With (. Sexp.list (Sexp.el (Sexp.sym "udivup") >>> Sexp.el sexpIso >>> Sexp.el sexpIso)) $
                      With (. Sexp.list (Sexp.el (Sexp.sym "sdiv") >>> Sexp.el sexpIso >>> Sexp.el sexpIso)) $
                        With (. Sexp.list (Sexp.el (Sexp.sym "sdivup") >>> Sexp.el sexpIso >>> Sexp.el sexpIso)) $
                          With (. Sexp.list (Sexp.el (Sexp.sym "fdiv") >>> Sexp.el sexpIso)) $
                            With (. Sexp.list (Sexp.el (Sexp.sym "fmod") >>> Sexp.el sexpIso)) $
                              With (. Sexp.list (Sexp.el (Sexp.sym "umod") >>> Sexp.el sexpIso >>> Sexp.el sexpIso)) $
                                With (. Sexp.list (Sexp.el (Sexp.sym "smod") >>> Sexp.el sexpIso >>> Sexp.el sexpIso)) $
                                  With (. Sexp.list (Sexp.el (Sexp.sym "squot") >>> Sexp.el sexpIso >>> Sexp.el sexpIso)) $
                                    With (. Sexp.list (Sexp.el (Sexp.sym "srem") >>> Sexp.el sexpIso >>> Sexp.el sexpIso)) $
                                      With (. Sexp.list (Sexp.el (Sexp.sym "smin") >>> Sexp.el sexpIso)) $
                                        With (. Sexp.list (Sexp.el (Sexp.sym "umin") >>> Sexp.el sexpIso)) $
                                          With (. Sexp.list (Sexp.el (Sexp.sym "fmin") >>> Sexp.el sexpIso)) $
                                            With (. Sexp.list (Sexp.el (Sexp.sym "smax") >>> Sexp.el sexpIso)) $
                                              With (. Sexp.list (Sexp.el (Sexp.sym "umax") >>> Sexp.el sexpIso)) $
                                                With (. Sexp.list (Sexp.el (Sexp.sym "fmap") >>> Sexp.el sexpIso)) $
                                                  With (. Sexp.list (Sexp.el (Sexp.sym "shl") >>> Sexp.el sexpIso)) $
                                                    With (. Sexp.list (Sexp.el (Sexp.sym "lshr") >>> Sexp.el sexpIso)) $
                                                      With (. Sexp.list (Sexp.el (Sexp.sym "ashr") >>> Sexp.el sexpIso)) $
                                                        With (. Sexp.list (Sexp.el (Sexp.sym "and") >>> Sexp.el sexpIso)) $
                                                          With (. Sexp.list (Sexp.el (Sexp.sym "or") >>> Sexp.el sexpIso)) $
                                                            With (. Sexp.list (Sexp.el (Sexp.sym "xor") >>> Sexp.el sexpIso)) $
                                                              With (. Sexp.list (Sexp.el (Sexp.sym "pow") >>> Sexp.el sexpIso)) $
                                                                With (. Sexp.list (Sexp.el (Sexp.sym "fpow") >>> Sexp.el sexpIso)) $
                                                                  With (. Sexp.sym "logand") $
                                                                    With
                                                                      (. Sexp.sym "logor")
                                                                      End

instance SexpIso CmpOp where
  sexpIso =
    match $
      With (. Sexp.list (Sexp.el (Sexp.sym "eq") >>> Sexp.el sexpIso)) $
        With (. Sexp.list (Sexp.el (Sexp.sym "ult") >>> Sexp.el sexpIso)) $
          With (. Sexp.list (Sexp.el (Sexp.sym "ule") >>> Sexp.el sexpIso)) $
            With (. Sexp.list (Sexp.el (Sexp.sym "slt") >>> Sexp.el sexpIso)) $
              With (. Sexp.list (Sexp.el (Sexp.sym "sle") >>> Sexp.el sexpIso)) $
                With (. Sexp.list (Sexp.el (Sexp.sym "lt") >>> Sexp.el sexpIso)) $
                  With (. Sexp.list (Sexp.el (Sexp.sym "le") >>> Sexp.el sexpIso)) $
                    With (. Sexp.sym "llt") $
                      With
                        (. Sexp.sym "lle")
                        End

instance SexpIso ConvOp where
  sexpIso =
    match $
      With (. Sexp.list (Sexp.el (Sexp.sym "zext") >>> Sexp.el sexpIso >>> Sexp.el sexpIso)) $
        With (. Sexp.list (Sexp.el (Sexp.sym "sext") >>> Sexp.el sexpIso >>> Sexp.el sexpIso)) $
          With (. Sexp.list (Sexp.el (Sexp.sym "fpconv") >>> Sexp.el sexpIso >>> Sexp.el sexpIso)) $
            With (. Sexp.list (Sexp.el (Sexp.sym "fptoui") >>> Sexp.el sexpIso >>> Sexp.el sexpIso)) $
              With (. Sexp.list (Sexp.el (Sexp.sym "fptosi") >>> Sexp.el sexpIso >>> Sexp.el sexpIso)) $
                With (. Sexp.list (Sexp.el (Sexp.sym "uitofp") >>> Sexp.el sexpIso >>> Sexp.el sexpIso)) $
                  With (. Sexp.list (Sexp.el (Sexp.sym "sitofp") >>> Sexp.el sexpIso >>> Sexp.el sexpIso)) $
                    With (. Sexp.list (Sexp.el (Sexp.sym "itob") >>> Sexp.el sexpIso)) $
                      With
                        (. Sexp.list (Sexp.el (Sexp.sym "btoi") >>> Sexp.el sexpIso))
                        End

-- Prettyprinting instances

instance Pretty BinOp where
  ppr (Add t OverflowWrap) = taggedI "add" t
  ppr (Add t OverflowUndef) = taggedI "add_nw" t
  ppr (Sub t OverflowWrap) = taggedI "sub" t
  ppr (Sub t OverflowUndef) = taggedI "sub_nw" t
  ppr (Mul t OverflowWrap) = taggedI "mul" t
  ppr (Mul t OverflowUndef) = taggedI "mul_nw" t
  ppr (FAdd t) = taggedF "fadd" t
  ppr (FSub t) = taggedF "fsub" t
  ppr (FMul t) = taggedF "fmul" t
  ppr (UDiv t Safe) = taggedI "udiv_safe" t
  ppr (UDiv t Unsafe) = taggedI "udiv" t
  ppr (UDivUp t Safe) = taggedI "udiv_up_safe" t
  ppr (UDivUp t Unsafe) = taggedI "udiv_up" t
  ppr (UMod t Safe) = taggedI "umod_safe" t
  ppr (UMod t Unsafe) = taggedI "umod" t
  ppr (SDiv t Safe) = taggedI "sdiv_safe" t
  ppr (SDiv t Unsafe) = taggedI "sdiv" t
  ppr (SDivUp t Safe) = taggedI "sdiv_up_safe" t
  ppr (SDivUp t Unsafe) = taggedI "sdiv_up" t
  ppr (SMod t Safe) = taggedI "smod_safe" t
  ppr (SMod t Unsafe) = taggedI "smod" t
  ppr (SQuot t Safe) = taggedI "squot_safe" t
  ppr (SQuot t Unsafe) = taggedI "squot" t
  ppr (SRem t Safe) = taggedI "srem_safe" t
  ppr (SRem t Unsafe) = taggedI "srem" t
  ppr (FDiv t) = taggedF "fdiv" t
  ppr (FMod t) = taggedF "fmod" t
  ppr (SMin t) = taggedI "smin" t
  ppr (UMin t) = taggedI "umin" t
  ppr (FMin t) = taggedF "fmin" t
  ppr (SMax t) = taggedI "smax" t
  ppr (UMax t) = taggedI "umax" t
  ppr (FMax t) = taggedF "fmax" t
  ppr (Shl t) = taggedI "shl" t
  ppr (LShr t) = taggedI "lshr" t
  ppr (AShr t) = taggedI "ashr" t
  ppr (And t) = taggedI "and" t
  ppr (Or t) = taggedI "or" t
  ppr (Xor t) = taggedI "xor" t
  ppr (Pow t) = taggedI "pow" t
  ppr (FPow t) = taggedF "fpow" t
  ppr LogAnd = text "logand"
  ppr LogOr = text "logor"

instance Pretty CmpOp where
  ppr (CmpEq t) = text "eq_" <> ppr t
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
    where
      (from, to) = convOpType op

instance Pretty UnOp where
  ppr Not = text "not"
  ppr (Abs t) = taggedI "abs" t
  ppr (FAbs t) = taggedF "fabs" t
  ppr (SSignum t) = taggedI "ssignum" t
  ppr (USignum t) = taggedI "usignum" t
  ppr (Complement t) = taggedI "complement" t

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
convOpFun IToB {} = "itob"
convOpFun BToI {} = "btoi"

taggedI :: String -> IntType -> Doc
taggedI s Int8 = text $ s ++ "8"
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

mul_hi8 :: IntValue -> IntValue -> Int8
mul_hi8 a b =
  let a' = intToWord64 a
      b' = intToWord64 b
   in fromIntegral (shiftR (a' * b') 8)

mul_hi16 :: IntValue -> IntValue -> Int16
mul_hi16 a b =
  let a' = intToWord64 a
      b' = intToWord64 b
   in fromIntegral (shiftR (a' * b') 16)

mul_hi32 :: IntValue -> IntValue -> Int32
mul_hi32 a b =
  let a' = intToWord64 a
      b' = intToWord64 b
   in fromIntegral (shiftR (a' * b') 32)

mul_hi64 :: IntValue -> IntValue -> Int64
mul_hi64 a b =
  let a' = (toInteger . intToWord64) a
      b' = (toInteger . intToWord64) b
   in fromIntegral (shiftR (a' * b') 64)

mad_hi8 :: IntValue -> IntValue -> Int8 -> Int8
mad_hi8 a b c = mul_hi8 a b + c

mad_hi16 :: IntValue -> IntValue -> Int16 -> Int16
mad_hi16 a b c = mul_hi16 a b + c

mad_hi32 :: IntValue -> IntValue -> Int32 -> Int32
mad_hi32 a b c = mul_hi32 a b + c

mad_hi64 :: IntValue -> IntValue -> Int64 -> Int64
mad_hi64 a b c = mul_hi64 a b + c
