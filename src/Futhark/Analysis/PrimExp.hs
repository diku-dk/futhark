{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

-- | A primitive expression is an expression where the non-leaves are
-- primitive operators.  Our representation does not guarantee that
-- the expression is type-correct.
module Futhark.Analysis.PrimExp
  ( PrimExp (..),
    TPrimExp (..),
    isInt8,
    isInt16,
    isInt32,
    isInt64,
    isBool,
    isF16,
    isF32,
    isF64,
    evalPrimExp,
    primExpType,
    primExpSizeAtLeast,
    coerceIntPrimExp,
    leafExpTypes,
    true,
    false,
    fromBool,
    constFoldPrimExp,

    -- * Construction
    module Language.Futhark.Primitive,
    NumExp (..),
    IntExp (..),
    FloatExp (..),
    sExt,
    zExt,
    (.&&.),
    (.||.),
    (.<.),
    (.<=.),
    (.>.),
    (.>=.),
    (.==.),
    (.&.),
    (.|.),
    (.^.),
    (.>>.),
    (.<<.),
    bNot,
    sMax32,
    sMin32,
    sMax64,
    sMin64,
    sExt32,
    sExt64,
    zExt32,
    zExt64,
    sExtAs,
    fMin16,
    fMin32,
    fMin64,
    fMax16,
    fMax32,
    fMax64,

    -- * Untyped construction
    (~*~),
    (~/~),
    (~+~),
    (~-~),
    (~==~),
  )
where

import Control.Category
import Control.Monad
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Traversable
import Futhark.IR.Prop.Names
import Futhark.Util.IntegralExp
import Futhark.Util.Pretty
import Language.Futhark.Primitive
import Prelude hiding (id, (.))

-- | A primitive expression parametrised over the representation of
-- free variables.  Note that the 'Functor', 'Traversable', and 'Num'
-- instances perform automatic (but simple) constant folding.
--
-- Note also that the 'Num' instance assumes 'OverflowUndef'
-- semantics!
data PrimExp v
  = LeafExp v PrimType
  | ValueExp PrimValue
  | BinOpExp BinOp (PrimExp v) (PrimExp v)
  | CmpOpExp CmpOp (PrimExp v) (PrimExp v)
  | UnOpExp UnOp (PrimExp v)
  | ConvOpExp ConvOp (PrimExp v)
  | FunExp T.Text [PrimExp v] PrimType
  deriving (Eq, Ord, Show)

instance Functor PrimExp where
  fmap = fmapDefault

instance Foldable PrimExp where
  foldMap = foldMapDefault

instance Traversable PrimExp where
  traverse f (LeafExp v t) =
    LeafExp <$> f v <*> pure t
  traverse _ (ValueExp v) =
    pure $ ValueExp v
  traverse f (BinOpExp op x y) =
    BinOpExp op <$> traverse f x <*> traverse f y
  traverse f (CmpOpExp op x y) =
    CmpOpExp op <$> traverse f x <*> traverse f y
  traverse f (ConvOpExp op x) =
    ConvOpExp op <$> traverse f x
  traverse f (UnOpExp op x) =
    UnOpExp op <$> traverse f x
  traverse f (FunExp h args t) =
    FunExp h <$> traverse (traverse f) args <*> pure t

instance (FreeIn v) => FreeIn (PrimExp v) where
  freeIn' = foldMap freeIn'

-- | A 'PrimExp' tagged with a phantom type used to provide type-safe
-- construction.  Does not guarantee that the underlying expression is
-- actually type correct.
newtype TPrimExp t v = TPrimExp {untyped :: PrimExp v}
  deriving (Eq, Ord, Show)

instance Functor (TPrimExp t) where
  fmap = fmapDefault

instance Foldable (TPrimExp t) where
  foldMap = foldMapDefault

instance Traversable (TPrimExp t) where
  traverse f (TPrimExp e) = TPrimExp <$> traverse f e

instance (FreeIn v) => FreeIn (TPrimExp t v) where
  freeIn' = freeIn' . untyped

-- | This expression is of type t'Int8'.
isInt8 :: PrimExp v -> TPrimExp Int8 v
isInt8 = TPrimExp

-- | This expression is of type t'Int16'.
isInt16 :: PrimExp v -> TPrimExp Int16 v
isInt16 = TPrimExp

-- | This expression is of type t'Int32'.
isInt32 :: PrimExp v -> TPrimExp Int32 v
isInt32 = TPrimExp

-- | This expression is of type t'Int64'.
isInt64 :: PrimExp v -> TPrimExp Int64 v
isInt64 = TPrimExp

-- | This is a boolean expression.
isBool :: PrimExp v -> TPrimExp Bool v
isBool = TPrimExp

-- | This expression is of type t'Half'.
isF16 :: PrimExp v -> TPrimExp Half v
isF16 = TPrimExp

-- | This expression is of type t'Float'.
isF32 :: PrimExp v -> TPrimExp Float v
isF32 = TPrimExp

-- | This expression is of type t'Double'.
isF64 :: PrimExp v -> TPrimExp Double v
isF64 = TPrimExp

-- | True if the 'PrimExp' has at least this many nodes.  This can be
-- much more efficient than comparing with 'length' for large
-- 'PrimExp's, as this function is lazy.
primExpSizeAtLeast :: Int -> PrimExp v -> Bool
primExpSizeAtLeast k = maybe True (>= k) . descend 0
  where
    descend i _
      | i >= k = Nothing
    descend i LeafExp {} = Just (i + 1)
    descend i ValueExp {} = Just (i + 1)
    descend i (BinOpExp _ x y) = do
      x' <- descend (i + 1) x
      descend x' y
    descend i (CmpOpExp _ x y) = do
      x' <- descend (i + 1) x
      descend x' y
    descend i (ConvOpExp _ x) = descend (i + 1) x
    descend i (UnOpExp _ x) = descend (i + 1) x
    descend i (FunExp _ args _) = foldM descend (i + 1) args

-- | Perform quick and dirty constant folding on the top level of a
-- PrimExp.  This is necessary because we want to consider
-- e.g. equality modulo constant folding.
constFoldPrimExp :: PrimExp v -> PrimExp v
constFoldPrimExp (BinOpExp Add {} x y)
  | zeroIshExp x = y
  | zeroIshExp y = x
constFoldPrimExp (BinOpExp Sub {} x y)
  | zeroIshExp y = x
constFoldPrimExp (BinOpExp Mul {} x y)
  | oneIshExp x = y
  | oneIshExp y = x
  | zeroIshExp x,
    IntType it <- primExpType y =
      ValueExp $ IntValue $ intValue it (0 :: Int)
  | zeroIshExp y,
    IntType it <- primExpType x =
      ValueExp $ IntValue $ intValue it (0 :: Int)
constFoldPrimExp (BinOpExp SDiv {} x y)
  | oneIshExp y = x
constFoldPrimExp (BinOpExp SQuot {} x y)
  | oneIshExp y = x
constFoldPrimExp (BinOpExp UDiv {} x y)
  | oneIshExp y = x
constFoldPrimExp (BinOpExp bop (ValueExp x) (ValueExp y))
  | Just z <- doBinOp bop x y =
      ValueExp z
constFoldPrimExp (BinOpExp LogAnd x y)
  | oneIshExp x = y
  | oneIshExp y = x
  | zeroIshExp x = x
  | zeroIshExp y = y
constFoldPrimExp (BinOpExp LogOr x y)
  | oneIshExp x = x
  | oneIshExp y = y
  | zeroIshExp x = y
  | zeroIshExp y = x
constFoldPrimExp (UnOpExp Abs {} x)
  | not $ negativeIshExp x = x
constFoldPrimExp (UnOpExp (Neg _) (ValueExp (BoolValue x))) =
  ValueExp $ BoolValue $ not x
constFoldPrimExp (BinOpExp UMod {} x y)
  | sameIshExp x y,
    IntType it <- primExpType x =
      ValueExp $ IntValue $ intValue it (0 :: Integer)
constFoldPrimExp (BinOpExp SMod {} x y)
  | sameIshExp x y,
    IntType it <- primExpType x =
      ValueExp $ IntValue $ intValue it (0 :: Integer)
constFoldPrimExp (BinOpExp SRem {} x y)
  | sameIshExp x y,
    IntType it <- primExpType x =
      ValueExp $ IntValue $ intValue it (0 :: Integer)
constFoldPrimExp e = e

constFoldCmpExp :: (Eq v) => PrimExp v -> PrimExp v
constFoldCmpExp (CmpOpExp (CmpEq _) x y)
  | x == y =
      untyped true
constFoldCmpExp (CmpOpExp (CmpEq _) (ValueExp x) (ValueExp y))
  | x /= y =
      untyped false
constFoldCmpExp e = constFoldPrimExp e

-- | The class of numeric types that can be used for constructing
-- 'TPrimExp's.
class NumExp t where
  -- | Construct a typed expression from an integer.
  fromInteger' :: Integer -> TPrimExp t v

  -- | Construct a numeric expression from a boolean expression.  This
  -- can be used to encode arithmetic control flow.
  fromBoolExp :: TPrimExp Bool v -> TPrimExp t v

-- | The class of integer types that can be used for constructing
-- 'TPrimExp's.
class (NumExp t) => IntExp t where
  -- | The type of an expression, known to be an integer type.
  expIntType :: TPrimExp t v -> IntType

instance NumExp Int8 where
  fromInteger' = isInt8 . ValueExp . IntValue . Int8Value . fromInteger
  fromBoolExp = isInt8 . ConvOpExp (BToI Int8) . untyped

instance IntExp Int8 where
  expIntType = const Int8

instance NumExp Int16 where
  fromInteger' = isInt16 . ValueExp . IntValue . Int16Value . fromInteger
  fromBoolExp = isInt16 . ConvOpExp (BToI Int16) . untyped

instance IntExp Int16 where
  expIntType = const Int16

instance NumExp Int32 where
  fromInteger' = isInt32 . ValueExp . IntValue . Int32Value . fromInteger
  fromBoolExp = isInt32 . ConvOpExp (BToI Int32) . untyped

instance IntExp Int32 where
  expIntType = const Int32

instance NumExp Int64 where
  fromInteger' = isInt64 . ValueExp . IntValue . Int64Value . fromInteger
  fromBoolExp = isInt64 . ConvOpExp (BToI Int64) . untyped

instance IntExp Int64 where
  expIntType = const Int64

-- | The class of floating-point types that can be used for
-- constructing 'TPrimExp's.
class (NumExp t) => FloatExp t where
  -- | Construct a typed expression from a rational.
  fromRational' :: Rational -> TPrimExp t v

  -- | The type of an expression, known to be a floating-point type.
  expFloatType :: TPrimExp t v -> FloatType

instance NumExp Half where
  fromInteger' = isF16 . ValueExp . FloatValue . Float16Value . fromInteger
  fromBoolExp = isF16 . ConvOpExp (SIToFP Int16 Float16) . ConvOpExp (BToI Int16) . untyped

instance NumExp Float where
  fromInteger' = isF32 . ValueExp . FloatValue . Float32Value . fromInteger
  fromBoolExp = isF32 . ConvOpExp (SIToFP Int32 Float32) . ConvOpExp (BToI Int32) . untyped

instance NumExp Double where
  fromInteger' = TPrimExp . ValueExp . FloatValue . Float64Value . fromInteger
  fromBoolExp = isF64 . ConvOpExp (SIToFP Int32 Float64) . ConvOpExp (BToI Int32) . untyped

instance FloatExp Half where
  fromRational' = TPrimExp . ValueExp . FloatValue . Float16Value . fromRational
  expFloatType = const Float16

instance FloatExp Float where
  fromRational' = TPrimExp . ValueExp . FloatValue . Float32Value . fromRational
  expFloatType = const Float32

instance FloatExp Double where
  fromRational' = TPrimExp . ValueExp . FloatValue . Float64Value . fromRational
  expFloatType = const Float64

instance (NumExp t, Pretty v) => Num (TPrimExp t v) where
  TPrimExp x + TPrimExp y
    | Just z <-
        msum
          [ asIntOp (`Add` OverflowUndef) x y,
            asFloatOp FAdd x y
          ] =
        TPrimExp $ constFoldPrimExp z
    | otherwise = numBad "+" (x, y)

  TPrimExp x - TPrimExp y
    | Just z <-
        msum
          [ asIntOp (`Sub` OverflowUndef) x y,
            asFloatOp FSub x y
          ] =
        TPrimExp $ constFoldPrimExp z
    | otherwise = numBad "-" (x, y)

  TPrimExp x * TPrimExp y
    | Just z <-
        msum
          [ asIntOp (`Mul` OverflowUndef) x y,
            asFloatOp FMul x y
          ] =
        TPrimExp $ constFoldPrimExp z
    | otherwise = numBad "*" (x, y)

  abs (TPrimExp x)
    | IntType t <- primExpType x = TPrimExp $ constFoldPrimExp $ UnOpExp (Abs t) x
    | FloatType t <- primExpType x = TPrimExp $ constFoldPrimExp $ UnOpExp (FAbs t) x
    | otherwise = numBad "abs" x

  signum (TPrimExp x)
    | IntType t <- primExpType x = TPrimExp $ UnOpExp (SSignum t) x
    | otherwise = numBad "signum" x

  fromInteger = fromInteger'

instance (FloatExp t, Pretty v) => Fractional (TPrimExp t v) where
  TPrimExp x / TPrimExp y
    | Just z <- msum [asFloatOp FDiv x y] = TPrimExp $ constFoldPrimExp z
    | otherwise = numBad "/" (x, y)

  fromRational = fromRational'

instance (Pretty v) => Floating (TPrimExp Half v) where
  x ** y = isF16 $ BinOpExp (FPow Float16) (untyped x) (untyped y)
  pi = isF16 $ ValueExp $ FloatValue $ Float16Value pi
  exp x = isF16 $ FunExp "exp16" [untyped x] $ FloatType Float16
  log x = isF16 $ FunExp "log16" [untyped x] $ FloatType Float16
  sin x = isF16 $ FunExp "sin16" [untyped x] $ FloatType Float16
  cos x = isF16 $ FunExp "cos16" [untyped x] $ FloatType Float16
  tan x = isF16 $ FunExp "tan16" [untyped x] $ FloatType Float16
  asin x = isF16 $ FunExp "asin16" [untyped x] $ FloatType Float16
  acos x = isF16 $ FunExp "acos16" [untyped x] $ FloatType Float16
  atan x = isF16 $ FunExp "atan16" [untyped x] $ FloatType Float16
  sinh x = isF16 $ FunExp "sinh16" [untyped x] $ FloatType Float16
  cosh x = isF16 $ FunExp "cosh16" [untyped x] $ FloatType Float16
  tanh x = isF16 $ FunExp "tanh16" [untyped x] $ FloatType Float16
  asinh x = isF16 $ FunExp "asinh16" [untyped x] $ FloatType Float16
  acosh x = isF16 $ FunExp "acosh16" [untyped x] $ FloatType Float16
  atanh x = isF16 $ FunExp "atanh16" [untyped x] $ FloatType Float16

instance (Pretty v) => Floating (TPrimExp Float v) where
  x ** y = isF32 $ BinOpExp (FPow Float32) (untyped x) (untyped y)
  pi = isF32 $ ValueExp $ FloatValue $ Float32Value pi
  exp x = isF32 $ FunExp "exp32" [untyped x] $ FloatType Float32
  log x = isF32 $ FunExp "log32" [untyped x] $ FloatType Float32
  sin x = isF32 $ FunExp "sin32" [untyped x] $ FloatType Float32
  cos x = isF32 $ FunExp "cos32" [untyped x] $ FloatType Float32
  tan x = isF32 $ FunExp "tan32" [untyped x] $ FloatType Float32
  asin x = isF32 $ FunExp "asin32" [untyped x] $ FloatType Float32
  acos x = isF32 $ FunExp "acos32" [untyped x] $ FloatType Float32
  atan x = isF32 $ FunExp "atan32" [untyped x] $ FloatType Float32
  sinh x = isF32 $ FunExp "sinh32" [untyped x] $ FloatType Float32
  cosh x = isF32 $ FunExp "cosh32" [untyped x] $ FloatType Float32
  tanh x = isF32 $ FunExp "tanh32" [untyped x] $ FloatType Float32
  asinh x = isF32 $ FunExp "asinh32" [untyped x] $ FloatType Float32
  acosh x = isF32 $ FunExp "acosh32" [untyped x] $ FloatType Float32
  atanh x = isF32 $ FunExp "atanh32" [untyped x] $ FloatType Float32

instance (Pretty v) => Floating (TPrimExp Double v) where
  x ** y = isF64 $ BinOpExp (FPow Float64) (untyped x) (untyped y)
  pi = isF64 $ ValueExp $ FloatValue $ Float64Value pi
  exp x = isF64 $ FunExp "exp64" [untyped x] $ FloatType Float64
  log x = isF64 $ FunExp "log64" [untyped x] $ FloatType Float64
  sin x = isF64 $ FunExp "sin64" [untyped x] $ FloatType Float64
  cos x = isF64 $ FunExp "cos64" [untyped x] $ FloatType Float64
  tan x = isF64 $ FunExp "tan64" [untyped x] $ FloatType Float64
  asin x = isF64 $ FunExp "asin64" [untyped x] $ FloatType Float64
  acos x = isF64 $ FunExp "acos64" [untyped x] $ FloatType Float64
  atan x = isF64 $ FunExp "atan64" [untyped x] $ FloatType Float64
  sinh x = isF64 $ FunExp "sinh64" [untyped x] $ FloatType Float64
  cosh x = isF64 $ FunExp "cosh64" [untyped x] $ FloatType Float64
  tanh x = isF64 $ FunExp "tanh64" [untyped x] $ FloatType Float64
  asinh x = isF64 $ FunExp "asinh64" [untyped x] $ FloatType Float64
  acosh x = isF64 $ FunExp "acosh64" [untyped x] $ FloatType Float64
  atanh x = isF64 $ FunExp "atanh64" [untyped x] $ FloatType Float64

instance (IntExp t, Pretty v) => IntegralExp (TPrimExp t v) where
  TPrimExp x `div` TPrimExp y
    | Just z <-
        msum
          [ asIntOp (`SDiv` Unsafe) x y,
            asFloatOp FDiv x y
          ] =
        TPrimExp $ constFoldPrimExp z
    | otherwise = numBad "div" (x, y)

  TPrimExp x `mod` TPrimExp y
    | Just z <- msum [asIntOp (`SMod` Unsafe) x y] =
        TPrimExp $ constFoldPrimExp z
    | otherwise = numBad "mod" (x, y)

  TPrimExp x `quot` TPrimExp y
    | oneIshExp y = TPrimExp x
    | Just z <- msum [asIntOp (`SQuot` Unsafe) x y] =
        TPrimExp $ constFoldPrimExp z
    | otherwise = numBad "quot" (x, y)

  TPrimExp x `rem` TPrimExp y
    | Just z <- msum [asIntOp (`SRem` Unsafe) x y] =
        TPrimExp $ constFoldPrimExp z
    | otherwise = numBad "rem" (x, y)

  TPrimExp x `divUp` TPrimExp y
    | Just z <- msum [asIntOp (`SDivUp` Unsafe) x y] =
        TPrimExp $ constFoldPrimExp z
    | otherwise = numBad "divRoundingUp" (x, y)

  TPrimExp x `pow` TPrimExp y
    | Just z <-
        msum
          [ asIntOp Pow x y,
            asFloatOp FPow x y
          ] =
        TPrimExp $ constFoldPrimExp z
    | otherwise = numBad "pow" (x, y)

  sgn (TPrimExp (ValueExp (IntValue i))) = Just $ signum $ valueIntegral i
  sgn _ = Nothing

-- | Lifted logical conjunction.
(.&&.) :: (Eq v) => TPrimExp Bool v -> TPrimExp Bool v -> TPrimExp Bool v
TPrimExp x .&&. TPrimExp y = TPrimExp $ constFoldPrimExp $ BinOpExp LogAnd x y

-- | Lifted logical conjunction.
(.||.) :: (Eq v) => TPrimExp Bool v -> TPrimExp Bool v -> TPrimExp Bool v
TPrimExp x .||. TPrimExp y = TPrimExp $ constFoldPrimExp $ BinOpExp LogOr x y

-- | Lifted relational operators; assuming signed numbers in case of
-- integers.
(.<.), (.>.), (.<=.), (.>=.), (.==.) :: (Eq v) => TPrimExp t v -> TPrimExp t v -> TPrimExp Bool v
TPrimExp x .<. TPrimExp y =
  TPrimExp $ constFoldCmpExp $ CmpOpExp cmp x y
  where
    cmp = case primExpType x of
      IntType t -> CmpSlt t
      FloatType t -> FCmpLt t
      _ -> CmpLlt
TPrimExp x .<=. TPrimExp y =
  TPrimExp $ constFoldCmpExp $ CmpOpExp cmp x y
  where
    cmp = case primExpType x of
      IntType t -> CmpSle t
      FloatType t -> FCmpLe t
      _ -> CmpLle
TPrimExp x .==. TPrimExp y =
  TPrimExp $ constFoldCmpExp $ CmpOpExp (CmpEq t) x y
  where
    t = primExpType x `min` primExpType y
x .>. y = y .<. x
x .>=. y = y .<=. x

-- | Lifted bitwise operators.  The right-shift is logical, *not* arithmetic.
(.&.), (.|.), (.^.), (.>>.), (.<<.) :: (Eq v) => TPrimExp t v -> TPrimExp t v -> TPrimExp t v
bitPrimExp :: (Eq v) => (IntType -> BinOp) -> TPrimExp t v -> TPrimExp t v -> TPrimExp t v
bitPrimExp op (TPrimExp x) (TPrimExp y) =
  TPrimExp $
    constFoldPrimExp $
      BinOpExp (op $ primExpIntType x) x y
(.&.) = bitPrimExp And
(.|.) = bitPrimExp Or
(.^.) = bitPrimExp Xor
(.>>.) = bitPrimExp LShr
(.<<.) = bitPrimExp Shl

infix 4 .==., .<., .>., .<=., .>=.

infixr 3 .&&.

infixr 2 .||.

-- | Untyped smart constructor for sign extension that does a bit of
-- constant folding.
sExt :: IntType -> PrimExp v -> PrimExp v
sExt it (ValueExp (IntValue v)) = ValueExp $ IntValue $ doSExt v it
sExt it e
  | primExpIntType e == it = e
  | otherwise = ConvOpExp (SExt (primExpIntType e) it) e

-- | Untyped smart constructor for zero extension that does a bit of
-- constant folding.
zExt :: IntType -> PrimExp v -> PrimExp v
zExt it (ValueExp (IntValue v)) = ValueExp $ IntValue $ doZExt v it
zExt it e
  | primExpIntType e == it = e
  | otherwise = ConvOpExp (ZExt (primExpIntType e) it) e

asIntOp :: (IntType -> BinOp) -> PrimExp v -> PrimExp v -> Maybe (PrimExp v)
asIntOp f x y
  | IntType x_t <- primExpType x = Just $ BinOpExp (f x_t) x y
  | otherwise = Nothing

asFloatOp :: (FloatType -> BinOp) -> PrimExp v -> PrimExp v -> Maybe (PrimExp v)
asFloatOp f x y
  | FloatType t <- primExpType x = Just $ BinOpExp (f t) x y
  | otherwise = Nothing

numBad :: (Pretty a) => String -> a -> b
numBad s x =
  error $ "Invalid argument to PrimExp method " ++ s ++ ": " ++ prettyString x

-- | Evaluate a 'PrimExp' in the given monad.  Invokes 'fail' on type
-- errors.
evalPrimExp :: (Pretty v, MonadFail m) => (v -> m PrimValue) -> PrimExp v -> m PrimValue
evalPrimExp f (LeafExp v _) = f v
evalPrimExp _ (ValueExp v) = pure v
evalPrimExp f (BinOpExp op x y) = do
  x' <- evalPrimExp f x
  y' <- evalPrimExp f y
  maybe (evalBad op (x, y)) pure $ doBinOp op x' y'
evalPrimExp f (CmpOpExp op x y) = do
  x' <- evalPrimExp f x
  y' <- evalPrimExp f y
  maybe (evalBad op (x, y)) (pure . BoolValue) $ doCmpOp op x' y'
evalPrimExp f (UnOpExp op x) = do
  x' <- evalPrimExp f x
  maybe (evalBad op x) pure $ doUnOp op x'
evalPrimExp f (ConvOpExp op x) = do
  x' <- evalPrimExp f x
  maybe (evalBad op x) pure $ doConvOp op x'
evalPrimExp f (FunExp h args _) = do
  args' <- mapM (evalPrimExp f) args
  maybe (evalBad h args) pure $ do
    (_, _, fun) <- M.lookup h primFuns
    fun args'

evalBad :: (Pretty a, Pretty b, MonadFail m) => a -> b -> m c
evalBad op arg =
  fail $
    "evalPrimExp: Type error when applying "
      ++ prettyString op
      ++ " to "
      ++ prettyString arg

-- | The type of values returned by a 'PrimExp'.  This function
-- returning does not imply that the 'PrimExp' is type-correct.
primExpType :: PrimExp v -> PrimType
primExpType (LeafExp _ t) = t
primExpType (ValueExp v) = primValueType v
primExpType (BinOpExp op _ _) = binOpType op
primExpType CmpOpExp {} = Bool
primExpType (UnOpExp op _) = unOpType op
primExpType (ConvOpExp op _) = snd $ convOpType op
primExpType (FunExp _ _ t) = t

-- | Is the expression a constant zero of some sort?
zeroIshExp :: PrimExp v -> Bool
zeroIshExp (ValueExp v) = zeroIsh v
zeroIshExp _ = False

-- | Is the expression a constant one of some sort?
oneIshExp :: PrimExp v -> Bool
oneIshExp (ValueExp v) = oneIsh v
oneIshExp _ = False

-- | Is the expression a constant negative of some sort?
negativeIshExp :: PrimExp v -> Bool
negativeIshExp (ValueExp v) = negativeIsh v
negativeIshExp _ = False

sameIshExp :: PrimExp v -> PrimExp v -> Bool
sameIshExp (ValueExp v1) (ValueExp v2) = v1 == v2
sameIshExp _ _ = False

-- | If the given 'PrimExp' is a constant of the wrong integer type,
-- coerce it to the given integer type.  This is a workaround for an
-- issue in the 'Num' instance.
coerceIntPrimExp :: IntType -> PrimExp v -> PrimExp v
coerceIntPrimExp t (ValueExp (IntValue v)) = ValueExp $ IntValue $ doSExt v t
coerceIntPrimExp _ e = e

primExpIntType :: PrimExp v -> IntType
primExpIntType e = case primExpType e of
  IntType t -> t
  _ -> Int64

-- | Boolean-valued PrimExps.
true, false :: TPrimExp Bool v
true = TPrimExp $ ValueExp $ BoolValue True
false = TPrimExp $ ValueExp $ BoolValue False

-- | Conversion from Bool to 'TPrimExp'
fromBool :: Bool -> TPrimExp Bool v
fromBool b = if b then true else false

-- | Boolean negation smart constructor.
bNot :: TPrimExp Bool v -> TPrimExp Bool v
bNot = TPrimExp . UnOpExp (Neg Bool) . untyped

-- | SMax on 32-bit integers.
sMax32 :: TPrimExp Int32 v -> TPrimExp Int32 v -> TPrimExp Int32 v
sMax32 x y = TPrimExp $ BinOpExp (SMax Int32) (untyped x) (untyped y)

-- | SMin on 32-bit integers.
sMin32 :: TPrimExp Int32 v -> TPrimExp Int32 v -> TPrimExp Int32 v
sMin32 x y = TPrimExp $ BinOpExp (SMin Int32) (untyped x) (untyped y)

-- | SMax on 64-bit integers.
sMax64 :: TPrimExp Int64 v -> TPrimExp Int64 v -> TPrimExp Int64 v
sMax64 x y = TPrimExp $ BinOpExp (SMax Int64) (untyped x) (untyped y)

-- | SMin on 64-bit integers.
sMin64 :: TPrimExp Int64 v -> TPrimExp Int64 v -> TPrimExp Int64 v
sMin64 x y = TPrimExp $ BinOpExp (SMin Int64) (untyped x) (untyped y)

-- | Sign-extend to 32 bit integer.
sExt32 :: (IntExp t) => TPrimExp t v -> TPrimExp Int32 v
sExt32 = isInt32 . sExt Int32 . untyped

-- | Sign-extend to 64 bit integer.
sExt64 :: (IntExp t) => TPrimExp t v -> TPrimExp Int64 v
sExt64 = isInt64 . sExt Int64 . untyped

-- | Zero-extend to 32 bit integer.
zExt32 :: (IntExp t) => TPrimExp t v -> TPrimExp Int32 v
zExt32 = isInt32 . zExt Int32 . untyped

-- | Zero-extend to 64 bit integer.
zExt64 :: (IntExp t) => TPrimExp t v -> TPrimExp Int64 v
zExt64 = isInt64 . zExt Int64 . untyped

-- | 16-bit float minimum.
fMin16 :: TPrimExp Half v -> TPrimExp Half v -> TPrimExp Half v
fMin16 x y = isF16 $ BinOpExp (FMin Float16) (untyped x) (untyped y)

-- | 32-bit float minimum.
fMin32 :: TPrimExp Float v -> TPrimExp Float v -> TPrimExp Float v
fMin32 x y = isF32 $ BinOpExp (FMin Float32) (untyped x) (untyped y)

-- | 64-bit float minimum.
fMin64 :: TPrimExp Double v -> TPrimExp Double v -> TPrimExp Double v
fMin64 x y = isF64 $ BinOpExp (FMin Float64) (untyped x) (untyped y)

-- | 16-bit float maximum.
fMax16 :: TPrimExp Half v -> TPrimExp Half v -> TPrimExp Half v
fMax16 x y = isF16 $ BinOpExp (FMax Float16) (untyped x) (untyped y)

-- | 32-bit float maximum.
fMax32 :: TPrimExp Float v -> TPrimExp Float v -> TPrimExp Float v
fMax32 x y = isF32 $ BinOpExp (FMax Float32) (untyped x) (untyped y)

-- | 64-bit float maximum.
fMax64 :: TPrimExp Double v -> TPrimExp Double v -> TPrimExp Double v
fMax64 x y = isF64 $ BinOpExp (FMax Float64) (untyped x) (untyped y)

-- | Convert result of some integer expression to have the same type
-- as another, using sign extension.
sExtAs ::
  (IntExp to, IntExp from) =>
  TPrimExp from v ->
  TPrimExp to v ->
  TPrimExp to v
sExtAs from to = TPrimExp $ sExt (expIntType to) (untyped from)

-- Prettyprinting instances

instance (Pretty v) => Pretty (PrimExp v) where
  pretty (LeafExp v _) = pretty v
  pretty (ValueExp v) = pretty v
  pretty (BinOpExp op x y) = pretty op <+> parens (pretty x) <+> parens (pretty y)
  pretty (CmpOpExp op x y) = pretty op <+> parens (pretty x) <+> parens (pretty y)
  pretty (ConvOpExp op x) = pretty op <+> parens (pretty x)
  pretty (UnOpExp op x) = pretty op <+> parens (pretty x)
  pretty (FunExp h args _) = pretty h <+> parens (commasep $ map pretty args)

instance (Pretty v) => Pretty (TPrimExp t v) where
  pretty = pretty . untyped

-- | Produce a mapping from the leaves of the 'PrimExp' to their
-- designated types.
leafExpTypes :: (Ord a) => PrimExp a -> S.Set (a, PrimType)
leafExpTypes (LeafExp x ptp) = S.singleton (x, ptp)
leafExpTypes (ValueExp _) = S.empty
leafExpTypes (UnOpExp _ e) = leafExpTypes e
leafExpTypes (ConvOpExp _ e) = leafExpTypes e
leafExpTypes (BinOpExp _ e1 e2) =
  S.union (leafExpTypes e1) (leafExpTypes e2)
leafExpTypes (CmpOpExp _ e1 e2) =
  S.union (leafExpTypes e1) (leafExpTypes e2)
leafExpTypes (FunExp _ pes _) =
  S.unions $ map leafExpTypes pes

-- | Multiplication of untyped 'PrimExp's, which must have the same
-- type.  Uses 'OverflowWrap' for integer operations.
(~*~) :: PrimExp v -> PrimExp v -> PrimExp v
x ~*~ y = BinOpExp op x y
  where
    t = primExpType x
    op = case t of
      IntType it -> Mul it OverflowWrap
      FloatType ft -> FMul ft
      Bool -> LogAnd
      Unit -> LogAnd

-- | Division of untyped 'PrimExp's, which must have the same
-- type.  For integers, this is unsafe signed division.
(~/~) :: PrimExp v -> PrimExp v -> PrimExp v
x ~/~ y = BinOpExp op x y
  where
    t = primExpType x
    op = case t of
      IntType it -> SDiv it Unsafe
      FloatType ft -> FDiv ft
      Bool -> LogAnd
      Unit -> LogAnd

-- | Addition of untyped 'PrimExp's, which must have the same type.
-- Uses 'OverflowWrap' for integer operations.
(~+~) :: PrimExp v -> PrimExp v -> PrimExp v
x ~+~ y = BinOpExp op x y
  where
    t = primExpType x
    op = case t of
      IntType it -> Add it OverflowWrap
      FloatType ft -> FAdd ft
      Bool -> LogOr
      Unit -> LogOr

-- | Subtraction of untyped 'PrimExp's, which must have the same type.
-- Uses 'OverflowWrap' for integer operations.
(~-~) :: PrimExp v -> PrimExp v -> PrimExp v
x ~-~ y = BinOpExp op x y
  where
    t = primExpType x
    op = case t of
      IntType it -> Sub it OverflowWrap
      FloatType ft -> FSub ft
      Bool -> LogOr
      Unit -> LogOr

-- | Equality of untyped 'PrimExp's, which must have the same type.
(~==~) :: PrimExp v -> PrimExp v -> PrimExp v
x ~==~ y = CmpOpExp (CmpEq t) x y
  where
    t = primExpType x

infix 7 ~*~, ~/~

infix 6 ~+~, ~-~

infix 4 ~==~
