-- | Partial derivatives of scalar Futhark operations and built-in functions.
module Futhark.AD.Derivatives
  ( pdBuiltin,
    pdBinOp,
    pdUnOp,
  )
where

import Data.Bifunctor (bimap)
import Futhark.Analysis.PrimExp.Convert
import Futhark.IR.Syntax.Core (Name, VName)
import Futhark.Util.IntegralExp
import Prelude hiding (quot)

iConst :: IntType -> Integer -> PrimExp VName
iConst it x = ValueExp $ IntValue $ intValue it x

fConst :: FloatType -> Double -> PrimExp VName
fConst ft x = ValueExp $ FloatValue $ floatValue ft x

untyped2 :: (TPrimExp t v, TPrimExp t v) -> (PrimExp v, PrimExp v)
untyped2 = bimap untyped untyped

-- | @pdUnOp op x@ computes the partial derivatives of @op@
-- with respect to @x@.
pdUnOp :: UnOp -> PrimExp VName -> PrimExp VName
pdUnOp (Abs it) a = UnOpExp (SSignum it) a
pdUnOp (FAbs ft) a = UnOpExp (FSignum ft) a
pdUnOp Not x = x
pdUnOp (Complement it) x = UnOpExp (Complement it) x
pdUnOp (SSignum it) _ = iConst it 0
pdUnOp (USignum it) _ = iConst it 0
pdUnOp (FSignum ft) _ = fConst ft 0

type OnBinOp t v = TPrimExp t v -> TPrimExp t v -> (TPrimExp t v, TPrimExp t v)

intBinOp ::
  OnBinOp Int8 v ->
  OnBinOp Int16 v ->
  OnBinOp Int32 v ->
  OnBinOp Int64 v ->
  IntType ->
  PrimExp v ->
  PrimExp v ->
  (PrimExp v, PrimExp v)
intBinOp f _ _ _ Int8 a b = untyped2 $ f (isInt8 a) (isInt8 b)
intBinOp _ f _ _ Int16 a b = untyped2 $ f (isInt16 a) (isInt16 b)
intBinOp _ _ f _ Int32 a b = untyped2 $ f (isInt32 a) (isInt32 b)
intBinOp _ _ _ f Int64 a b = untyped2 $ f (isInt64 a) (isInt64 b)

floatBinOp ::
  OnBinOp Half v ->
  OnBinOp Float v ->
  OnBinOp Double v ->
  FloatType ->
  PrimExp v ->
  PrimExp v ->
  (PrimExp v, PrimExp v)
floatBinOp f _ _ Float16 a b = untyped2 $ f (isF16 a) (isF16 b)
floatBinOp _ f _ Float32 a b = untyped2 $ f (isF32 a) (isF32 b)
floatBinOp _ _ f Float64 a b = untyped2 $ f (isF64 a) (isF64 b)

-- | @pdBinOp op x y@ computes the partial derivatives of @op@ with
-- respect to @x@ and @y@.
pdBinOp :: BinOp -> PrimExp VName -> PrimExp VName -> (PrimExp VName, PrimExp VName)
pdBinOp (Add it _) _ _ = (iConst it 1, iConst it 1)
pdBinOp (Sub it _) _ _ = (iConst it 1, iConst it (-1))
pdBinOp (Mul _ _) x y = (y, x)
pdBinOp (Pow it) a b =
  intBinOp derivs derivs derivs derivs it a b
  where
    derivs x y = ((x `pow` (y - 1)) * y, 0) -- FIXME (wrt y)
pdBinOp (SDiv it _) a b =
  intBinOp derivs derivs derivs derivs it a b
  where
    derivs x y = (1 `quot` y, negate (x `quot` (y * y)))
pdBinOp (SDivUp it _) a b =
  intBinOp derivs derivs derivs derivs it a b
  where
    derivs x y = (1 `quot` y, negate (x `quot` (y * y)))
pdBinOp (SQuot it _) a b =
  intBinOp derivs derivs derivs derivs it a b
  where
    derivs x y = (1 `quot` y, negate (x `quot` (y * y)))
pdBinOp (UDiv it _) a b =
  intBinOp derivs derivs derivs derivs it a b
  where
    derivs x y = (1 `quot` y, negate (x `quot` (y * y)))
pdBinOp (UDivUp it _) a b =
  intBinOp derivs derivs derivs derivs it a b
  where
    derivs x y = (1 `quot` y, negate (x `quot` (y * y)))
pdBinOp (UMod it _) _ _ = (iConst it 1, iConst it 0) -- FIXME
pdBinOp (SMod it _) _ _ = (iConst it 1, iConst it 0) -- FIXME
pdBinOp (SRem it _) _ _ = (iConst it 1, iConst it 0) -- FIXME
pdBinOp (FMod ft) _ _ = (fConst ft 1, fConst ft 0) -- FIXME
pdBinOp (UMax it) a b =
  intBinOp derivs derivs derivs derivs it a b
  where
    derivs x y = (fromBoolExp (x .>=. y), fromBoolExp (x .<. y))
pdBinOp (SMax it) a b =
  intBinOp derivs derivs derivs derivs it a b
  where
    derivs x y = (fromBoolExp (x .>=. y), fromBoolExp (x .<. y))
pdBinOp (UMin it) a b =
  intBinOp derivs derivs derivs derivs it a b
  where
    derivs x y = (fromBoolExp (x .<=. y), fromBoolExp (x .>. y))
pdBinOp (SMin it) a b =
  intBinOp derivs derivs derivs derivs it a b
  where
    derivs x y = (fromBoolExp (x .<=. y), fromBoolExp (x .>. y))
--
pdBinOp (Shl it) a b =
  pdBinOp (Mul it OverflowWrap) a $ BinOpExp (Pow it) (iConst it 2) b
pdBinOp (LShr it) a b =
  pdBinOp (UDiv it Unsafe) a $ BinOpExp (Pow it) (iConst it 2) b
pdBinOp (AShr it) a b =
  pdBinOp (SDiv it Unsafe) a $ BinOpExp (Pow it) (iConst it 2) b
pdBinOp (And it) _a _b = (iConst it 0, iConst it 0) -- FIXME
pdBinOp (Or it) _a _b = (iConst it 0, iConst it 0) -- FIXME
pdBinOp (Xor it) _a _b = (iConst it 0, iConst it 0) -- FIXME
--
pdBinOp (FAdd ft) _ _ = (fConst ft 1, fConst ft 1)
pdBinOp (FSub ft) _ _ = (fConst ft 1, fConst ft (-1))
pdBinOp (FMul _) x y = (y, x)
pdBinOp (FDiv ft) a b =
  floatBinOp derivs derivs derivs ft a b
  where
    derivs x y = (1 / y, negate (x / (y * y)))
pdBinOp (FPow ft) a b =
  floatBinOp derivs derivs derivs ft a b
  where
    derivs x y = (y * (x ** (y - 1)), (x ** y) * log x)
pdBinOp (FMax ft) a b =
  floatBinOp derivs derivs derivs ft a b
  where
    derivs x y = (fromBoolExp (x .>=. y), fromBoolExp (x .<. y))
pdBinOp (FMin ft) a b =
  floatBinOp derivs derivs derivs ft a b
  where
    derivs x y = (fromBoolExp (x .<=. y), fromBoolExp (x .>. y))
pdBinOp LogAnd a b = (b, a)
pdBinOp LogOr _ _ = (ValueExp $ BoolValue True, ValueExp $ BoolValue False)

-- | @pdBuiltin f args i@ computes the partial derivative of @f@
-- applied to @args@ with respect to each of its arguments.  Returns
-- 'Nothing' if no such derivative is known.
pdBuiltin :: Name -> [PrimExp VName] -> Maybe [PrimExp VName]
pdBuiltin "sqrt16" [x] =
  Just [untyped $ 1 / (2 * sqrt (isF16 x))]
pdBuiltin "sqrt32" [x] =
  Just [untyped $ 1 / (2 * sqrt (isF32 x))]
pdBuiltin "sqrt64" [x] =
  Just [untyped $ 1 / (2 * sqrt (isF64 x))]
pdBuiltin "cbrt16" [x] =
  Just [untyped $ 1 / (3 * cbrt16 (isF16 x) * cbrt16 (isF16 x))]
  where
    cbrt16 a = isF16 $ FunExp "cbrt16" [untyped a] $ FloatType Float16
pdBuiltin "cbrt32" [x] =
  Just [untyped $ 1 / (3 * cbrt32 (isF32 x) * cbrt32 (isF32 x))]
  where
    cbrt32 a = isF32 $ FunExp "cbrt32" [untyped a] $ FloatType Float32
pdBuiltin "cbrt64" [x] =
  Just [untyped $ 1 / (3 * cbrt64 (isF64 x) * cbrt64 (isF64 x))]
  where
    cbrt64 a = isF64 $ FunExp "cbrt64" [untyped a] $ FloatType Float32
pdBuiltin "log16" [x] =
  Just [untyped $ 1 / isF16 x]
pdBuiltin "log32" [x] =
  Just [untyped $ 1 / isF32 x]
pdBuiltin "log64" [x] =
  Just [untyped $ 1 / isF64 x]
pdBuiltin "log10_16" [x] =
  Just [untyped $ 1 / (isF16 x * log 10)]
pdBuiltin "log10_32" [x] =
  Just [untyped $ 1 / (isF32 x * log 10)]
pdBuiltin "log10_64" [x] =
  Just [untyped $ 1 / (isF64 x * log 10)]
pdBuiltin "log2_16" [x] =
  Just [untyped $ 1 / (isF16 x * log 2)]
pdBuiltin "log2_32" [x] =
  Just [untyped $ 1 / (isF32 x * log 2)]
pdBuiltin "log2_64" [x] =
  Just [untyped $ 1 / (isF64 x * log 2)]
pdBuiltin "log1p_16" [x] =
  Just [untyped $ 1 / (isF16 x + 1)]
pdBuiltin "log1p_32" [x] =
  Just [untyped $ 1 / (isF32 x + 1)]
pdBuiltin "log1p_64" [x] =
  Just [untyped $ 1 / (isF64 x + 1)]
pdBuiltin "exp16" [x] =
  Just [untyped $ exp (isF16 x)]
pdBuiltin "exp32" [x] =
  Just [untyped $ exp (isF32 x)]
pdBuiltin "exp64" [x] =
  Just [untyped $ exp (isF64 x)]
pdBuiltin "sin16" [x] =
  Just [untyped $ cos (isF16 x)]
pdBuiltin "sin32" [x] =
  Just [untyped $ cos (isF32 x)]
pdBuiltin "sin64" [x] =
  Just [untyped $ cos (isF64 x)]
pdBuiltin "sinh16" [x] =
  Just [untyped $ cosh (isF16 x)]
pdBuiltin "sinh32" [x] =
  Just [untyped $ cosh (isF32 x)]
pdBuiltin "sinh64" [x] =
  Just [untyped $ cosh (isF64 x)]
pdBuiltin "cos16" [x] =
  Just [untyped $ -sin (isF16 x)]
pdBuiltin "cos32" [x] =
  Just [untyped $ -sin (isF32 x)]
pdBuiltin "cos64" [x] =
  Just [untyped $ -sin (isF64 x)]
pdBuiltin "cosh16" [x] =
  Just [untyped $ sinh (isF16 x)]
pdBuiltin "cosh32" [x] =
  Just [untyped $ sinh (isF32 x)]
pdBuiltin "cosh64" [x] =
  Just [untyped $ sinh (isF64 x)]
pdBuiltin "tan16" [x] =
  Just [untyped $ 1 / (cos (isF16 x) * cos (isF16 x))]
pdBuiltin "tan32" [x] =
  Just [untyped $ 1 / (cos (isF32 x) * cos (isF32 x))]
pdBuiltin "tan64" [x] =
  Just [untyped $ 1 / (cos (isF64 x) * cos (isF64 x))]
pdBuiltin "asin16" [x] =
  Just [untyped $ 1 / sqrt (1 - isF16 x * isF16 x)]
pdBuiltin "asin32" [x] =
  Just [untyped $ 1 / sqrt (1 - isF32 x * isF32 x)]
pdBuiltin "asin64" [x] =
  Just [untyped $ 1 / sqrt (1 - isF64 x * isF64 x)]
pdBuiltin "asinh16" [x] =
  Just [untyped $ 1 / sqrt (1 + isF16 x * isF16 x)]
pdBuiltin "asinh32" [x] =
  Just [untyped $ 1 / sqrt (1 + isF32 x * isF32 x)]
pdBuiltin "asinh64" [x] =
  Just [untyped $ 1 / sqrt (1 + isF64 x * isF64 x)]
pdBuiltin "acos16" [x] =
  Just [untyped $ -1 / sqrt (1 - isF16 x * isF16 x)]
pdBuiltin "acos32" [x] =
  Just [untyped $ -1 / sqrt (1 - isF32 x * isF32 x)]
pdBuiltin "acos64" [x] =
  Just [untyped $ -1 / sqrt (1 - isF64 x * isF64 x)]
pdBuiltin "acosh16" [x] =
  Just [untyped $ 1 / sqrt (isF16 x * isF16 x - 1)]
pdBuiltin "acosh32" [x] =
  Just [untyped $ 1 / sqrt (isF32 x * isF32 x - 1)]
pdBuiltin "acosh64" [x] =
  Just [untyped $ 1 / sqrt (isF64 x * isF64 x - 1)]
pdBuiltin "atan16" [x] =
  Just [untyped $ 1 / (1 + isF16 x * isF16 x)]
pdBuiltin "atan32" [x] =
  Just [untyped $ 1 / (1 + isF32 x * isF32 x)]
pdBuiltin "atan64" [x] =
  Just [untyped $ 1 / (1 + isF64 x * isF64 x)]
pdBuiltin "atanh16" [x] =
  Just [untyped $ cosh (isF16 x) * cosh (isF16 x)]
pdBuiltin "atanh32" [x] =
  Just [untyped $ cosh (isF32 x) * cosh (isF32 x)]
pdBuiltin "atanh64" [x] =
  Just [untyped $ cosh (isF64 x) * cosh (isF64 x)]
pdBuiltin "atan2_16" [x, y] =
  Just
    [ untyped $ -isF16 y / (isF16 x * isF16 x + isF16 y * isF16 y),
      untyped $ -isF16 x / (isF16 x * isF16 x + isF16 y * isF16 y)
    ]
pdBuiltin "atan2_32" [x, y] =
  Just
    [ untyped $ -isF32 y / (isF32 x * isF32 x + isF32 y * isF32 y),
      untyped $ -isF32 x / (isF32 x * isF32 x + isF32 y * isF32 y)
    ]
pdBuiltin "atan2_64" [x, y] =
  Just
    [ untyped $ -isF64 y / (isF64 x * isF64 x + isF64 y * isF64 y),
      untyped $ -isF64 x / (isF64 x * isF64 x + isF64 y * isF64 y)
    ]
pdBuiltin "tanh16" [x] =
  Just [untyped $ 1 - tanh (isF16 x) * tanh (isF16 x)]
pdBuiltin "tanh32" [x] =
  Just [untyped $ 1 - tanh (isF32 x) * tanh (isF32 x)]
pdBuiltin "tanh64" [x] =
  Just [untyped $ 1 - tanh (isF64 x) * tanh (isF64 x)]
pdBuiltin "fma16" [a, b, _c] =
  Just [b, a, fConst Float16 1]
pdBuiltin "fma32" [a, b, _c] =
  Just [b, a, fConst Float32 1]
pdBuiltin "fma64" [a, b, _c] =
  Just [b, a, fConst Float64 1]
pdBuiltin "mad16" [a, b, _c] =
  Just [b, a, fConst Float16 1]
pdBuiltin "mad32" [a, b, _c] =
  Just [b, a, fConst Float32 1]
pdBuiltin "mad64" [a, b, _c] =
  Just [b, a, fConst Float64 1]
pdBuiltin "from_bits16" [_] =
  Just [fConst Float16 1]
pdBuiltin "from_bits32" [_] =
  Just [fConst Float32 1]
pdBuiltin "from_bits64" [_] =
  Just [fConst Float64 1]
pdBuiltin "to_bits16" [_] =
  Just [iConst Int16 1]
pdBuiltin "to_bits32" [_] =
  Just [iConst Int32 1]
pdBuiltin "to_bits64" [_] =
  Just [iConst Int64 1]
pdBuiltin "hypot16" [x, y] =
  Just
    [ untyped $ isF16 x / isF16 (FunExp "hypot16" [x, y] $ FloatType Float16),
      untyped $ isF16 y / isF16 (FunExp "hypot16" [x, y] $ FloatType Float16)
    ]
pdBuiltin "hypot32" [x, y] =
  Just
    [ untyped $ isF32 x / isF32 (FunExp "hypot32" [x, y] $ FloatType Float32),
      untyped $ isF32 y / isF32 (FunExp "hypot32" [x, y] $ FloatType Float32)
    ]
pdBuiltin "hypot64" [x, y] =
  Just
    [ untyped $ isF64 x / isF64 (FunExp "hypot64" [x, y] $ FloatType Float64),
      untyped $ isF64 y / isF64 (FunExp "hypot64" [x, y] $ FloatType Float64)
    ]
pdBuiltin "lerp16" [v0, v1, t] =
  Just
    [ untyped $ 1 - fMax16 0 (fMin16 1 (isF16 t)),
      untyped $ fMax16 0 (fMin16 1 (isF16 t)),
      untyped $ isF16 v1 - isF16 v0
    ]
pdBuiltin "lerp32" [v0, v1, t] =
  Just
    [ untyped $ 1 - fMax32 0 (fMin32 1 (isF32 t)),
      untyped $ fMax32 0 (fMin32 1 (isF32 t)),
      untyped $ isF32 v1 - isF32 v0
    ]
pdBuiltin "lerp64" [v0, v1, t] =
  Just
    [ untyped $ 1 - fMax64 0 (fMin64 1 (isF64 t)),
      untyped $ fMax64 0 (fMin64 1 (isF64 t)),
      untyped $ isF64 v1 - isF64 v0
    ]
pdBuiltin "ldexp16" [x, y] =
  Just
    [ untyped $ 2 ** isF16 x,
      untyped $ log 2 * (2 ** isF16 y) * isF16 x
    ]
pdBuiltin "ldexp32" [x, y] =
  Just
    [ untyped $ 2 ** isF32 x,
      untyped $ log 2 * (2 ** isF32 y) * isF32 x
    ]
pdBuiltin "ldexp64" [x, y] =
  Just
    [ untyped $ 2 ** isF64 x,
      untyped $ log 2 * (2 ** isF64 y) * isF64 x
    ]
pdBuiltin "erf16" [z] =
  Just [untyped $ (2 / sqrt pi) * exp (negate (isF16 z * isF16 z))]
pdBuiltin "erf32" [z] =
  Just [untyped $ (2 / sqrt pi) * exp (negate (isF32 z * isF32 z))]
pdBuiltin "erf64" [z] =
  Just [untyped $ (2 / sqrt pi) * exp (negate (isF64 z * isF64 z))]
pdBuiltin "erfc16" [z] =
  Just [untyped $ negate $ (2 / sqrt pi) * exp (negate (isF16 z * isF16 z))]
pdBuiltin "erfc32" [z] =
  Just [untyped $ negate $ (2 / sqrt pi) * exp (negate (isF32 z * isF32 z))]
pdBuiltin "erfc64" [z] =
  Just [untyped $ negate $ (2 / sqrt pi) * exp (negate (isF64 z * isF64 z))]
pdBuiltin "copysign16" [_x, y] =
  Just [untyped $ 1 * isF16 (UnOpExp (FSignum Float16) y), fConst Float16 0]
pdBuiltin "copysign32" [_x, y] =
  Just [untyped $ 1 * isF32 (UnOpExp (FSignum Float32) y), fConst Float32 0]
pdBuiltin "copysign64" [_x, y] =
  Just [untyped $ 1 * isF64 (UnOpExp (FSignum Float64) y), fConst Float64 0]
-- More problematic derivatives follow below.
pdBuiltin "umul_hi8" [x, y] = Just [y, x]
pdBuiltin "umul_hi16" [x, y] = Just [y, x]
pdBuiltin "umul_hi32" [x, y] = Just [y, x]
pdBuiltin "umul_hi64" [x, y] = Just [y, x]
pdBuiltin "umad_hi8" [a, b, _c] = Just [b, a, iConst Int8 1]
pdBuiltin "umad_hi16" [a, b, _c] = Just [b, a, iConst Int16 1]
pdBuiltin "umad_hi32" [a, b, _c] = Just [b, a, iConst Int32 1]
pdBuiltin "umad_hi64" [a, b, _c] = Just [b, a, iConst Int64 1]
pdBuiltin "smul_hi8" [x, y] = Just [y, x]
pdBuiltin "smul_hi16" [x, y] = Just [y, x]
pdBuiltin "smul_hi32" [x, y] = Just [y, x]
pdBuiltin "smul_hi64" [x, y] = Just [y, x]
pdBuiltin "smad_hi8" [a, b, _c] = Just [b, a, iConst Int8 1]
pdBuiltin "smad_hi16" [a, b, _c] = Just [b, a, iConst Int16 1]
pdBuiltin "smad_hi32" [a, b, _c] = Just [b, a, iConst Int32 1]
pdBuiltin "smad_hi64" [a, b, _c] = Just [b, a, iConst Int64 1]
pdBuiltin "isnan16" [_] = Just [untyped false]
pdBuiltin "isnan32" [_] = Just [untyped false]
pdBuiltin "isnan64" [_] = Just [untyped false]
pdBuiltin "isinf16" [_] = Just [untyped false]
pdBuiltin "isinf32" [_] = Just [untyped false]
pdBuiltin "isinf64" [_] = Just [untyped false]
pdBuiltin "round16" [_] = Just [fConst Float16 0]
pdBuiltin "round32" [_] = Just [fConst Float32 0]
pdBuiltin "round64" [_] = Just [fConst Float64 0]
pdBuiltin "ceil16" [_] = Just [fConst Float16 0]
pdBuiltin "ceil32" [_] = Just [fConst Float32 0]
pdBuiltin "ceil64" [_] = Just [fConst Float64 0]
pdBuiltin "floor16" [_] = Just [fConst Float16 0]
pdBuiltin "floor32" [_] = Just [fConst Float32 0]
pdBuiltin "floor64" [_] = Just [fConst Float64 0]
pdBuiltin "nextafter16" [_, _] = Just [fConst Float16 1, fConst Float16 0]
pdBuiltin "nextafter32" [_, _] = Just [fConst Float32 1, fConst Float32 0]
pdBuiltin "nextafter64" [_, _] = Just [fConst Float64 1, fConst Float64 0]
pdBuiltin "clz8" [_] = Just [iConst Int32 0]
pdBuiltin "clz16" [_] = Just [iConst Int32 0]
pdBuiltin "clz32" [_] = Just [iConst Int32 0]
pdBuiltin "clz64" [_] = Just [iConst Int32 0]
pdBuiltin "ctz8" [_] = Just [iConst Int32 0]
pdBuiltin "ctz16" [_] = Just [iConst Int32 0]
pdBuiltin "ctz32" [_] = Just [iConst Int32 0]
pdBuiltin "ctz64" [_] = Just [iConst Int32 0]
pdBuiltin "popc8" [_] = Just [iConst Int32 0]
pdBuiltin "popc16" [_] = Just [iConst Int32 0]
pdBuiltin "popc32" [_] = Just [iConst Int32 0]
pdBuiltin "popc64" [_] = Just [iConst Int32 0]
pdBuiltin _ _ = Nothing
