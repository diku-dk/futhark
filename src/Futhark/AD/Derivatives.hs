{-# LANGUAGE OverloadedStrings #-}

-- | Partial derivatives of scalar Futhark operations and built-in functions.
module Futhark.AD.Derivatives
  ( pdBuiltin,
    pdBinOp,
  )
where

import Data.Bifunctor (bimap)
import Futhark.Analysis.PrimExp.Convert
import Futhark.IR.Syntax.Core
import Futhark.Util.IntegralExp
import Prelude hiding (quot)

iConst :: IntType -> Integer -> PrimExp VName
iConst it x = ValueExp $ IntValue $ intValue it x

fConst :: FloatType -> Double -> PrimExp VName
fConst ft x = ValueExp $ FloatValue $ floatValue ft x

untyped2 :: (TPrimExp t v, TPrimExp t v) -> (PrimExp v, PrimExp v)
untyped2 = bimap untyped untyped

type OnBinOp t v = (TPrimExp t v -> TPrimExp t v -> (TPrimExp t v, TPrimExp t v))

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
  OnBinOp Float v ->
  OnBinOp Double v ->
  FloatType ->
  PrimExp v ->
  PrimExp v ->
  (PrimExp v, PrimExp v)
floatBinOp f _ Float32 a b = untyped2 $ f (isF32 a) (isF32 b)
floatBinOp _ f Float64 a b = untyped2 $ f (isF64 a) (isF64 b)

pdBinOp :: BinOp -> PrimExp VName -> PrimExp VName -> (PrimExp VName, PrimExp VName)
pdBinOp (Add it _) _ _ = (iConst it 1, iConst it 1)
pdBinOp (Sub it _) _ _ = (iConst it 1, iConst it (-1))
pdBinOp (Mul _ _) x y = (y, x)
pdBinOp (SDiv it _) a b =
  intBinOp derivs derivs derivs derivs it a b
  where
    derivs :: IntegralExp t => t -> t -> (t, t)
    derivs x y = (1 `quot` y, negate (x `quot` (y * y)))
--
pdBinOp (FAdd ft) _ _ = (fConst ft 1, fConst ft 1)
pdBinOp (FSub ft) _ _ = (fConst ft 1, fConst ft (-1))
pdBinOp (FMul _) x y = (y, x)
pdBinOp (FDiv ft) a b =
  floatBinOp derivs derivs ft a b
  where
    derivs :: Floating t => t -> t -> (t, t)
    derivs x y = (1 / x, negate (x / (y ** 2)))
pdBinOp (FPow ft) a b =
  floatBinOp derivs derivs ft a b
  where
    derivs :: Floating t => t -> t -> (t, t)
    derivs x y = (y * (x ** (y -1)), (x ** y) * log x)

-- | @pdBuiltin f args i@ computes the partial derivative of @f@
-- applied to @args@ with respect to each of its arguments.  Returns
-- 'Nothing' if no such derivative is known.
pdBuiltin :: Name -> [PrimExp VName] -> Maybe [PrimExp VName]
pdBuiltin "sqrt32" [x] =
  Just [untyped $ 1 / (2 * sqrt (isF64 x))]
pdBuiltin "sqrt64" [x] =
  Just [untyped $ 1 / (2 * sqrt (isF64 x))]
pdBuiltin "log32" [x] =
  Just [untyped $ 1 / isF32 x]
pdBuiltin "log64" [x] =
  Just [untyped $ 1 / isF64 x]
pdBuiltin "log10_32" [x] =
  Just [untyped $ 1 / (isF32 x * log 10)]
pdBuiltin "log10_64" [x] =
  Just [untyped $ 1 / (isF64 x * log 10)]
pdBuiltin "log2_32" [x] =
  Just [untyped $ 1 / (isF32 x * log 2)]
pdBuiltin "log2_64" [x] =
  Just [untyped $ 1 / (isF64 x * log 2)]
pdBuiltin "exp32" [x] =
  Just [untyped $ exp (isF32 x)]
pdBuiltin "exp64" [x] =
  Just [untyped $ exp (isF64 x)]
pdBuiltin "sin32" [x] =
  Just [untyped $ cos (isF32 x)]
pdBuiltin "sin64" [x] =
  Just [untyped $ cos (isF64 x)]
pdBuiltin "sinh32" [x] =
  Just [untyped $ cosh (isF32 x)]
pdBuiltin "sinh64" [x] =
  Just [untyped $ cosh (isF64 x)]
pdBuiltin "cos32" [x] =
  Just [untyped $ - sin (isF32 x)]
pdBuiltin "cos64" [x] =
  Just [untyped $ - sin (isF64 x)]
pdBuiltin "cosh32" [x] =
  Just [untyped $ sinh (isF32 x)]
pdBuiltin "cosh64" [x] =
  Just [untyped $ sinh (isF64 x)]
pdBuiltin "tan32" [x] =
  Just [untyped $ 1 / (cos (isF32 x) ** 2)]
pdBuiltin "tan64" [x] =
  Just [untyped $ 1 / (cos (isF64 x) ** 2)]
pdBuiltin "asin32" [x] =
  Just [untyped $ 1 / sqrt (1 - isF32 x ** 2)]
pdBuiltin "asin64" [x] =
  Just [untyped $ 1 / sqrt (1 - isF64 x ** 2)]
pdBuiltin "asinh32" [x] =
  Just [untyped $ 1 / sqrt (1 + isF32 x ** 2)]
pdBuiltin "asinh64" [x] =
  Just [untyped $ 1 / sqrt (1 + isF64 x ** 2)]
pdBuiltin "acos32" [x] =
  Just [untyped $ - 1 / sqrt (1 - isF32 x ** 2)]
pdBuiltin "acos64" [x] =
  Just [untyped $ - 1 / sqrt (1 - isF64 x ** 2)]
pdBuiltin "acosh32" [x] =
  Just [untyped $ 1 / sqrt (isF32 x ** 2 - 1)]
pdBuiltin "acosh64" [x] =
  Just [untyped $ 1 / sqrt (isF64 x ** 2 - 1)]
pdBuiltin "atan32" [x] =
  Just [untyped $ 1 / (1 + isF32 x ** 2)]
pdBuiltin "atan64" [x] =
  Just [untyped $ 1 / (1 + isF64 x ** 2)]
pdBuiltin "atanh32" [x] =
  Just [untyped $ cosh (isF32 x) ** 2]
pdBuiltin "atanh64" [x] =
  Just [untyped $ cosh (isF64 x) ** 2]
pdBuiltin "atan2_32" [x, y] =
  Just
    [ untyped $ - isF32 y / (isF32 x ** 2 + isF32 y ** 2),
      untyped $ - isF32 x / (isF32 x ** 2 + isF32 y ** 2)
    ]
pdBuiltin "atan2_64" [x, y] =
  Just
    [ untyped $ - isF64 y / (isF64 x ** 2 + isF64 y ** 2),
      untyped $ - isF64 x / (isF64 x ** 2 + isF64 y ** 2)
    ]
-- More problematic derivatives follow bellow
pdBuiltin "round32" [_] =
  Just [fConst Float32 0]
pdBuiltin "round64" [_] =
  Just [fConst Float64 0]
pdBuiltin "ceil32" [_] =
  Just [fConst Float32 0]
pdBuiltin "ceil64" [_] =
  Just [fConst Float64 0]
pdBuiltin "floor32" [_] =
  Just [fConst Float32 0]
pdBuiltin "floor64" [_] =
  Just [fConst Float64 0]
pdBuiltin _ _ =
  Nothing
