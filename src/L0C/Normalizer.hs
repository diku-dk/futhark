module L0.Normalizer where

import L0.AbSyn

import Data.Loc

intConst :: Int -> SrcLoc -> Exp Type
intConst v s = Literal $ IntVal v s

realConst :: Double -> SrcLoc -> Exp Type
realConst v s = Literal $ RealVal v s

zero :: SrcLoc -> Exp Type
zero = intConst 0

one :: SrcLoc -> Exp Type
one = intConst 1

negOne :: SrcLoc -> Type -> Exp Type
negOne s (Int _) = intConst (-1) s
negOne s (Real _) = realConst (-1.0) s
negOne _ _ = error "Partial functions are baaad."

singleton :: Exp Type -> Type -> SrcLoc -> Exp Type
singleton x = ArrayLit [x]

-- don't know what to do about maybe types when declaring built-ins, so run
-- this after the typechecker has put in types.

binopLambda :: BinOp -> Type -> SrcLoc -> Lambda Type
binopLambda op t s =
  let
    x = Ident "x" t s
    y = Ident "y" t s
  in
    AnonymFun [x,y] (BinOp op (Var x) (Var y) t s) t s

normalize :: Exp Type -> Exp Type
normalize (Literal v) = Literal v
normalize (BinOp Minus x y t s) =
  -- x - y = x + (-1) * y
  -- assume no type coersion (yielding a partial method)
  let
    x' = normalize x
    y' = normalize y
    y'' = Reduce (binopLambda Times t s) (negOne s t) (singleton y' t s) t s
  in
    Reduce (binopLambda Plus t s) x' (singleton y'' t s) t s
normalize x = x

-- remember to improve location proppagation
