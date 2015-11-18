{-# LANGUAGE QuasiQuotes #-}
-- | Simple C runtime representation.
module Futhark.CodeGen.Backends.SimpleRepresentation
  ( sameRepresentation
  , tupleField
  , tupleFieldExp
  , funName
  , defaultMemBlockType
  , builtInFunctionDefs
    -- * Specific builtin functions
  , c_toFloat32, c_trunc32, c_log32, c_sqrt32, c_exp32
  , c_toFloat64, c_trunc64, c_log64, c_sqrt64, c_exp64
  )
  where

import qualified Language.C.Syntax as C
import qualified Language.C.Quote.C as C

import Futhark.CodeGen.ImpCode

-- | True if both types map to the same runtime representation.  This
-- is the case if they are identical modulo uniqueness.
sameRepresentation :: [Type] -> [Type] -> Bool
sameRepresentation ets1 ets2
  | length ets1 == length ets2 =
    and $ zipWith sameRepresentation' ets1 ets2
  | otherwise = False

sameRepresentation' :: Type -> Type -> Bool
sameRepresentation' (Scalar t1) (Scalar t2) =
  t1 == t2
sameRepresentation' (Mem _ space1) (Mem _ space2) = space1 == space2
sameRepresentation' _ _ = False

-- | @tupleField i@ is the name of field number @i@ in a tuple.
tupleField :: Int -> String
tupleField i = "elem_" ++ show i

-- | @tupleFieldExp e i@ is the expression for accesing field @i@ of
-- tuple @e@.  If @e@ is an lvalue, so will the resulting expression
-- be.
tupleFieldExp :: C.Exp -> Int -> C.Exp
tupleFieldExp e i = [C.cexp|$exp:e.$id:(tupleField i)|]

-- | @funName f@ is the name of the C function corresponding to
-- the Futhark function @f@.
funName :: Name -> String
funName = ("futhark_"++) . nameToString

funName' :: String -> String
funName' = funName . nameFromString

-- | The type of memory blocks in the default memory space.
defaultMemBlockType :: C.Type
defaultMemBlockType = [C.cty|unsigned char*|]

c_toFloat32 :: C.Func
c_toFloat32 = [C.cfun|
              float $id:(funName' "toFloat32")(int x) {
                return x;
              }
             |]

c_trunc32 :: C.Func
c_trunc32 = [C.cfun|
    int $id:(funName' "trunc32")(float x) {
      return x;
    }
   |]

c_log32 :: C.Func
c_log32 = [C.cfun|
    float $id:(funName' "log32")(float x) {
      return log(x);
    }
    |]

c_sqrt32 :: C.Func
c_sqrt32 = [C.cfun|
    float $id:(funName' "sqrt32")(float x) {
      return sqrt(x);
    }
    |]

c_exp32 ::C.Func
c_exp32 = [C.cfun|
    float $id:(funName' "exp32")(float x) {
      return exp(x);
    }
  |]

c_toFloat64 :: C.Func
c_toFloat64 = [C.cfun|
              double $id:(funName' "toFloat64")(int x) {
                return x;
              }
             |]

c_trunc64 :: C.Func
c_trunc64 = [C.cfun|
    int $id:(funName' "trunc64")(double x) {
      return x;
    }
   |]

c_log64 :: C.Func
c_log64 = [C.cfun|
    double $id:(funName' "log64")(double x) {
      return log(x);
    }
    |]

c_sqrt64 :: C.Func
c_sqrt64 = [C.cfun|
    double $id:(funName' "sqrt64")(double x) {
      return sqrt(x);
    }
    |]

c_exp64 ::C.Func
c_exp64 = [C.cfun|
    double $id:(funName' "exp64")(double x) {
      return exp(x);
    }
  |]

-- | C definitions of the Futhark "standard library".
builtInFunctionDefs :: [C.Func]
builtInFunctionDefs =
  [c_toFloat32, c_trunc32, c_log32, c_sqrt32, c_exp32,
   c_toFloat64, c_trunc64, c_log64, c_sqrt64, c_exp64]
