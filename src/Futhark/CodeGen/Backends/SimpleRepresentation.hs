{-# LANGUAGE QuasiQuotes #-}
-- | Simple C runtime representation.
module Futhark.CodeGen.Backends.SimpleRepresentation
  ( sameRepresentation
  , tupleField
  , tupleFieldExp
  , funName
  , defaultMemBlockType
  , builtInFunctionDefs
  , intTypeToCType
  , floatTypeToCType
  , primTypeToCType

    -- * Primitive value operations
  , cIntOps
  , cFloat32Ops
  , cFloat64Ops
  , cFloatConvOps

    -- * Specific builtin functions
  , c_log32, c_sqrt32, c_exp32, c_sin32, c_cos32, c_atan2_32, c_isnan32, c_isinf32
  , c_log64, c_sqrt64, c_exp64, c_sin64, c_cos64, c_atan2_64, c_isnan64, c_isinf64
  )
  where

import qualified Language.C.Syntax as C
import qualified Language.C.Quote.C as C

import Futhark.CodeGen.ImpCode
import Futhark.Util.Pretty (pretty)

intTypeToCType :: IntType -> C.Type
intTypeToCType Int8 = [C.cty|typename int8_t|]
intTypeToCType Int16 = [C.cty|typename int16_t|]
intTypeToCType Int32 = [C.cty|typename int32_t|]
intTypeToCType Int64 = [C.cty|typename int64_t|]

uintTypeToCType :: IntType -> C.Type
uintTypeToCType Int8 = [C.cty|typename uint8_t|]
uintTypeToCType Int16 = [C.cty|typename uint16_t|]
uintTypeToCType Int32 = [C.cty|typename uint32_t|]
uintTypeToCType Int64 = [C.cty|typename uint64_t|]

floatTypeToCType :: FloatType -> C.Type
floatTypeToCType Float32 = [C.cty|float|]
floatTypeToCType Float64 = [C.cty|double|]

primTypeToCType :: PrimType -> C.Type
primTypeToCType (IntType t) = intTypeToCType t
primTypeToCType (FloatType t) = floatTypeToCType t
primTypeToCType Bool = [C.cty|char|]
primTypeToCType Cert = [C.cty|char|]

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

cIntOps :: [C.Definition]
cIntOps = concatMap (`map` [minBound..maxBound]) ops
  where ops = [mkAdd, mkSub, mkMul,
               mkUDiv, mkUMod,
               mkSDiv, mkSMod,
               mkSQuot, mkSRem,
               mkShl, mkLShr, mkAShr,
               mkAnd, mkOr, mkXor,
               mkUlt, mkUle,  mkSlt, mkSle,
               mkPow
              ] ++
              map mkSExt [minBound..maxBound] ++
              map mkZExt [minBound..maxBound]

        taggedI s Int8 = s ++ "8"
        taggedI s Int16 = s ++ "16"
        taggedI s Int32 = s ++ "32"
        taggedI s Int64 = s ++ "64"

        mkAdd = simpleIntOp "add" [C.cexp|x + y|]
        mkSub = simpleIntOp "sub" [C.cexp|x - y|]
        mkMul = simpleIntOp "mul" [C.cexp|x * y|]
        mkUDiv = simpleUintOp "udiv" [C.cexp|x / y|]
        mkUMod = simpleUintOp "umod" [C.cexp|x % y|]

        mkSDiv t =
          let ct = intTypeToCType t
          in [C.cedecl|static inline $ty:ct $id:(taggedI "sdiv" t)($ty:ct x, $ty:ct y) {
                         $ty:ct q = x / y;
                         $ty:ct r = x % y;
                         return q -
                           (((r != 0) && ((r < 0) != (y < 0))) ? 1 : 0);
             }|]
        mkSMod t =
          let ct = intTypeToCType t
          in [C.cedecl|static inline $ty:ct $id:(taggedI "smod" t)($ty:ct x, $ty:ct y) {
                         $ty:ct r = x % y;
                         return r +
                           ((r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0)) ? 0 : y);
              }|]

        mkSQuot = simpleIntOp "squot" [C.cexp|x / y|]
        mkSRem = simpleIntOp "srem" [C.cexp|x % y|]
        mkShl = simpleUintOp "shl" [C.cexp|x << y|]
        mkLShr = simpleUintOp "lshr" [C.cexp|x >> y|]
        mkAShr = simpleIntOp "ashr" [C.cexp|x >> y|]
        mkAnd = simpleUintOp "and" [C.cexp|x & y|]
        mkOr = simpleUintOp "or" [C.cexp|x | y|]
        mkXor = simpleUintOp "xor" [C.cexp|x ^ y|]
        mkUlt = uintCmpOp "ult" [C.cexp|x < y|]
        mkUle = uintCmpOp "ule" [C.cexp|x <= y|]
        mkSlt = intCmpOp "slt" [C.cexp|x < y|]
        mkSle = intCmpOp "sle" [C.cexp|x <= y|]

        mkPow t =
          let ct = intTypeToCType t
          in [C.cedecl|static inline $ty:ct $id:(taggedI "pow" t)($ty:ct x, $ty:ct y) {
                         $ty:ct res = 1, rem = y;
                         while (rem != 0) {
                           if (rem & 1) {
                             res *= x;
                           }
                           rem >>= 1;
                           x *= x;
                         }
                         return res;
              }|]

        mkSExt from_t to_t =
          [C.cedecl|static inline $ty:to_ct
                    $id:name($ty:from_ct x) { return x;} |]
          where name = "sext_"++pretty from_t++"_"++pretty to_t
                from_ct = intTypeToCType from_t
                to_ct = intTypeToCType to_t

        mkZExt from_t to_t =
          [C.cedecl|static inline $ty:to_ct
                    $id:name($ty:from_ct x) { return x;} |]
          where name = "zext_"++pretty from_t++"_"++pretty to_t
                from_ct = uintTypeToCType from_t
                to_ct = uintTypeToCType to_t

        simpleUintOp s e t =
          [C.cedecl|static inline $ty:ct $id:(taggedI s t)($ty:ct x, $ty:ct y) { return $exp:e; }|]
            where ct = uintTypeToCType t
        simpleIntOp s e t =
          [C.cedecl|static inline $ty:ct $id:(taggedI s t)($ty:ct x, $ty:ct y) { return $exp:e; }|]
            where ct = intTypeToCType t
        intCmpOp s e t =
          [C.cedecl|static inline char $id:(taggedI s t)($ty:ct x, $ty:ct y) { return $exp:e; }|]
            where ct = intTypeToCType t
        uintCmpOp s e t =
          [C.cedecl|static inline char $id:(taggedI s t)($ty:ct x, $ty:ct y) { return $exp:e; }|]
            where ct = uintTypeToCType t

cFloat32Ops :: [C.Definition]
cFloat64Ops :: [C.Definition]
cFloatConvOps :: [C.Definition]
(cFloat32Ops, cFloat64Ops, cFloatConvOps) =
  ( map ($Float32) mkOps
  , map ($Float64) mkOps
  , [ mkFPConvFF "fpconv" from to |
      from <- [minBound..maxBound],
      to <- [minBound..maxBound] ])
  where taggedF s Float32 = s ++ "32"
        taggedF s Float64 = s ++ "64"
        convOp s from to = s ++ "_" ++ pretty from ++ "_" ++ pretty to

        mkOps = [mkFDiv, mkFAdd, mkFSub, mkFMul, mkPow, mkCmpLt, mkCmpLe] ++
                map (mkFPConvIF "sitofp") [minBound..maxBound] ++
                map (mkFPConvUF "uitofp") [minBound..maxBound] ++
                map (flip $ mkFPConvFI "fptosi") [minBound..maxBound] ++
                map (flip $ mkFPConvFU "fptoui") [minBound..maxBound]

        mkFDiv = simpleFloatOp "fdiv" [C.cexp|x / y|]
        mkFAdd = simpleFloatOp "fadd" [C.cexp|x + y|]
        mkFSub = simpleFloatOp "fsub" [C.cexp|x - y|]
        mkFMul = simpleFloatOp "fmul" [C.cexp|x * y|]
        mkCmpLt = floatCmpOp "cmplt" [C.cexp|x < y|]
        mkCmpLe = floatCmpOp "cmple" [C.cexp|x <= y|]

        mkPow Float32 =
          [C.cedecl|static inline float fpow32(float x, float y) { return pow(x, y); }|]
        mkPow Float64 =
          [C.cedecl|static inline double fpow64(double x, double y) { return pow(x, y); }|]

        mkFPConv from_f to_f s from_t to_t =
          [C.cedecl|static inline $ty:to_ct
                    $id:(convOp s from_t to_t)($ty:from_ct x) { return x;} |]
          where from_ct = from_f from_t
                to_ct = to_f to_t

        mkFPConvFF = mkFPConv floatTypeToCType floatTypeToCType
        mkFPConvFI = mkFPConv floatTypeToCType intTypeToCType
        mkFPConvIF = mkFPConv intTypeToCType floatTypeToCType
        mkFPConvFU = mkFPConv floatTypeToCType uintTypeToCType
        mkFPConvUF = mkFPConv uintTypeToCType floatTypeToCType

        simpleFloatOp s e t =
          [C.cedecl|static inline $ty:ct $id:(taggedF s t)($ty:ct x, $ty:ct y) { return $exp:e; }|]
            where ct = floatTypeToCType t
        floatCmpOp s e t =
          [C.cedecl|static inline char $id:(taggedF s t)($ty:ct x, $ty:ct y) { return $exp:e; }|]
            where ct = floatTypeToCType t

c_log32 :: C.Func
c_log32 = [C.cfun|
    static inline float $id:(funName' "log32")(float x) {
      return log(x);
    }
    |]

c_sqrt32 :: C.Func
c_sqrt32 = [C.cfun|
    static inline float $id:(funName' "sqrt32")(float x) {
      return sqrt(x);
    }
    |]

c_exp32 ::C.Func
c_exp32 = [C.cfun|
    static inline float $id:(funName' "exp32")(float x) {
      return exp(x);
    }
  |]

c_cos32 ::C.Func
c_cos32 = [C.cfun|
    static inline float $id:(funName' "cos32")(float x) {
      return cos(x);
    }
  |]

c_sin32 ::C.Func
c_sin32 = [C.cfun|
    static inline float $id:(funName' "sin32")(float x) {
      return sin(x);
    }
  |]

c_atan2_32 ::C.Func
c_atan2_32 = [C.cfun|
    static inline double $id:(funName' "atan2_32")(double x, double y) {
      return atan2(x,y);
    }
  |]

c_isnan32 ::C.Func
c_isnan32 = [C.cfun|
    static inline char $id:(funName' "isnan32")(float x) {
      return isnan(x);
    }
  |]

c_isinf32 ::C.Func
c_isinf32 = [C.cfun|
    static inline char $id:(funName' "isinf32")(float x) {
      return isinf(x);
    }
  |]

c_log64 :: C.Func
c_log64 = [C.cfun|
    static inline double $id:(funName' "log64")(double x) {
      return log(x);
    }
    |]

c_sqrt64 :: C.Func
c_sqrt64 = [C.cfun|
    static inline double $id:(funName' "sqrt64")(double x) {
      return sqrt(x);
    }
    |]

c_exp64 ::C.Func
c_exp64 = [C.cfun|
    static inline double $id:(funName' "exp64")(double x) {
      return exp(x);
    }
  |]

c_cos64 ::C.Func
c_cos64 = [C.cfun|
    static inline double $id:(funName' "cos64")(double x) {
      return cos(x);
    }
  |]

c_sin64 ::C.Func
c_sin64 = [C.cfun|
    static inline double $id:(funName' "sin64")(double x) {
      return sin(x);
    }
  |]

c_atan2_64 ::C.Func
c_atan2_64 = [C.cfun|
    static inline double $id:(funName' "atan2_64")(double x, double y) {
      return atan2(x,y);
    }
  |]

c_isnan64 ::C.Func
c_isnan64 = [C.cfun|
    static inline char $id:(funName' "isnan64")(double x) {
      return isnan(x);
    }
  |]

c_isinf64 ::C.Func
c_isinf64 = [C.cfun|
    static inline char $id:(funName' "isinf64")(double x) {
      return isinf(x);
    }
  |]

-- | C definitions of the Futhark "standard library".
builtInFunctionDefs :: [C.Func]
builtInFunctionDefs =
  [c_log32, c_sqrt32, c_exp32, c_cos32, c_sin32, c_atan2_32, c_isnan32, c_isinf32,
   c_log64, c_sqrt64, c_exp64, c_cos64, c_sin64, c_atan2_64, c_isnan64, c_isinf64]
