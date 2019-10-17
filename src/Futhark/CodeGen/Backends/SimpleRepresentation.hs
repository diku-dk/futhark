{-# LANGUAGE QuasiQuotes #-}
-- | Simple C runtime representation.
module Futhark.CodeGen.Backends.SimpleRepresentation
  ( sameRepresentation
  , tupleField
  , tupleFieldExp
  , funName
  , defaultMemBlockType
  , intTypeToCType
  , floatTypeToCType
  , primTypeToCType
  , signedPrimTypeToCType

    -- * Primitive value operations
  , cIntOps
  , cFloat32Ops, cFloat32Funs
  , cFloat64Ops, cFloat64Funs
  , cFloatConvOps
  )
  where

import qualified Language.C.Syntax as C
import qualified Language.C.Quote.C as C

import Futhark.CodeGen.ImpCode
import Futhark.Util.Pretty (prettyOneLine)
import Futhark.Util (zEncodeString)

-- | The C type corresponding to a signed integer type.
intTypeToCType :: IntType -> C.Type
intTypeToCType Int8 = [C.cty|typename int8_t|]
intTypeToCType Int16 = [C.cty|typename int16_t|]
intTypeToCType Int32 = [C.cty|typename int32_t|]
intTypeToCType Int64 = [C.cty|typename int64_t|]

-- | The C type corresponding to an unsigned integer type.
uintTypeToCType :: IntType -> C.Type
uintTypeToCType Int8 = [C.cty|typename uint8_t|]
uintTypeToCType Int16 = [C.cty|typename uint16_t|]
uintTypeToCType Int32 = [C.cty|typename uint32_t|]
uintTypeToCType Int64 = [C.cty|typename uint64_t|]

-- | The C type corresponding to a float type.
floatTypeToCType :: FloatType -> C.Type
floatTypeToCType Float32 = [C.cty|float|]
floatTypeToCType Float64 = [C.cty|double|]

-- | The C type corresponding to a primitive type.  Integers are
-- assumed to be unsigned.
primTypeToCType :: PrimType -> C.Type
primTypeToCType (IntType t) = intTypeToCType t
primTypeToCType (FloatType t) = floatTypeToCType t
primTypeToCType Bool = [C.cty|typename bool|]
primTypeToCType Cert = [C.cty|typename bool|]

-- | The C type corresponding to a primitive type.  Integers are
-- assumed to have the specified sign.
signedPrimTypeToCType :: Signedness -> PrimType -> C.Type
signedPrimTypeToCType TypeUnsigned (IntType t) = uintTypeToCType t
signedPrimTypeToCType TypeDirect (IntType t) = intTypeToCType t
signedPrimTypeToCType _ t = primTypeToCType t

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
tupleField i = "v" ++ show i

-- | @tupleFieldExp e i@ is the expression for accesing field @i@ of
-- tuple @e@.  If @e@ is an lvalue, so will the resulting expression
-- be.
tupleFieldExp :: C.ToExp a => a -> Int -> C.Exp
tupleFieldExp e i = [C.cexp|$exp:e.$id:(tupleField i)|]

-- | @funName f@ is the name of the C function corresponding to
-- the Futhark function @f@.
funName :: Name -> String
funName = ("futrts_"++) . zEncodeString . nameToString

funName' :: String -> String
funName' = funName . nameFromString

-- | The type of memory blocks in the default memory space.
defaultMemBlockType :: C.Type
defaultMemBlockType = [C.cty|char*|]

cIntOps :: [C.Definition]
cIntOps = concatMap (`map` [minBound..maxBound]) ops
  where ops = [mkAdd, mkSub, mkMul,
               mkUDiv, mkUMod,
               mkSDiv, mkSMod,
               mkSQuot, mkSRem,
               mkSMin, mkUMin,
               mkSMax, mkUMax,
               mkShl, mkLShr, mkAShr,
               mkAnd, mkOr, mkXor,
               mkUlt, mkUle,  mkSlt, mkSle,
               mkPow,
               mkIToB, mkBToI
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
        mkUMax = simpleUintOp "umax" [C.cexp|x < y ? y : x|]
        mkUMin = simpleUintOp "umin" [C.cexp|x < y ? x : y|]

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
        mkSMax = simpleIntOp "smax" [C.cexp|x < y ? y : x|]
        mkSMin = simpleIntOp "smin" [C.cexp|x < y ? x : y|]
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

        -- We define some operations as macros rather than functions,
        -- because this allows us to use them as constant expressions
        -- in things like array sizes and static initialisers.
        macro name rhs =
          [C.cedecl|$esc:("#define " ++ name ++ "(x) (" ++ prettyOneLine rhs ++ ")")|]

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

        mkSExt from_t to_t = macro name [C.cexp|($ty:to_ct)(($ty:from_ct)x)|]
          where name = "sext_"++pretty from_t++"_"++pretty to_t
                from_ct = intTypeToCType from_t
                to_ct = intTypeToCType to_t

        mkZExt from_t to_t = macro name [C.cexp|($ty:to_ct)(($ty:from_ct)x)|]
          where name = "zext_"++pretty from_t++"_"++pretty to_t
                from_ct = uintTypeToCType from_t
                to_ct = uintTypeToCType to_t

        mkBToI to_t =
          [C.cedecl|static inline $ty:to_ct
                    $id:name($ty:from_ct x) { return x; } |]
          where name = "btoi_bool_"++pretty to_t
                from_ct = primTypeToCType Bool
                to_ct = intTypeToCType to_t

        mkIToB from_t =
          [C.cedecl|static inline $ty:to_ct
                    $id:name($ty:from_ct x) { return x; } |]
          where name = "itob_"++pretty from_t++"_bool"
                to_ct = primTypeToCType Bool
                from_ct = intTypeToCType from_t

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

        mkOps = [mkFDiv, mkFAdd, mkFSub, mkFMul, mkFMin, mkFMax, mkPow, mkCmpLt, mkCmpLe] ++
                map (mkFPConvIF "sitofp") [minBound..maxBound] ++
                map (mkFPConvUF "uitofp") [minBound..maxBound] ++
                map (flip $ mkFPConvFI "fptosi") [minBound..maxBound] ++
                map (flip $ mkFPConvFU "fptoui") [minBound..maxBound]

        mkFDiv = simpleFloatOp "fdiv" [C.cexp|x / y|]
        mkFAdd = simpleFloatOp "fadd" [C.cexp|x + y|]
        mkFSub = simpleFloatOp "fsub" [C.cexp|x - y|]
        mkFMul = simpleFloatOp "fmul" [C.cexp|x * y|]
        mkFMin = simpleFloatOp "fmin" [C.cexp|fmin(x, y)|]
        mkFMax = simpleFloatOp "fmax" [C.cexp|fmax(x, y)|]
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

cFloat32Funs :: [C.Definition]
cFloat32Funs = [C.cunit|
    static inline float $id:(funName' "log32")(float x) {
      return log(x);
    }

    static inline float $id:(funName' "log2_32")(float x) {
      return log2(x);
    }

    static inline float $id:(funName' "log10_32")(float x) {
      return log10(x);
    }

    static inline float $id:(funName' "sqrt32")(float x) {
      return sqrt(x);
    }

    static inline float $id:(funName' "exp32")(float x) {
      return exp(x);
    }

    static inline float $id:(funName' "cos32")(float x) {
      return cos(x);
    }

    static inline float $id:(funName' "sin32")(float x) {
      return sin(x);
    }

    static inline float $id:(funName' "tan32")(float x) {
      return tan(x);
    }

    static inline float $id:(funName' "acos32")(float x) {
      return acos(x);
    }

    static inline float $id:(funName' "asin32")(float x) {
      return asin(x);
    }

    static inline float $id:(funName' "atan32")(float x) {
      return atan(x);
    }

    static inline float $id:(funName' "atan2_32")(float x, float y) {
      return atan2(x,y);
    }

    static inline float $id:(funName' "gamma32")(float x) {
      return tgamma(x);
    }

    static inline float $id:(funName' "lgamma32")(float x) {
      return lgamma(x);
    }

    static inline char $id:(funName' "isnan32")(float x) {
      return isnan(x);
    }

    static inline char $id:(funName' "isinf32")(float x) {
      return isinf(x);
    }

    static inline typename int32_t $id:(funName' "to_bits32")(float x) {
      union {
        float f;
        typename int32_t t;
      } p;
      p.f = x;
      return p.t;
    }

    static inline float $id:(funName' "from_bits32")(typename int32_t x) {
      union {
        typename int32_t f;
        float t;
      } p;
      p.f = x;
      return p.t;
    }

$esc:("#ifdef __OPENCL_VERSION__")
    static inline float fmod32(float x, float y) {
      return fmod(x, y);
    }
    static inline float $id:(funName' "round32")(float x) {
      return rint(x);
    }
    static inline float $id:(funName' "floor32")(float x) {
      return floor(x);
    }
    static inline float $id:(funName' "ceil32")(float x) {
      return ceil(x);
    }
    static inline float $id:(funName' "lerp32")(float v0, float v1, float t) {
      return mix(v0, v1, t);
    }
$esc:("#else")
    static inline float fmod32(float x, float y) {
      return fmodf(x, y);
    }
    static inline float $id:(funName' "round32")(float x) {
      return rintf(x);
    }
    static inline float $id:(funName' "floor32")(float x) {
      return floorf(x);
    }
    static inline float $id:(funName' "ceil32")(float x) {
      return ceilf(x);
    }
    static inline float $id:(funName' "lerp32")(float v0, float v1, float t) {
      return v0 + (v1-v0)*t;
    }
$esc:("#endif")
|]

cFloat64Funs :: [C.Definition]
cFloat64Funs = [C.cunit|
    static inline double $id:(funName' "log64")(double x) {
      return log(x);
    }

    static inline double $id:(funName' "log2_64")(double x) {
      return log2(x);
    }

    static inline double $id:(funName' "log10_64")(double x) {
      return log10(x);
    }

    static inline double $id:(funName' "sqrt64")(double x) {
      return sqrt(x);
    }

    static inline double $id:(funName' "exp64")(double x) {
      return exp(x);
    }

    static inline double $id:(funName' "cos64")(double x) {
      return cos(x);
    }

    static inline double $id:(funName' "sin64")(double x) {
      return sin(x);
    }

    static inline double $id:(funName' "tan64")(double x) {
      return tan(x);
    }

    static inline double $id:(funName' "acos64")(double x) {
      return acos(x);
    }

    static inline double $id:(funName' "asin64")(double x) {
      return asin(x);
    }

    static inline double $id:(funName' "atan64")(double x) {
      return atan(x);
    }

    static inline double $id:(funName' "atan2_64")(double x, double y) {
      return atan2(x,y);
    }

    static inline double $id:(funName' "gamma64")(double x) {
      return tgamma(x);
    }

    static inline double $id:(funName' "lgamma64")(double x) {
      return lgamma(x);
    }

    static inline double $id:(funName' "round64")(double x) {
      return rint(x);
    }

    static inline double $id:(funName' "ceil64")(double x) {
      return ceil(x);
    }

    static inline double $id:(funName' "floor64")(double x) {
      return floor(x);
    }

    static inline char $id:(funName' "isnan64")(double x) {
      return isnan(x);
    }

    static inline char $id:(funName' "isinf64")(double x) {
      return isinf(x);
    }

    static inline typename int64_t $id:(funName' "to_bits64")(double x) {
      union {
        double f;
        typename int64_t t;
      } p;
      p.f = x;
      return p.t;
    }

    static inline double $id:(funName' "from_bits64")(typename int64_t x) {
      union {
        typename int64_t f;
        double t;
      } p;
      p.f = x;
      return p.t;
    }

    static inline float fmod64(float x, float y) {
      return fmod(x, y);
    }

$esc:("#ifdef __OPENCL_VERSION__")
    static inline double $id:(funName' "lerp64")(double v0, double v1, double t) {
      return mix(v0, v1, t);
    }
$esc:("#else")
    static inline double $id:(funName' "lerp64")(double v0, double v1, double t) {
      return v0 + (v1-v0)*t;
    }
$esc:("#endif")
|]
