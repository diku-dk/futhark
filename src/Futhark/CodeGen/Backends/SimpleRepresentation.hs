{-# LANGUAGE QuasiQuotes #-}
-- | Simple C runtime representation.
module Futhark.CodeGen.Backends.SimpleRepresentation
  ( tupleField
  , funName
  , defaultMemBlockType
  , primTypeToCType
  , signedPrimTypeToCType

    -- * Primitive value operations
  , cIntOps
  , cFloat32Ops, cFloat32Funs
  , cFloat64Ops, cFloat64Funs
  , cFloatConvOps
  , glIntOps
  , glFloat32Ops, glFloat32Funs
  , glFloat64Ops, glFloat64Funs
  , glFloatConvOps
  )
  where

import qualified Language.C.Syntax as C
import qualified Language.C.Quote.C as C

import Futhark.CodeGen.ImpCode
import Futhark.Util.Pretty (prettyOneLine)
import Futhark.Util (zEncodeString)

-- | The C type corresponding to a signed integer type.
intTypeToCType :: IntType -> C.Type
intTypeToCType Int8  = [C.cty|typename int8_t|]
intTypeToCType Int16 = [C.cty|typename int16_t|]
intTypeToCType Int32 = [C.cty|typename int32_t|]
intTypeToCType Int64 = [C.cty|typename int64_t|]

-- | The C type corresponding to an unsigned integer type.
uintTypeToCType :: IntType -> C.Type
uintTypeToCType Int8  = [C.cty|typename uint8_t|]
uintTypeToCType Int16 = [C.cty|typename uint16_t|]
uintTypeToCType Int32 = [C.cty|typename uint32_t|]
uintTypeToCType Int64 = [C.cty|typename uint64_t|]

-- | The C type corresponding to a float type.
floatTypeToCType :: FloatType -> C.Type
floatTypeToCType Float32 = [C.cty|float|]
floatTypeToCType Float64 = [C.cty|double|]

-- | The C type corresponding to a primitive type. Integers are
-- assumed to be unsigned.
primTypeToCType :: PrimType -> C.Type
primTypeToCType (IntType t)   = intTypeToCType t
primTypeToCType (FloatType t) = floatTypeToCType t
primTypeToCType Bool = [C.cty|typename bool|]
primTypeToCType Cert = [C.cty|typename bool|]

-- | The C type corresponding to a primitive type. Integers are
-- assumed to have the specified sign.
signedPrimTypeToCType :: Signedness -> PrimType -> C.Type
signedPrimTypeToCType TypeUnsigned (IntType t) = uintTypeToCType t
signedPrimTypeToCType TypeDirect (IntType t)   = intTypeToCType t
signedPrimTypeToCType _ t = primTypeToCType t

-- | @tupleField i@ is the name of field number @i@ in a tuple.
tupleField :: Int -> String
tupleField i = "v" ++ show i

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
          ++ cIntPrimFuns
  where ops = [mkAdd,   mkSub, mkMul,
               mkUDiv,  mkUMod,
               mkSDiv,  mkSMod,
               mkSQuot, mkSRem,
               mkSMin,  mkUMin,
               mkSMax,  mkUMax,
               mkShl,   mkLShr, mkAShr,
               mkAnd,   mkOr,   mkXor,
               mkUlt,   mkUle,  mkSlt, mkSle,
               mkPow,
               mkIToB, mkBToI
              ] ++
              map mkSExt [minBound..maxBound] ++
              map mkZExt [minBound..maxBound]

        taggedI s Int8  = s ++ "8"
        taggedI s Int16 = s ++ "16"
        taggedI s Int32 = s ++ "32"
        taggedI s Int64 = s ++ "64"

        -- Use unsigned types for add/sub/mul so we can do
        -- well-defined overflow.
        mkAdd  = simpleUintOp "add"  [C.cexp|x + y|]
        mkSub  = simpleUintOp "sub"  [C.cexp|x - y|]
        mkMul  = simpleUintOp "mul"  [C.cexp|x * y|]
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

        mkSQuot = simpleIntOp  "squot" [C.cexp|x / y|]
        mkSRem  = simpleIntOp  "srem"  [C.cexp|x % y|]
        mkSMax  = simpleIntOp  "smax"  [C.cexp|x < y ? y : x|]
        mkSMin  = simpleIntOp  "smin"  [C.cexp|x < y ? x : y|]
        mkShl   = simpleUintOp "shl"   [C.cexp|x << y|]
        mkLShr  = simpleUintOp "lshr"  [C.cexp|x >> y|]
        mkAShr  = simpleIntOp  "ashr"  [C.cexp|x >> y|]
        mkAnd   = simpleUintOp "and"   [C.cexp|x & y|]
        mkOr    = simpleUintOp "or"    [C.cexp|x | y|]
        mkXor   = simpleUintOp "xor"   [C.cexp|x ^ y|]
        mkUlt   = uintCmpOp    "ult"   [C.cexp|x < y|]
        mkUle   = uintCmpOp    "ule"   [C.cexp|x <= y|]
        mkSlt   = intCmpOp     "slt"   [C.cexp|x < y|]
        mkSle   = intCmpOp     "sle"   [C.cexp|x <= y|]

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
          where name    = "sext_"++pretty from_t++"_"++pretty to_t
                from_ct = intTypeToCType from_t
                to_ct   = intTypeToCType to_t

        mkZExt from_t to_t = macro name [C.cexp|($ty:to_ct)(($ty:from_ct)x)|]
          where name    = "zext_"++pretty from_t++"_"++pretty to_t
                from_ct = uintTypeToCType from_t
                to_ct   = uintTypeToCType to_t

        mkBToI to_t =
          [C.cedecl|static inline $ty:to_ct
                    $id:name($ty:from_ct x) { return x; } |]
          where name    = "btoi_bool_"++pretty to_t
                from_ct = primTypeToCType Bool
                to_ct   = intTypeToCType to_t

        mkIToB from_t =
          [C.cedecl|static inline $ty:to_ct
                    $id:name($ty:from_ct x) { return x; } |]
          where name    = "itob_"++pretty from_t++"_bool"
                to_ct   = primTypeToCType Bool
                from_ct = intTypeToCType from_t

        simpleUintOp s e t =
          [C.cedecl|static inline $ty:ct $id:(taggedI s t)($ty:ct x, $ty:ct y) { return $exp:e; }|]
            where ct = uintTypeToCType t
        simpleIntOp s e t =
          [C.cedecl|static inline $ty:ct $id:(taggedI s t)($ty:ct x, $ty:ct y) { return $exp:e; }|]
            where ct = intTypeToCType t
        intCmpOp s e t =
          [C.cedecl|static inline typename bool $id:(taggedI s t)($ty:ct x, $ty:ct y) { return $exp:e; }|]
            where ct = intTypeToCType t
        uintCmpOp s e t =
          [C.cedecl|static inline typename bool $id:(taggedI s t)($ty:ct x, $ty:ct y) { return $exp:e; }|]
            where ct = uintTypeToCType t

cIntPrimFuns :: [C.Definition]
cIntPrimFuns =
  [C.cunit|
$esc:("#if defined(__OPENCL_VERSION__)")
   static typename int32_t $id:(funName' "popc8") (typename int8_t x) {
      return popcount(x);
   }
   static typename int32_t $id:(funName' "popc16") (typename int16_t x) {
      return popcount(x);
   }
   static typename int32_t $id:(funName' "popc32") (typename int32_t x) {
      return popcount(x);
   }
   static typename int32_t $id:(funName' "popc64") (typename int64_t x) {
      return popcount(x);
   }
$esc:("#elif defined(__CUDA_ARCH__)")
   static typename int32_t $id:(funName' "popc8") (typename int8_t x) {
      return __popc(zext_i8_i32(x));
   }
   static typename int32_t $id:(funName' "popc16") (typename int16_t x) {
      return __popc(zext_i16_i32(x));
   }
   static typename int32_t $id:(funName' "popc32") (typename int32_t x) {
      return __popc(x);
   }
   static typename int32_t $id:(funName' "popc64") (typename int64_t x) {
      return __popcll(x);
   }
$esc:("#else")
   static typename int32_t $id:(funName' "popc8") (typename int8_t x) {
     int c = 0;
     for (; x; ++c) {
       x &= x - 1;
     }
     return c;
    }
   static typename int32_t $id:(funName' "popc16") (typename int16_t x) {
     int c = 0;
     for (; x; ++c) {
       x &= x - 1;
     }
     return c;
   }
   static typename int32_t $id:(funName' "popc32") (typename int32_t x) {
     int c = 0;
     for (; x; ++c) {
       x &= x - 1;
     }
     return c;
   }
   static typename int32_t $id:(funName' "popc64") (typename int64_t x) {
     int c = 0;
     for (; x; ++c) {
       x &= x - 1;
     }
     return c;
   }
$esc:("#endif")

$esc:("#if defined(__OPENCL_VERSION__)")
   static typename uint8_t $id:(funName' "mul_hi8") (typename uint8_t a, typename uint8_t b) {
      return mul_hi(a, b);
   }
   static typename uint16_t $id:(funName' "mul_hi16") (typename uint16_t a, typename uint16_t b) {
      return mul_hi(a, b);
   }
   static typename uint32_t $id:(funName' "mul_hi32") (typename uint32_t a, typename uint32_t b) {
      return mul_hi(a, b);
   }
   static typename uint64_t $id:(funName' "mul_hi64") (typename uint64_t a, typename uint64_t b) {
      return mul_hi(a, b);
   }
$esc:("#elif defined(__CUDA_ARCH__)")
   static typename uint8_t $id:(funName' "mul_hi8") (typename uint8_t a, typename uint8_t b) {
     typename uint16_t aa = a;
     typename uint16_t bb = b;
     return (aa * bb) >> 8;
   }
   static typename uint16_t $id:(funName' "mul_hi16") (typename uint16_t a, typename uint16_t b) {
     typename uint32_t aa = a;
     typename uint32_t bb = b;
     return (aa * bb) >> 16;
   }
   static typename uint32_t $id:(funName' "mul_hi32") (typename uint32_t a, typename uint32_t b) {
      return mulhi(a, b);
   }
   static typename uint64_t $id:(funName' "mul_hi64") (typename uint64_t a, typename uint64_t b) {
      return mul64hi(a, b);
   }
$esc:("#else")
   static typename uint8_t $id:(funName' "mul_hi8") (typename uint8_t a, typename uint8_t b) {
     typename uint16_t aa = a;
     typename uint16_t bb = b;
     return (aa * bb) >> 8;
    }
   static typename uint16_t $id:(funName' "mul_hi16") (typename uint16_t a, typename uint16_t b) {
     typename uint32_t aa = a;
     typename uint32_t bb = b;
     return (aa * bb) >> 16;
    }
   static typename uint32_t $id:(funName' "mul_hi32") (typename uint32_t a, typename uint32_t b) {
     typename uint64_t aa = a;
     typename uint64_t bb = b;
     return (aa * bb) >> 32;
    }
   static typename uint64_t $id:(funName' "mul_hi64") (typename uint64_t a, typename uint64_t b) {
     typename __uint128_t aa = a;
     typename __uint128_t bb = b;
     return (aa * bb) >> 64;
    }
$esc:("#endif")

$esc:("#if defined(__OPENCL_VERSION__)")
   static typename uint8_t $id:(funName' "mad_hi8") (typename uint8_t a, typename uint8_t b, typename uint8_t c) {
      return mad_hi(a, b, c);
   }
   static typename uint16_t $id:(funName' "mad_hi16") (typename uint16_t a, typename uint16_t b, typename uint16_t c) {
      return mad_hi(a, b, c);
   }
   static typename uint32_t $id:(funName' "mad_hi32") (typename uint32_t a, typename uint32_t b, typename uint32_t c) {
      return mad_hi(a, b, c);
   }
   static typename uint64_t $id:(funName' "mad_hi64") (typename uint64_t a, typename uint64_t b, typename uint64_t c) {
      return mad_hi(a, b, c);
   }
$esc:("#else")
   static typename uint8_t $id:(funName' "mad_hi8") (typename uint8_t a, typename uint8_t b, typename uint8_t c) {
     return futrts_mul_hi8(a, b) + c;
    }
   static typename uint16_t $id:(funName' "mad_hi16") (typename uint16_t a, typename uint16_t b, typename uint16_t c) {
     return futrts_mul_hi16(a, b) + c;
    }
   static typename uint32_t $id:(funName' "mad_hi32") (typename uint32_t a, typename uint32_t b, typename uint32_t c) {
     return futrts_mul_hi32(a, b) + c;
    }
   static typename uint64_t $id:(funName' "mad_hi64") (typename uint64_t a, typename uint64_t b, typename uint64_t c) {
     return futrts_mul_hi64(a, b) + c;
    }
$esc:("#endif")


$esc:("#if defined(__OPENCL_VERSION__)")
   static typename int32_t $id:(funName' "clz8") (typename int8_t x) {
      return clz(x);
   }
   static typename int32_t $id:(funName' "clz16") (typename int16_t x) {
      return clz(x);
   }
   static typename int32_t $id:(funName' "clz32") (typename int32_t x) {
      return clz(x);
   }
   static typename int32_t $id:(funName' "clz64") (typename int64_t x) {
      return clz(x);
   }
$esc:("#elif defined(__CUDA_ARCH__)")
   static typename int32_t $id:(funName' "clz8") (typename int8_t x) {
      return __clz(zext_i8_i32(x))-24;
   }
   static typename int32_t $id:(funName' "clz16") (typename int16_t x) {
      return __clz(zext_i16_i32(x))-16;
   }
   static typename int32_t $id:(funName' "clz32") (typename int32_t x) {
      return __clz(x);
   }
   static typename int32_t $id:(funName' "clz64") (typename int64_t x) {
      return __clzll(x);
   }
$esc:("#else")
   static typename int32_t $id:(funName' "clz8") (typename int8_t x) {
    int n = 0;
    int bits = sizeof(x) * 8;
    for (int i = 0; i < bits; i++) {
        if (x < 0) break;
        n++;
        x <<= 1;
    }
    return n;
   }
   static typename int32_t $id:(funName' "clz16") (typename int16_t x) {
    int n = 0;
    int bits = sizeof(x) * 8;
    for (int i = 0; i < bits; i++) {
        if (x < 0) break;
        n++;
        x <<= 1;
    }
    return n;
   }
   static typename int32_t $id:(funName' "clz32") (typename int32_t x) {
    int n = 0;
    int bits = sizeof(x) * 8;
    for (int i = 0; i < bits; i++) {
        if (x < 0) break;
        n++;
        x <<= 1;
    }
    return n;
   }
   static typename int32_t $id:(funName' "clz64") (typename int64_t x) {
    int n = 0;
    int bits = sizeof(x) * 8;
    for (int i = 0; i < bits; i++) {
        if (x < 0) break;
        n++;
        x <<= 1;
    }
    return n;
   }
$esc:("#endif")
|]

-- | Same as `cIntOps` but to adapt for GLSL.
glIntOps :: [C.Definition]
glIntOps = concatMap (`map` [minBound..maxBound]) ops
          ++ glIntPrimFuns
  where ops = [mkPow,
               mkAdd,   mkSub,  mkMul,
               mkUAdd,  mkUSub, mkUMul,
               mkUDiv,  mkUMod,
               mkSDiv,  mkSMod,
               mkSQuot, mkSRem,
               mkSMin,  mkUMin,
               mkSMax,  mkUMax,
               mkShl,   mkLShr, mkAShr,
               mkAnd,   mkOr,   mkXor,
               mkUlt,   mkUle,  mkSlt, mkSle,
               mkIToB, mkBToI
              ] ++
              map mkSExt [minBound..maxBound] ++
              map mkZExt [minBound..maxBound]

        taggedI s Int8  = s ++ "8"
        taggedI s Int16 = s ++ "16"
        taggedI s Int32 = s ++ "32"
        taggedI s Int64 = s ++ "64"

        -- Use unsigned types for add/sub/mul so we can do
        -- well-defined overflow.
        mkUAdd = simpleUintOp "add"  [C.cexp|x + y|]
        mkUSub = simpleUintOp "sub"  [C.cexp|x - y|]
        mkUMul = simpleUintOp "mul"  [C.cexp|x * y|]
        mkAdd  = simpleIntOp  "add"  [C.cexp|x + y|]
        mkSub  = simpleIntOp  "sub"  [C.cexp|x - y|]
        mkMul  = simpleIntOp  "mul"  [C.cexp|x * y|]
        mkUDiv = simpleUintOp "udiv" [C.cexp|x / y|]
        mkUMod = simpleUintOp "umod" [C.cexp|x % y|]
        mkUMax = simpleUintOp "umax" [C.cexp|x < y ? y : x|]
        mkUMin = simpleUintOp "umin" [C.cexp|x < y ? x : y|]

        mkSDiv t =
          let ct = intTypeToCType t
          in [C.cedecl|$ty:ct $id:(taggedI "sdiv" t)($ty:ct x, $ty:ct y) {
                         $ty:ct q = x / y;
                         $ty:ct r = x % y;
                         return q -
                           (((r != 0) && ((r < 0) != (y < 0))) ? 1 : 0);
             }|]
        mkSMod t =
          let ct = intTypeToCType t
          in [C.cedecl|$ty:ct $id:(taggedI "smod" t)($ty:ct x, $ty:ct y) {
                         $ty:ct r = x % y;
                         return r +
                           ((r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0)) ? 0 : y);
              }|]

        mkShl t =
          let ct = intTypeToCType t
          in if t == Int64 then
            [C.cedecl|$ty:ct $id:(taggedI "shl" t)($ty:ct x, $ty:ct y) {
                       return x * pow64(int64_t(2),y);
            }|]
          else
            [C.cedecl|$ty:ct $id:(taggedI "shl" t)($ty:ct x, $ty:ct y) {
                       return x << y;
            }|]

        mkLShr t =
          let ct = intTypeToCType t
          in if t == Int64 then
            [C.cedecl|$ty:ct $id:(taggedI "lshr" t)($ty:ct x, $ty:ct y) {
                       return x / pow64(int64_t(2),y);
            }|]
          else
            [C.cedecl|$ty:ct $id:(taggedI "lshr" t)($ty:ct x, $ty:ct y) {
                       return x << y;
            }|]

        mkAShr t =
          let ct = intTypeToCType t
          in if t == Int64 then
            [C.cedecl|$ty:ct $id:(taggedI "ashr" t)($ty:ct x, $ty:ct y) {
                       return x / pow64(int64_t(2),y);
            }|]
          else
            [C.cedecl|$ty:ct $id:(taggedI "ashr" t)($ty:ct x, $ty:ct y) {
                       return x << y;
            }|]

        mkSQuot = simpleIntOp  "squot" [C.cexp|x / y|]
        mkSRem  = simpleIntOp  "srem"  [C.cexp|x % y|]
        mkSMax  = simpleIntOp  "smax"  [C.cexp|x < y ? y : x|]
        mkSMin  = simpleIntOp  "smin"  [C.cexp|x < y ? x : y|]
        mkAnd   = simpleUintOp "and"   [C.cexp|x & y|]
        mkOr    = simpleUintOp "or"    [C.cexp|x | y|]
        mkXor   = simpleUintOp "xor"   [C.cexp|x ^ y|]
        mkUlt   = uintCmpOp    "ult"   [C.cexp|x < y|]
        mkUle   = uintCmpOp    "ule"   [C.cexp|x <= y|]
        mkSlt   = intCmpOp     "slt"   [C.cexp|x < y|]
        mkSle   = intCmpOp     "sle"   [C.cexp|x <= y|]

        -- We define some operations as macros rather than functions,
        -- because this allows us to use them as constant expressions
        -- in things like array sizes and static initialisers.
        macro name rhs =
          [C.cedecl|$esc:("#define " ++ name ++ "(x) (" ++ prettyOneLine rhs ++ ")")|]

        mkPow t =
          let ct = intTypeToCType t
          in if t == Int64 then
            [C.cedecl|$ty:ct $id:(taggedI "pow" t)($ty:ct x, $ty:ct y) {
                           $ty:ct res = 1, rem = y;
                           while (rem != 0) {
                             if ((rem & int64_t(1)) != 0) {
                               res *= x;
                             }
                             rem >>= 1;
                             x *= x;
                           }
                           return res;
                }|]
          else
            [C.cedecl|$ty:ct $id:(taggedI "pow" t)($ty:ct x, $ty:ct y) {
                           $ty:ct res = 1, rem = y;
                           while (rem != 0) {
                             if ((rem & 1) != 0) {
                               res *= x;
                             }
                             rem >>= 1;
                             x *= x;
                           }
                           return res;
                }|]

        mkSExt from_t to_t = macro name [C.cexp|($ty:to_ct)(($ty:from_ct)x)|]
          where name    = "sext_"++pretty from_t++"_"++pretty to_t
                from_ct = intTypeToCType from_t
                to_ct   = intTypeToCType to_t

        mkZExt from_t to_t = macro name [C.cexp|($ty:to_ct)(($ty:from_ct)x)|]
          where name    = "zext_"++pretty from_t++"_"++pretty to_t
                from_ct = uintTypeToCType from_t
                to_ct   = uintTypeToCType to_t

        mkBToI to_t =
          if to_t == Int64 then
            [C.cedecl|$ty:to_ct
                      $id:name($ty:from_ct x) { return int64_t(x); } |]
          else
            [C.cedecl|$ty:to_ct
                      $id:name($ty:from_ct x) { return int32_t(x); } |]
          where name    = "btoi_bool_"++pretty to_t
                from_ct = primTypeToCType Bool
                to_ct   = intTypeToCType to_t

        mkIToB from_t =
          [C.cedecl|$ty:to_ct
                    $id:name($ty:from_ct x) { return bool(x); } |]
          where name    = "itob_"++pretty from_t++"_bool"
                to_ct   = primTypeToCType Bool
                from_ct = intTypeToCType from_t

        simpleUintOp s e t =
          [C.cedecl|$ty:ct $id:(taggedI s t)($ty:ct x, $ty:ct y) { return $exp:e; }|]
            where ct = uintTypeToCType t
        simpleIntOp s e t =
          [C.cedecl|$ty:ct $id:(taggedI s t)($ty:ct x, $ty:ct y) { return $exp:e; }|]
            where ct = intTypeToCType t
        intCmpOp s e t =
          [C.cedecl|typename bool $id:(taggedI s t)($ty:ct x, $ty:ct y) { return $exp:e; }|]
            where ct = intTypeToCType t
        uintCmpOp s e t =
          [C.cedecl|typename bool $id:(taggedI s t)($ty:ct x, $ty:ct y) { return $exp:e; }|]
            where ct = uintTypeToCType t

-- | Same as `cIntPrimFuns` but to adapt for GLSL.
glIntPrimFuns :: [C.Definition]
glIntPrimFuns = [C.cunit|
   typename int32_t $id:(funName' "popc8") (typename int8_t x) {
      return bitCount(x);
   }
   typename int32_t $id:(funName' "popc16") (typename int16_t x) {
      return bitCount(x);
   }
   typename int32_t $id:(funName' "popc32") (typename int32_t x) {
      return bitCount(x);
   }
   typename int32_t $id:(funName' "popc64") (typename int64_t x) {
     int c = 0;
     for (; x > 0; ++c) {
       x &= x - 1;
     }
     return c;
   }

   typename uint8_t $id:(funName' "mul_hi8") (typename uint8_t a, typename uint8_t b) {
     typename uint16_t aa = a;
     typename uint16_t bb = b;
     return (aa * bb) >> 32;
    }
   typename uint16_t $id:(funName' "mul_hi16") (typename uint16_t a, typename uint16_t b) {
     typename uint32_t aa = a;
     typename uint32_t bb = b;
     return (aa * bb) >> 32;
    }
   typename uint32_t $id:(funName' "mul_hi32") (typename uint32_t a, typename uint32_t b) {
     typename uint64_t aa = a;
     typename uint64_t bb = b;
     return uint32_t(aa * bb) >> 32;
    }
   typename uint64_t $id:(funName' "mul_hi64") (typename uint64_t xx, typename uint64_t yy) {
     typename uint64_t x = xx;
     typename uint64_t y = yy;
     typename uint64_t accum = uint64_t(uint32_t(x)) * uint64_t(uint32_t(y));
     accum /= uint64_t(pow64(int64_t(2), int64_t(32)));
     typename uint64_t term1 = (x / uint64_t(pow64(int64_t(2), int64_t(32)))) *
                      uint64_t(uint32_t(y));
     typename uint64_t term2 = (y / uint64_t(pow64(int64_t(2), int64_t(32)))) *
                      uint64_t(uint32_t(x));
     accum += uint32_t(term1);
     accum += uint32_t(term2);
     accum /= uint64_t(pow64(int64_t(2), int64_t(32)));
     accum += (term1 / uint64_t(pow64(int64_t(2), int64_t(32)))) +
              (term2 / uint64_t(pow64(int64_t(2), int64_t(32))));
     return accum;
   }

   typename uint8_t $id:(funName' "mad_hi8") (typename uint8_t a, typename uint8_t b, typename uint8_t c) {
     return futrts_mul_hi8(a, b) + c;
    }
   typename uint16_t $id:(funName' "mad_hi16") (typename uint16_t a, typename uint16_t b, typename uint16_t c) {
     return futrts_mul_hi16(a, b) + c;
    }
   typename uint32_t $id:(funName' "mad_hi32") (typename uint32_t a, typename uint32_t b, typename uint32_t c) {
     return futrts_mul_hi32(a, b) + c;
    }
   typename uint64_t $id:(funName' "mad_hi64") (typename uint64_t a, typename uint64_t b, typename uint64_t c) {
     return futrts_mul_hi64(a, b) + c;
    }

   typename int32_t $id:(funName' "clz8") (typename int8_t x) {
    int n = 0;
    int bits = 32;
    for (int i = 0; i < bits; i++) {
        if (x < 0) break;
        n++;
        x <<= 1;
    }
    return n;
   }
   typename int32_t $id:(funName' "clz16") (typename int16_t x) {
    int n = 0;
    int bits = 32;
    for (int i = 0; i < bits; i++) {
        if (x < 0) break;
        n++;
        x <<= 1;
    }
    return n;
   }
   typename int32_t $id:(funName' "clz32") (typename int32_t x) {
    int n = 0;
    int bits = 32;
    for (int i = 0; i < bits; i++) {
        if (x < 0) break;
        n++;
        x <<= 1;
    }
    return n;
   }
   typename int32_t $id:(funName' "clz64") (typename int64_t x) {
    int n = 0;
    int bits = 64;
    for (int i = 0; i < bits; i++) {
        if (x < 0) break;
        n++;
        x *= 2;
    }
    return n;
   }
|]

cFloat32Ops   :: [C.Definition]
cFloat64Ops   :: [C.Definition]
cFloatConvOps :: [C.Definition]
(cFloat32Ops, cFloat64Ops, cFloatConvOps) =
  ( map ($Float32) mkOps
  , map ($Float64) mkOps
  , [ mkFPConvFF "fpconv" from to |
      from <- [minBound..maxBound],
      to   <- [minBound..maxBound] ]
  )
  where taggedF s Float32 = s ++ "32"
        taggedF s Float64 = s ++ "64"
        convOp  s from to = s ++ "_" ++ pretty from ++ "_" ++ pretty to

        mkOps = [mkFDiv, mkFAdd, mkFSub, mkFMul, mkFMin, mkFMax, mkPow, mkCmpLt, mkCmpLe] ++
                map (mkFPConvIF "sitofp") [minBound..maxBound] ++
                map (mkFPConvUF "uitofp") [minBound..maxBound] ++
                map (flip $ mkFPConvFI "fptosi") [minBound..maxBound] ++
                map (flip $ mkFPConvFU "fptoui") [minBound..maxBound]

        mkFDiv  = simpleFloatOp "fdiv"  [C.cexp|x / y|]
        mkFAdd  = simpleFloatOp "fadd"  [C.cexp|x + y|]
        mkFSub  = simpleFloatOp "fsub"  [C.cexp|x - y|]
        mkFMul  = simpleFloatOp "fmul"  [C.cexp|x * y|]
        mkFMin  = simpleFloatOp "fmin"  [C.cexp|fmin(x, y)|]
        mkFMax  = simpleFloatOp "fmax"  [C.cexp|fmax(x, y)|]
        mkCmpLt = floatCmpOp    "cmplt" [C.cexp|x < y|]
        mkCmpLe = floatCmpOp    "cmple" [C.cexp|x <= y|]

        mkPow Float32 =
          [C.cedecl|static inline float fpow32(float x, float y) { return pow(x, y); }|]
        mkPow Float64 =
          [C.cedecl|static inline double fpow64(double x, double y) { return pow(x, y); }|]

        mkFPConv from_f to_f s from_t to_t =
          [C.cedecl|static inline $ty:to_ct
                    $id:(convOp s from_t to_t)($ty:from_ct x) { return ($ty:to_ct)x;} |]
          where from_ct = from_f from_t
                to_ct = to_f to_t

        mkFPConvFF = mkFPConv floatTypeToCType floatTypeToCType
        mkFPConvFI = mkFPConv floatTypeToCType intTypeToCType
        mkFPConvIF = mkFPConv intTypeToCType   floatTypeToCType
        mkFPConvFU = mkFPConv floatTypeToCType uintTypeToCType
        mkFPConvUF = mkFPConv uintTypeToCType  floatTypeToCType

        simpleFloatOp s e t =
          [C.cedecl|static inline $ty:ct $id:(taggedF s t)($ty:ct x, $ty:ct y) { return $exp:e; }|]
            where ct = floatTypeToCType t
        floatCmpOp s e t =
          [C.cedecl|static inline typename bool $id:(taggedF s t)($ty:ct x, $ty:ct y) { return $exp:e; }|]
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

    static inline float $id:(funName' "cosh32")(float x) {
      return cosh(x);
    }

    static inline float $id:(funName' "sinh32")(float x) {
      return sinh(x);
    }

    static inline float $id:(funName' "tanh32")(float x) {
      return tanh(x);
    }

    static inline float $id:(funName' "acosh32")(float x) {
      return acosh(x);
    }

    static inline float $id:(funName' "asinh32")(float x) {
      return asinh(x);
    }

    static inline float $id:(funName' "atanh32")(float x) {
      return atanh(x);
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

    static inline typename bool $id:(funName' "isnan32")(float x) {
      return isnan(x);
    }

    static inline typename bool $id:(funName' "isinf32")(float x) {
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
    static inline float $id:(funName' "mad32")(float a, float b, float c) {
      return mad(a,b,c);
    }
    static inline float $id:(funName' "fma32")(float a, float b, float c) {
      return fma(a,b,c);
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
    static inline float $id:(funName' "mad32")(float a, float b, float c) {
      return a*b+c;
    }
    static inline float $id:(funName' "fma32")(float a, float b, float c) {
      return fmaf(a,b,c);
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

    static inline double $id:(funName' "cosh64")(double x) {
      return cosh(x);
    }

    static inline double $id:(funName' "sinh64")(double x) {
      return sinh(x);
    }

    static inline double $id:(funName' "tanh64")(double x) {
      return tanh(x);
    }

    static inline double $id:(funName' "acosh64")(double x) {
      return acosh(x);
    }

    static inline double $id:(funName' "asinh64")(double x) {
      return asinh(x);
    }

    static inline double $id:(funName' "atanh64")(double x) {
      return atanh(x);
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

    static inline double $id:(funName' "fma64")(double a, double b, double c) {
      return fma(a,b,c);
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

    static inline typename bool $id:(funName' "isnan64")(double x) {
      return isnan(x);
    }

    static inline typename bool $id:(funName' "isinf64")(double x) {
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

    static inline double fmod64(double x, double y) {
      return fmod(x, y);
    }

$esc:("#ifdef __OPENCL_VERSION__")
    static inline double $id:(funName' "lerp64")(double v0, double v1, double t) {
      return mix(v0, v1, t);
    }
    static inline double $id:(funName' "mad64")(double a, double b, double c) {
      return mad(a,b,c);
    }
$esc:("#else")
    static inline double $id:(funName' "lerp64")(double v0, double v1, double t) {
      return v0 + (v1-v0)*t;
    }
    static inline double $id:(funName' "mad64")(double a, double b, double c) {
      return a*b+c;
    }
$esc:("#endif")
|]

glFloat32Ops   :: [C.Definition]
glFloat64Ops   :: [C.Definition]
glFloatConvOps :: [C.Definition]
(glFloat32Ops, glFloat64Ops, glFloatConvOps) =
  ( map ($Float32) mkOps
  , map ($Float64) mkOps
  , [ mkFPConvFF "fpconv" from to |
      from <- [minBound..maxBound],
      to   <- [minBound..maxBound] ]
  )
  where taggedF s Float32 = s
        taggedF s Float64 = s ++ "64"
        convOp  s from to = s ++ pretty from ++ pretty to

        mkOps = [mkFDiv, mkFAdd, mkFSub, mkFMul, mkFMin, mkFMax, mkPow, mkCmpLt, mkCmpLe] ++
                map (mkFPConvIF "sitofp") [minBound..maxBound] ++
                map (mkFPConvUF "uitofp") [minBound..maxBound] ++
                map (flip $ mkFPConvFI "fptosi") [minBound..maxBound] ++
                map (flip $ mkFPConvFU "fptoui") [minBound..maxBound]

        mkFDiv  = simpleFloatOp "fdiv"  [C.cexp|x / y|]
        mkFAdd  = simpleFloatOp "fadd"  [C.cexp|x + y|]
        mkFSub  = simpleFloatOp "fsub"  [C.cexp|x - y|]
        mkFMul  = simpleFloatOp "fmul"  [C.cexp|x * y|]
        mkCmpLt = floatCmpOp    "cmplt" [C.cexp|x < y|]
        mkCmpLe = floatCmpOp    "cmple" [C.cexp|x <= y|]

        mkFMin t =
          let ct = floatTypeToCType t
          in if t == Float64 then
            [C.cedecl|$ty:ct $id:(taggedF "fmin" t)($ty:ct x, $ty:ct y) {
                           if (x < y) {
                             return x;
                           }
                           return y;
               }|]
          else
            [C.cedecl|$ty:ct $id:(taggedF "fmin" t)($ty:ct x, $ty:ct y) { return min(x, y); }|]

        mkFMax t =
          let ct = floatTypeToCType t
          in if t == Float64 then
            [C.cedecl|$ty:ct $id:(taggedF "fmax" t)($ty:ct x, $ty:ct y) {
                           if (x > y) {
                             return x;
                           }
                           return y;
               }|]
          else
            [C.cedecl|$ty:ct $id:(taggedF "fmax" t)($ty:ct x, $ty:ct y) { return max(x, y); }|]

        mkPow Float32 =
          [C.cedecl|float fpow32(float x, float y) { return pow(x, y); }|]
        mkPow Float64 =
          [C.cedecl|double fpow64(double x, double y) {
                      double res = 1.0, rem = y;
                      while (rem != 0.0) {
                          if (float64((int64_t(rem) & int64_t(1.0))) != 0.0)
                              res *= x;
                          rem = float64(int64_t(rem) >> 1);
                          x *= x;
                      }
                      return res;
              }|]

        mkFPConv from_f to_f s from_t to_t =
          -- from float to int
          if from_ct == [C.cty|float|] && to_ct == [C.cty|typename int8_t|] then
            [C.cedecl|$ty:to_ct
                      $id:(convOp s from_t to_t)($ty:from_ct x) { return int8_t(x);} |]
          else if from_ct == [C.cty|float|] && to_ct == [C.cty|typename int16_t|] then
            [C.cedecl|$ty:to_ct
                      $id:(convOp s from_t to_t)($ty:from_ct x) { return int16_t(x);} |]
          else if from_ct == [C.cty|float|] && to_ct == [C.cty|typename int64_t|] then
            [C.cedecl|$ty:to_ct
                      $id:(convOp s from_t to_t)($ty:from_ct x) { return int64_t(x);} |]
          else if from_ct == [C.cty|float|] && to_ct == [C.cty|typename int32_t|] then
            [C.cedecl|$ty:to_ct
                      $id:(convOp s from_t to_t)($ty:from_ct x) { return int32_t(x);} |]
          else if from_ct == [C.cty|float|] && to_ct == [C.cty|typename int64_t|] then
            [C.cedecl|$ty:to_ct
                      $id:(convOp s from_t to_t)($ty:from_ct x) { return int64_t(x);} |]
          -- from float to uint
          else if from_ct == [C.cty|float|] && to_ct == [C.cty|typename uint8_t|] then
            [C.cedecl|$ty:to_ct
                      $id:(convOp s from_t to_t)($ty:from_ct x) { return uint8_t(x);} |]
          else if from_ct == [C.cty|float|] && to_ct == [C.cty|typename uint16_t|] then
            [C.cedecl|$ty:to_ct
                      $id:(convOp s from_t to_t)($ty:from_ct x) { return uint16_t(x);} |]
          else if from_ct == [C.cty|float|] && to_ct == [C.cty|typename uint32_t|] then
            [C.cedecl|$ty:to_ct
                      $id:(convOp s from_t to_t)($ty:from_ct x) { return uint32_t(x);} |]
          else if from_ct == [C.cty|float|] && to_ct == [C.cty|typename uint64_t|] then
            [C.cedecl|$ty:to_ct
                      $id:(convOp s from_t to_t)($ty:from_ct x) { return uint64_t(x);} |]
          -- from 64-bit ints to float
          else if from_ct == [C.cty|typename int64_t|] && to_ct == [C.cty|float|] then
            [C.cedecl|$ty:to_ct
                      $id:(convOp s from_t to_t)($ty:from_ct x) { return float32(x);} |]
          else if from_ct == [C.cty|typename uint64_t|] && to_ct == [C.cty|float|] then
            [C.cedecl|$ty:to_ct
                      $id:(convOp s from_t to_t)($ty:from_ct x) { return float32(x);} |]
          -- from double to int
          else if from_ct == [C.cty|double|] && to_ct == [C.cty|typename int8_t|] then
            [C.cedecl|$ty:to_ct
                      $id:(convOp s from_t to_t)($ty:from_ct x) { return int8_t(x);} |]
          else if from_ct == [C.cty|double|] && to_ct == [C.cty|typename int16_t|] then
            [C.cedecl|$ty:to_ct
                      $id:(convOp s from_t to_t)($ty:from_ct x) { return int16_t(x);} |]
          else if from_ct == [C.cty|double|] && to_ct == [C.cty|typename int32_t|] then
            [C.cedecl|$ty:to_ct
                      $id:(convOp s from_t to_t)($ty:from_ct x) { return int32_t(x);} |]
          else if from_ct == [C.cty|double|] && to_ct == [C.cty|typename int64_t|] then
            [C.cedecl|$ty:to_ct
                      $id:(convOp s from_t to_t)($ty:from_ct x) { return int64_t(x);} |]
          -- from double to uint
          else if from_ct == [C.cty|double|] && to_ct == [C.cty|typename uint8_t|] then
            [C.cedecl|$ty:to_ct
                      $id:(convOp s from_t to_t)($ty:from_ct x) { return uint8_t(x);} |]
          else if from_ct == [C.cty|double|] && to_ct == [C.cty|typename uint16_t|] then
            [C.cedecl|$ty:to_ct
                      $id:(convOp s from_t to_t)($ty:from_ct x) { return uint16_t(x);} |]
          else if from_ct == [C.cty|double|] && to_ct == [C.cty|typename uint32_t|] then
            [C.cedecl|$ty:to_ct
                      $id:(convOp s from_t to_t)($ty:from_ct x) { return uint32_t(x);} |]
          else if from_ct == [C.cty|double|] && to_ct == [C.cty|typename uint64_t|] then
            [C.cedecl|$ty:to_ct
                      $id:(convOp s from_t to_t)($ty:from_ct x) { return uint64_t(x);} |]
          -- from double to float
          else if from_ct == [C.cty|double|] && to_ct == [C.cty|float|] then
            [C.cedecl|$ty:to_ct
                      $id:(convOp s from_t to_t)($ty:from_ct x) { return float32(x);} |]
          else
            [C.cedecl|$ty:to_ct
                      $id:(convOp s from_t to_t)($ty:from_ct x) { return x;} |]
          where from_ct = from_f from_t
                to_ct = to_f to_t

        mkFPConvFF = mkFPConv floatTypeToCType floatTypeToCType
        mkFPConvFI = mkFPConv floatTypeToCType intTypeToCType
        mkFPConvIF = mkFPConv intTypeToCType   floatTypeToCType
        mkFPConvFU = mkFPConv floatTypeToCType uintTypeToCType
        mkFPConvUF = mkFPConv uintTypeToCType  floatTypeToCType

        simpleFloatOp s e t =
          [C.cedecl|$ty:ct $id:(taggedF s t)($ty:ct x, $ty:ct y) { return $exp:e; }|]
            where ct = floatTypeToCType t
        floatCmpOp s e t =
          [C.cedecl|typename bool $id:(taggedF s t)($ty:ct x, $ty:ct y) { return $exp:e; }|]
            where ct = floatTypeToCType t

-- | Same as `cFloat32Funs` but to adapt for GLSL.
glFloat32Funs :: [C.Definition]
glFloat32Funs = [C.cunit|
    float $id:(funName' "log32")(float x) {
      return log(x);
    }

    float $id:(funName' "log2_32")(float x) {
      return log2(x);
    }

    float $id:(funName' "log10_32")(float x) {
      return (log(x) / log(10.0));
    }

    float $id:(funName' "sqrt32")(float x) {
      return sqrt(x);
    }

    float $id:(funName' "exp32")(float x) {
      return exp(x);
    }

    float $id:(funName' "cos32")(float x) {
      return cos(x);
    }

    float $id:(funName' "sin32")(float x) {
      return sin(x);
    }

    float $id:(funName' "tan32")(float x) {
      return tan(x);
    }

    float $id:(funName' "acos32")(float x) {
      return acos(x);
    }

    float $id:(funName' "asin32")(float x) {
      return asin(x);
    }

    float $id:(funName' "atan32")(float x) {
      return atan(x);
    }

    float $id:(funName' "cosh32")(float x) {
      return cosh(x);
    }

    float $id:(funName' "sinh32")(float x) {
      return sinh(x);
    }

    float $id:(funName' "tanh32")(float x) {
      return tanh(x);
    }

    float $id:(funName' "acosh32")(float x) {
      return acosh(x);
    }

    float $id:(funName' "asinh32")(float x) {
      return asinh(x);
    }

    float $id:(funName' "atanh32")(float x) {
      return atanh(x);
    }

    float $id:(funName' "atan2_32")(float x, float y) {
      return atan(x,y);
    }

    typename bool $id:(funName' "isnan32")(float x) {
      return isnan(x);
    }

    typename bool $id:(funName' "isinf32")(float x) {
      return isinf(x);
    }

    typename int32_t $id:(funName' "to_bits32")(float x) {
      return floatBitsToInt(x);
    }

    float $id:(funName' "from_bits32")(typename int32_t x) {
      return intBitsToFloat(x);
    }

    float fmod32(float x, float y) {
      return mod(x, y);
    }
    float $id:(funName' "round32")(float x) {
      return round(x);
    }
    float $id:(funName' "floor32")(float x) {
      return floor(x);
    }
    float $id:(funName' "ceil32")(float x) {
      return ceil(x);
    }
    float $id:(funName' "lerp32")(float v0, float v1, float t) {
      return mix(v0, v1, t);
    }
    float $id:(funName' "mad32")(float a, float b, float c) {
      return a*b+c;
    }
    float $id:(funName' "fma32")(float a, float b, float c) {
      return fma(a,b,c);
    }
|]

-- | Same as `cFloat64Funs` but to adapt for GLSL.
glFloat64Funs :: [C.Definition]
glFloat64Funs = [C.cunit|
    double $id:(funName' "log64")(double x) {
      return log(float32(x));
    }

    double $id:(funName' "log2_64")(double x) {
      return log2(float32(x));
    }

    double $id:(funName' "log10_64")(double x) {
      return (log(float32(x)) / log(10.0));
    }

    double $id:(funName' "sqrt64")(double x) {
      return sqrt(x);
    }

    double $id:(funName' "exp64")(double x) {
      return exp(float32(x));
    }

    double $id:(funName' "cos64")(double x) {
      return cos(float32(x));
    }

    double $id:(funName' "sin64")(double x) {
      return sin(float32(x));
    }

    double $id:(funName' "tan64")(double x) {
      return tan(float32(x));
    }

    double $id:(funName' "acos64")(double x) {
      return acos(float32(x));
    }

    double $id:(funName' "asin64")(double x) {
      return asin(float32(x));
    }

    double $id:(funName' "atan64")(double x) {
      return atan(float32(x));
    }

    double $id:(funName' "cosh64")(double x) {
      return cosh(float32(x));
    }

    double $id:(funName' "sinh64")(double x) {
      return sinh(float32(x));
    }

    double $id:(funName' "tanh64")(double x) {
      return tanh(float32(x));
    }

    double $id:(funName' "acosh64")(double x) {
      return acosh(float32(x));
    }

    double $id:(funName' "asinh64")(double x) {
      return asinh(float32(x));
    }

    double $id:(funName' "atanh64")(double x) {
      return atanh(float32(x));
    }

    double $id:(funName' "atan2_64")(double x, double y) {
      return atan(float32(x),float32(y));
    }

    double $id:(funName' "fma64")(double a, double b, double c) {
      return fma(a,b,c);
    }

    double $id:(funName' "round64")(double x) {
      return round(x);
    }

    double $id:(funName' "ceil64")(double x) {
      return ceil(x);
    }

    double $id:(funName' "floor64")(double x) {
      return floor(x);
    }

    typename bool $id:(funName' "isnan64")(double x) {
      return isnan(x);
    }

    typename bool $id:(funName' "isinf64")(double x) {
      return isinf(x);
    }

    double fmod64(double x, double y) {
      return mod(x, y);
    }

    double $id:(funName' "lerp64")(double v0, double v1, double t) {
      return mix(v0, v1, t);
    }
    double $id:(funName' "mad64")(double a, double b, double c) {
      return a*b+c;
    }
|]
