# Start of scalar.py.

import numpy as np
import math
import struct

pi16 = np.float16(np.pi)
pi32 = np.float32(np.pi)
pi64 = np.float64(np.pi)


def intlit(t, x):
    if t == np.int8:
        return np.int8(x)
    elif t == np.int16:
        return np.int16(x)
    elif t == np.int32:
        return np.int32(x)
    else:
        return np.int64(x)


def signed(x):
    if type(x) == np.uint8:
        return np.int8(x)
    elif type(x) == np.uint16:
        return np.int16(x)
    elif type(x) == np.uint32:
        return np.int32(x)
    else:
        return np.int64(x)


def unsigned(x):
    if type(x) == np.int8:
        return np.uint8(x)
    elif type(x) == np.int16:
        return np.uint16(x)
    elif type(x) == np.int32:
        return np.uint32(x)
    else:
        return np.uint64(x)


def shlN(x, y):
    return x << y


def ashrN(x, y):
    return x >> y


# Python is so slow that we just make all the unsafe operations safe,
# always.


def sdivN(x, y):
    if y == 0:
        return intlit(type(x), 0)
    else:
        return x // y


def sdiv_upN(x, y):
    if y == 0:
        return intlit(type(x), 0)
    else:
        return (x + y - intlit(type(x), 1)) // y


def smodN(x, y):
    if y == 0:
        return intlit(type(x), 0)
    else:
        return x % y


def udivN(x, y):
    if y == 0:
        return intlit(type(x), 0)
    else:
        return signed(unsigned(x) // unsigned(y))


def udiv_upN(x, y):
    if y == 0:
        return intlit(type(x), 0)
    else:
        return signed(
            (unsigned(x) + unsigned(y) - unsigned(intlit(type(x), 1)))
            // unsigned(y)
        )


def umodN(x, y):
    if y == 0:
        return intlit(type(x), 0)
    else:
        return signed(unsigned(x) % unsigned(y))


def squotN(x, y):
    if y == 0:
        return intlit(type(x), 0)
    else:
        return np.floor_divide(np.abs(x), np.abs(y)) * np.sign(x) * np.sign(y)


def sremN(x, y):
    if y == 0:
        return intlit(type(x), 0)
    else:
        return np.remainder(np.abs(x), np.abs(y)) * np.sign(x)


def sminN(x, y):
    return min(x, y)


def smaxN(x, y):
    return max(x, y)


def uminN(x, y):
    return signed(min(unsigned(x), unsigned(y)))


def umaxN(x, y):
    return signed(max(unsigned(x), unsigned(y)))


def fminN(x, y):
    return np.fmin(x, y)


def fmaxN(x, y):
    return np.fmax(x, y)


def powN(x, y):
    return x**y


def fpowN(x, y):
    return x**y


def sleN(x, y):
    return x <= y


def sltN(x, y):
    return x < y


def uleN(x, y):
    return unsigned(x) <= unsigned(y)


def ultN(x, y):
    return unsigned(x) < unsigned(y)


def lshr8(x, y):
    return np.int8(np.uint8(x) >> np.uint8(y))


def lshr16(x, y):
    return np.int16(np.uint16(x) >> np.uint16(y))


def lshr32(x, y):
    return np.int32(np.uint32(x) >> np.uint32(y))


def lshr64(x, y):
    return np.int64(np.uint64(x) >> np.uint64(y))


def sext_T_i8(x):
    return np.int8(x)


def sext_T_i16(x):
    return np.int16(x)


def sext_T_i32(x):
    return np.int32(x)


def sext_T_i64(x):
    return np.int64(x)


def itob_T_bool(x):
    return bool(x)


def btoi_bool_i8(x):
    return np.int8(x)


def btoi_bool_i16(x):
    return np.int16(x)


def btoi_bool_i32(x):
    return np.int32(x)


def btoi_bool_i64(x):
    return np.int64(x)


def ftob_T_bool(x):
    return bool(x)


def btof_bool_f16(x):
    return np.float16(x)


def btof_bool_f32(x):
    return np.float32(x)


def btof_bool_f64(x):
    return np.float64(x)


def zext_i8_i8(x):
    return np.int8(np.uint8(x))


def zext_i8_i16(x):
    return np.int16(np.uint8(x))


def zext_i8_i32(x):
    return np.int32(np.uint8(x))


def zext_i8_i64(x):
    return np.int64(np.uint8(x))


def zext_i16_i8(x):
    return np.int8(np.uint16(x))


def zext_i16_i16(x):
    return np.int16(np.uint16(x))


def zext_i16_i32(x):
    return np.int32(np.uint16(x))


def zext_i16_i64(x):
    return np.int64(np.uint16(x))


def zext_i32_i8(x):
    return np.int8(np.uint32(x))


def zext_i32_i16(x):
    return np.int16(np.uint32(x))


def zext_i32_i32(x):
    return np.int32(np.uint32(x))


def zext_i32_i64(x):
    return np.int64(np.uint32(x))


def zext_i64_i8(x):
    return np.int8(np.uint64(x))


def zext_i64_i16(x):
    return np.int16(np.uint64(x))


def zext_i64_i32(x):
    return np.int32(np.uint64(x))


def zext_i64_i64(x):
    return np.int64(np.uint64(x))


sdiv8 = sdiv16 = sdiv32 = sdiv64 = sdivN
sdiv_up8 = sdiv1_up6 = sdiv_up32 = sdiv_up64 = sdiv_upN
sdiv_safe8 = sdiv1_safe6 = sdiv_safe32 = sdiv_safe64 = sdivN
sdiv_up_safe8 = sdiv_up1_safe6 = sdiv_up_safe32 = sdiv_up_safe64 = sdiv_upN
smod8 = smod16 = smod32 = smod64 = smodN
smod_safe8 = smod_safe16 = smod_safe32 = smod_safe64 = smodN
udiv8 = udiv16 = udiv32 = udiv64 = udivN
udiv_up8 = udiv_up16 = udiv_up32 = udiv_up64 = udivN
udiv_safe8 = udiv_safe16 = udiv_safe32 = udiv_safe64 = udiv_upN
udiv_up_safe8 = udiv_up_safe16 = udiv_up_safe32 = udiv_up_safe64 = udiv_upN
umod8 = umod16 = umod32 = umod64 = umodN
umod_safe8 = umod_safe16 = umod_safe32 = umod_safe64 = umodN
squot8 = squot16 = squot32 = squot64 = squotN
squot_safe8 = squot_safe16 = squot_safe32 = squot_safe64 = squotN
srem8 = srem16 = srem32 = srem64 = sremN
srem_safe8 = srem_safe16 = srem_safe32 = srem_safe64 = sremN

shl8 = shl16 = shl32 = shl64 = shlN
ashr8 = ashr16 = ashr32 = ashr64 = ashrN
smax8 = smax16 = smax32 = smax64 = smaxN
smin8 = smin16 = smin32 = smin64 = sminN
umax8 = umax16 = umax32 = umax64 = umaxN
umin8 = umin16 = umin32 = umin64 = uminN
pow8 = pow16 = pow32 = pow64 = powN
fpow16 = fpow32 = fpow64 = fpowN
fmax16 = fmax32 = fmax64 = fmaxN
fmin16 = fmin32 = fmin64 = fminN
sle8 = sle16 = sle32 = sle64 = sleN
slt8 = slt16 = slt32 = slt64 = sltN
ule8 = ule16 = ule32 = ule64 = uleN
ult8 = ult16 = ult32 = ult64 = ultN
sext_i8_i8 = sext_i16_i8 = sext_i32_i8 = sext_i64_i8 = sext_T_i8
sext_i8_i16 = sext_i16_i16 = sext_i32_i16 = sext_i64_i16 = sext_T_i16
sext_i8_i32 = sext_i16_i32 = sext_i32_i32 = sext_i64_i32 = sext_T_i32
sext_i8_i64 = sext_i16_i64 = sext_i32_i64 = sext_i64_i64 = sext_T_i64
itob_i8_bool = itob_i16_bool = itob_i32_bool = itob_i64_bool = itob_T_bool
ftob_f16_bool = ftob_f32_bool = ftob_f64_bool = ftob_T_bool


def clz_T(x):
    n = np.int32(0)
    bits = x.itemsize * 8
    for i in range(bits):
        if x < 0:
            break
        n += np.int32(1)
        x <<= np.int8(1)
    return n


def ctz_T(x):
    n = np.int32(0)
    bits = x.itemsize * 8
    for i in range(bits):
        if (x & 1) == 1:
            break
        n += np.int32(1)
        x >>= np.int8(1)
    return n


def popc_T(x):
    c = np.int32(0)
    while x != 0:
        x &= x - np.int8(1)
        c += np.int32(1)
    return c


futhark_popc8 = futhark_popc16 = futhark_popc32 = futhark_popc64 = popc_T
futhark_clzz8 = futhark_clzz16 = futhark_clzz32 = futhark_clzz64 = clz_T
futhark_ctzz8 = futhark_ctzz16 = futhark_ctzz32 = futhark_ctzz64 = ctz_T


def ssignum(x):
    return np.sign(x)


def usignum(x):
    if x < 0:
        return ssignum(-x)
    else:
        return ssignum(x)


def sitofp_T_f32(x):
    return np.float32(x)


sitofp_i8_f32 = sitofp_i16_f32 = sitofp_i32_f32 = sitofp_i64_f32 = sitofp_T_f32


def sitofp_T_f64(x):
    return np.float64(x)


sitofp_i8_f64 = sitofp_i16_f64 = sitofp_i32_f64 = sitofp_i64_f64 = sitofp_T_f64


def uitofp_T_f32(x):
    return np.float32(unsigned(x))


uitofp_i8_f32 = uitofp_i16_f32 = uitofp_i32_f32 = uitofp_i64_f32 = uitofp_T_f32


def uitofp_T_f64(x):
    return np.float64(unsigned(x))


uitofp_i8_f64 = uitofp_i16_f64 = uitofp_i32_f64 = uitofp_i64_f64 = uitofp_T_f64


def fptosi_T_i8(x):
    if np.isnan(x) or np.isinf(x):
        return np.int8(0)
    else:
        return np.int8(np.trunc(x))


fptosi_f16_i8 = fptosi_f32_i8 = fptosi_f64_i8 = fptosi_T_i8


def fptosi_T_i16(x):
    if np.isnan(x) or np.isinf(x):
        return np.int16(0)
    else:
        return np.int16(np.trunc(x))


fptosi_f16_i16 = fptosi_f32_i16 = fptosi_f64_i16 = fptosi_T_i16


def fptosi_T_i32(x):
    if np.isnan(x) or np.isinf(x):
        return np.int32(0)
    else:
        return np.int32(np.trunc(x))


fptosi_f16_i32 = fptosi_f32_i32 = fptosi_f64_i32 = fptosi_T_i32


def fptosi_T_i64(x):
    if np.isnan(x) or np.isinf(x):
        return np.int64(0)
    else:
        return np.int64(np.trunc(x))


fptosi_f16_i64 = fptosi_f32_i64 = fptosi_f64_i64 = fptosi_T_i64


def fptoui_T_i8(x):
    if np.isnan(x) or np.isinf(x):
        return np.int8(0)
    else:
        return np.int8(np.trunc(x))


fptoui_f16_i8 = fptoui_f32_i8 = fptoui_f64_i8 = fptoui_T_i8


def fptoui_T_i16(x):
    if np.isnan(x) or np.isinf(x):
        return np.int16(0)
    else:
        return np.int16(np.trunc(x))


fptoui_f16_i16 = fptoui_f32_i16 = fptoui_f64_i16 = fptoui_T_i16


def fptoui_T_i32(x):
    if np.isnan(x) or np.isinf(x):
        return np.int32(0)
    else:
        return np.int32(np.trunc(x))


fptoui_f16_i32 = fptoui_f32_i32 = fptoui_f64_i32 = fptoui_T_i32


def fptoui_T_i64(x):
    if np.isnan(x) or np.isinf(x):
        return np.int64(0)
    else:
        return np.int64(np.trunc(x))


fptoui_f16_i64 = fptoui_f32_i64 = fptoui_f64_i64 = fptoui_T_i64


def fpconv_f16_f32(x):
    return np.float32(x)


def fpconv_f16_f64(x):
    return np.float64(x)


def fpconv_f32_f16(x):
    return np.float16(x)


def fpconv_f32_f64(x):
    return np.float64(x)


def fpconv_f64_f16(x):
    return np.float16(x)


def fpconv_f64_f32(x):
    return np.float32(x)


def futhark_umul_hi8(a, b):
    return np.int8(
        (np.uint64(np.uint8(a)) * np.uint64(np.uint8(b))) >> np.uint64(8)
    )


def futhark_umul_hi16(a, b):
    return np.int16(
        (np.uint64(np.uint16(a)) * np.uint64(np.uint16(b))) >> np.uint64(16)
    )


def futhark_umul_hi32(a, b):
    return np.int32(
        (np.uint64(np.uint32(a)) * np.uint64(np.uint32(b))) >> np.uint64(32)
    )


def futhark_umul_hi64(a, b):
    return np.int64(np.uint64(int(np.uint64(a)) * int(np.uint64(b)) >> 64))


def futhark_smul_hi8(a, b):
    return np.int8((np.int64(a) * np.int64(b)) >> np.int64(8))


def futhark_smul_hi16(a, b):
    return np.int16((np.int64(a) * np.int64(b)) >> np.int64(16))


def futhark_smul_hi32(a, b):
    return np.int32((np.int64(a) * np.int64(b)) >> np.int64(32))


def futhark_smul_hi64(a, b):
    return np.int64(int(a) * int(b) >> 64)


def futhark_umad_hi8(a, b, c):
    return futhark_umul_hi8(a, b) + c


def futhark_umad_hi16(a, b, c):
    return futhark_umul_hi16(a, b) + c


def futhark_umad_hi32(a, b, c):
    return futhark_umul_hi32(a, b) + c


def futhark_umad_hi64(a, b, c):
    return futhark_umul_hi64(a, b) + c


def futhark_smad_hi8(a, b, c):
    return futhark_smul_hi8(a, b) + c


def futhark_smad_hi16(a, b, c):
    return futhark_smul_hi16(a, b) + c


def futhark_smad_hi32(a, b, c):
    return futhark_smul_hi32(a, b) + c


def futhark_smad_hi64(a, b, c):
    return futhark_smul_hi64(a, b) + c


def futhark_log64(x):
    return np.float64(np.log(x))


def futhark_log2_64(x):
    return np.float64(np.log2(x))


def futhark_log10_64(x):
    return np.float64(np.log10(x))


def futhark_log1p_64(x):
    return np.float64(np.log1p(x))


def futhark_sqrt64(x):
    return np.sqrt(x)


def futhark_rsqrt64(x):
    return 1 / np.sqrt(x)


def futhark_cbrt64(x):
    return np.cbrt(x)


def futhark_exp64(x):
    return np.exp(x)


def futhark_cos64(x):
    return np.cos(x)


def futhark_cospi64(x):
    return np.cos(pi64 * x)


def futhark_sin64(x):
    return np.sin(x)


def futhark_sinpi64(x):
    return np.sin(pi64 * x)


def futhark_tan64(x):
    return np.tan(x)


def futhark_tanpi64(x):
    return np.tan(pi64 * x)


def futhark_acos64(x):
    return np.arccos(x)


def futhark_acospi64(x):
    return np.arccos(x) / pi64


def futhark_asin64(x):
    return np.arcsin(x)


def futhark_asinpi64(x):
    return np.arcsin(x) / pi64


def futhark_atan64(x):
    return np.arctan(x)


def futhark_atanpi64(x):
    return np.arctan(x) / pi64


def futhark_cosh64(x):
    return np.cosh(x)


def futhark_sinh64(x):
    return np.sinh(x)


def futhark_tanh64(x):
    return np.tanh(x)


def futhark_acosh64(x):
    return np.arccosh(x)


def futhark_asinh64(x):
    return np.arcsinh(x)


def futhark_atanh64(x):
    return np.arctanh(x)


def futhark_atan2_64(x, y):
    return np.arctan2(x, y)


def futhark_atan2pi_64(x, y):
    return np.arctan2(x, y) / pi64


def futhark_hypot64(x, y):
    return np.hypot(x, y)


def futhark_gamma64(x):
    return np.float64(math.gamma(x))


def futhark_lgamma64(x):
    return np.float64(math.lgamma(x))


def futhark_erf64(x):
    return np.float64(math.erf(x))


def futhark_erfc64(x):
    return np.float64(math.erfc(x))


def futhark_round64(x):
    return np.round(x)


def futhark_ceil64(x):
    return np.ceil(x)


def futhark_floor64(x):
    return np.floor(x)


def futhark_nextafter64(x, y):
    return np.nextafter(x, y)


def futhark_isnan64(x):
    return np.isnan(x)


def futhark_isinf64(x):
    return np.isinf(x)


def futhark_to_bits64(x):
    s = struct.pack(">d", x)
    return np.int64(struct.unpack(">q", s)[0])


def futhark_from_bits64(x):
    s = struct.pack(">q", x)
    return np.float64(struct.unpack(">d", s)[0])


def futhark_log32(x):
    return np.float32(np.log(x))


def futhark_log2_32(x):
    return np.float32(np.log2(x))


def futhark_log10_32(x):
    return np.float32(np.log10(x))


def futhark_log1p_32(x):
    return np.float32(np.log1p(x))


def futhark_sqrt32(x):
    return np.float32(np.sqrt(x))


def futhark_rsqrt32(x):
    return np.float32(1 / np.sqrt(x))


def futhark_cbrt32(x):
    return np.float32(np.cbrt(x))


def futhark_exp32(x):
    return np.exp(x)


def futhark_cos32(x):
    return np.cos(x)


def futhark_cospi32(x):
    return np.cos(pi32 * x)


def futhark_sin32(x):
    return np.sin(x)


def futhark_sinpi32(x):
    return np.sin(pi32 * x)


def futhark_tan32(x):
    return np.tan(x)


def futhark_tanpi32(x):
    return np.tan(pi32 * x)


def futhark_acos32(x):
    return np.arccos(x)


def futhark_acospi32(x):
    return np.arccos(x) / pi32


def futhark_asin32(x):
    return np.arcsin(x)


def futhark_asinpi32(x):
    return np.arcsin(x) / pi32


def futhark_atan32(x):
    return np.arctan(x)


def futhark_atanpi32(x):
    return np.arctan(x) / pi32


def futhark_cosh32(x):
    return np.cosh(x)


def futhark_sinh32(x):
    return np.sinh(x)


def futhark_tanh32(x):
    return np.tanh(x)


def futhark_acosh32(x):
    return np.arccosh(x)


def futhark_asinh32(x):
    return np.arcsinh(x)


def futhark_atanh32(x):
    return np.arctanh(x)


def futhark_atan2_32(x, y):
    return np.arctan2(x, y)


def futhark_atan2pi_32(x, y):
    return np.arctan2(x, y) / pi32


def futhark_hypot32(x, y):
    return np.hypot(x, y)


def futhark_gamma32(x):
    return np.float32(math.gamma(x))


def futhark_lgamma32(x):
    return np.float32(math.lgamma(x))


def futhark_erf32(x):
    return np.float32(math.erf(x))


def futhark_erfc32(x):
    return np.float32(math.erfc(x))


def futhark_round32(x):
    return np.round(x)


def futhark_ceil32(x):
    return np.ceil(x)


def futhark_floor32(x):
    return np.floor(x)


def futhark_nextafter32(x, y):
    return np.nextafter(x, y)


def futhark_isnan32(x):
    return np.isnan(x)


def futhark_isinf32(x):
    return np.isinf(x)


def futhark_to_bits32(x):
    s = struct.pack(">f", x)
    return np.int32(struct.unpack(">l", s)[0])


def futhark_from_bits32(x):
    s = struct.pack(">l", x)
    return np.float32(struct.unpack(">f", s)[0])


def futhark_log16(x):
    return np.float16(np.log(x))


def futhark_log2_16(x):
    return np.float16(np.log2(x))


def futhark_log10_16(x):
    return np.float16(np.log10(x))


def futhark_log1p_16(x):
    return np.float16(np.log1p(x))


def futhark_sqrt16(x):
    return np.float16(np.sqrt(x))


def futhark_rsqrt16(x):
    return np.float16(1 / np.sqrt(x))


def futhark_cbrt16(x):
    return np.float16(np.cbrt(x))


def futhark_exp16(x):
    return np.exp(x)


def futhark_cos16(x):
    return np.cos(x)


def futhark_cospi16(x):
    return np.cos(pi16 * x)


def futhark_sin16(x):
    return np.sin(x)


def futhark_sinpi16(x):
    return np.sin(pi16 * x)


def futhark_tan16(x):
    return np.tan(x)


def futhark_tanpi16(x):
    return np.tan(pi16 * x)


def futhark_acos16(x):
    return np.arccos(x)


def futhark_acospi16(x):
    return np.arccos(x) / pi16


def futhark_asin16(x):
    return np.arcsin(x)


def futhark_asinpi16(x):
    return np.arcsin(x) / pi16


def futhark_atan16(x):
    return np.arctan(x)


def futhark_atanpi16(x):
    return np.arctan(x) / pi16


def futhark_cosh16(x):
    return np.cosh(x)


def futhark_sinh16(x):
    return np.sinh(x)


def futhark_tanh16(x):
    return np.tanh(x)


def futhark_acosh16(x):
    return np.arccosh(x)


def futhark_asinh16(x):
    return np.arcsinh(x)


def futhark_atanh16(x):
    return np.arctanh(x)


def futhark_atan2_16(x, y):
    return np.arctan2(x, y)


def futhark_atan2pi_16(x, y):
    return np.arctan2(x, y) / pi16


def futhark_hypot16(x, y):
    return np.hypot(x, y)


def futhark_gamma16(x):
    return np.float16(math.gamma(x))


def futhark_lgamma16(x):
    return np.float16(math.lgamma(x))


def futhark_erf16(x):
    return np.float16(math.erf(x))


def futhark_erfc16(x):
    return np.float16(math.erfc(x))


def futhark_round16(x):
    return np.round(x)


def futhark_ceil16(x):
    return np.ceil(x)


def futhark_floor16(x):
    return np.floor(x)


def futhark_nextafter16(x, y):
    return np.nextafter(x, y)


def futhark_isnan16(x):
    return np.isnan(x)


def futhark_isinf16(x):
    return np.isinf(x)


def futhark_to_bits16(x):
    s = struct.pack(">e", x)
    return np.int16(struct.unpack(">H", s)[0])


def futhark_from_bits16(x):
    s = struct.pack(">H", np.uint16(x))
    return np.float16(struct.unpack(">e", s)[0])


def futhark_lerp16(v0, v1, t):
    return v0 + (v1 - v0) * t


def futhark_lerp32(v0, v1, t):
    return v0 + (v1 - v0) * t


def futhark_lerp64(v0, v1, t):
    return v0 + (v1 - v0) * t


def futhark_ldexp16(x, y):
    return np.ldexp(x, y)


def futhark_ldexp32(x, y):
    return np.ldexp(x, y)


def futhark_ldexp64(x, y):
    return np.ldexp(x, y)


def futhark_mad16(a, b, c):
    return a * b + c


def futhark_mad32(a, b, c):
    return a * b + c


def futhark_mad64(a, b, c):
    return a * b + c


def futhark_fma16(a, b, c):
    return a * b + c


def futhark_fma32(a, b, c):
    return a * b + c


def futhark_fma64(a, b, c):
    return a * b + c


futhark_copysign16 = futhark_copysign32 = futhark_copysign64 = np.copysign


def futhark_cond(x, y, z):
    return y if x else z


futhark_cond_f16 = futhark_cond_f32 = futhark_cond_f64 = futhark_cond
futhark_cond_i18 = futhark_cond_i16 = futhark_cond_i32 = futhark_cond_i64 = (
    futhark_cond
)
futhark_cond_bool = futhark_cond_unit = futhark_cond


# End of scalar.py.
