// Start of scalar_f16.h.

// Half-precision is emulated if needed (e.g. in straight C) with the
// native type used if possible.  The emulation works by typedef'ing
// 'float' to 'f16', and then implementing all operations on single
// precision.  To cut down on duplication, we use the same code for
// those Futhark functions that require just operators or casts.  The
// in-memory representation for arrays will still be 16 bits even
// under emulation, so the compiler will have to be careful when
// generating reads or writes.

#if !defined(cl_khr_fp16) && !(defined(__CUDA_ARCH__) && __CUDA_ARCH__ >= 600)
#define EMULATE_F16
#endif

#if !defined(EMULATE_F16) && defined(__OPENCL_VERSION__)
#pragma OPENCL EXTENSION cl_khr_fp16 : enable
#endif

#ifdef EMULATE_F16

// Note that the half-precision storage format is still 16 bits - the
// compiler will have to be real careful!
typedef float f16;

#else

#ifdef __CUDA_ARCH__
#include <cuda_fp16.h>
#endif

typedef half f16;

#endif

// Some of these functions convert to single precision because half
// precision versions are not available.

static inline f16 fadd16(f16 x, f16 y) {
  return x + y;
}

static inline f16 fsub16(f16 x, f16 y) {
  return x - y;
}

static inline f16 fmul16(f16 x, f16 y) {
  return x * y;
}

static inline bool cmplt16(f16 x, f16 y) {
  return x < y;
}

static inline bool cmple16(f16 x, f16 y) {
  return x <= y;
}

static inline f16 sitofp_i8_f16(int8_t x) {
  return (f16) x;
}

static inline f16 sitofp_i16_f16(int16_t x) {
  return (f16) x;
}

static inline f16 sitofp_i32_f16(int32_t x) {
  return (f16) x;
}

static inline f16 sitofp_i64_f16(int64_t x) {
  return (f16) x;
}

static inline f16 uitofp_i8_f16(uint8_t x) {
  return (f16) x;
}

static inline f16 uitofp_i16_f16(uint16_t x) {
  return (f16) x;
}

static inline f16 uitofp_i32_f16(uint32_t x) {
  return (f16) x;
}

static inline f16 uitofp_i64_f16(uint64_t x) {
  return (f16) x;
}

static inline int8_t fptosi_f16_i8(f16 x) {
  return (int8_t) (float) x;
}

static inline int16_t fptosi_f16_i16(f16 x) {
  return (int16_t) x;
}

static inline int32_t fptosi_f16_i32(f16 x) {
  return (int32_t) x;
}

static inline int64_t fptosi_f16_i64(f16 x) {
  return (int64_t) x;
}

static inline uint8_t fptoui_f16_i8(f16 x) {
  return (uint8_t) (float) x;
}

static inline uint16_t fptoui_f16_i16(f16 x) {
  return (uint16_t) x;
}

static inline uint32_t fptoui_f16_i32(f16 x) {
  return (uint32_t) x;
}

static inline uint64_t fptoui_f16_i64(f16 x) {
  return (uint64_t) x;
}

static inline bool ftob_f16_bool(f16 x) {
  return x != (f16)0;
}

static inline f16 btof_bool_f16(bool x) {
  return x ? 1 : 0;
}

#ifndef EMULATE_F16

#ifdef __OPENCL_VERSION__
static inline f16 fabs16(f16 x) {
  return fabs(x);
}

static inline f16 fmax16(f16 x, f16 y) {
  return fmax(x, y);
}

static inline f16 fmin16(f16 x, f16 y) {
  return fmin(x, y);
}

static inline f16 fpow16(f16 x, f16 y) {
  return pow(x, y);
}

#else // Assuming CUDA.

static inline f16 fabs16(f16 x) {
  return fabsf(x);
}

static inline f16 fmax16(f16 x, f16 y) {
  return fmaxf(x, y);
}

static inline f16 fmin16(f16 x, f16 y) {
  return fminf(x, y);
}

static inline f16 fpow16(f16 x, f16 y) {
  return powf(x, y);
}
#endif

static inline bool futrts_isnan16(f16 x) {
  return isnan((float)x);
}

static inline bool futrts_isinf16(f16 x) {
  return isinf((float)x);
}

#ifdef __OPENCL_VERSION__
static inline f16 futrts_log16(f16 x) {
  return log(x);
}

static inline f16 futrts_log2_16(f16 x) {
  return log2(x);
}

static inline f16 futrts_log10_16(f16 x) {
  return log10(x);
}

static inline f16 futrts_sqrt16(f16 x) {
  return sqrt(x);
}

static inline f16 futrts_cbrt16(f16 x) {
  return cbrt(x);
}

static inline f16 futrts_exp16(f16 x) {
  return exp(x);
}

static inline f16 futrts_cos16(f16 x) {
  return cos(x);
}

static inline f16 futrts_sin16(f16 x) {
  return sin(x);
}

static inline f16 futrts_tan16(f16 x) {
  return tan(x);
}

static inline f16 futrts_acos16(f16 x) {
  return acos(x);
}

static inline f16 futrts_asin16(f16 x) {
  return asin(x);
}

static inline f16 futrts_atan16(f16 x) {
  return atan(x);
}

static inline f16 futrts_cosh16(f16 x) {
  return cosh(x);
}

static inline f16 futrts_sinh16(f16 x) {
  return sinh(x);
}

static inline f16 futrts_tanh16(f16 x) {
  return tanh(x);
}

static inline f16 futrts_acosh16(f16 x) {
  return acosh(x);
}

static inline f16 futrts_asinh16(f16 x) {
  return asinh(x);
}

static inline f16 futrts_atanh16(f16 x) {
  return atanh(x);
}

static inline f16 futrts_atan2_16(f16 x, f16 y) {
  return atan2(x, y);
}

static inline f16 futrts_hypot16(f16 x, f16 y) {
  return hypot(x, y);
}

static inline f16 futrts_gamma16(f16 x) {
  return tgamma(x);
}

static inline f16 futrts_lgamma16(f16 x) {
  return lgamma(x);
}

static inline f16 futrts_erf16(f16 x) {
  return erf(x);
}

static inline f16 futrts_erfc16(f16 x) {
  return erfc(x);
}

static inline f16 fmod16(f16 x, f16 y) {
  return fmod(x, y);
}

static inline f16 futrts_round16(f16 x) {
  return rint(x);
}

static inline f16 futrts_floor16(f16 x) {
  return floor(x);
}

static inline f16 futrts_ceil16(f16 x) {
  return ceil(x);
}

static inline f16 futrts_lerp16(f16 v0, f16 v1, f16 t) {
  return mix(v0, v1, t);
}

static inline f16 futrts_mad16(f16 a, f16 b, f16 c) {
  return mad(a, b, c);
}

static inline f16 futrts_fma16(f16 a, f16 b, f16 c) {
  return fma(a, b, c);
}

#else // Assume CUDA.

static inline f16 futrts_log16(f16 x) {
  return hlog(x);
}

static inline f16 futrts_log2_16(f16 x) {
  return hlog2(x);
}

static inline f16 futrts_log10_16(f16 x) {
  return hlog10(x);
}

static inline f16 futrts_sqrt16(f16 x) {
  return hsqrt(x);
}

static inline f16 futrts_cbrt16(f16 x) {
  return cbrtf(x);
}

static inline f16 futrts_exp16(f16 x) {
  return hexp(x);
}

static inline f16 futrts_cos16(f16 x) {
  return hcos(x);
}

static inline f16 futrts_sin16(f16 x) {
  return hsin(x);
}

static inline f16 futrts_tan16(f16 x) {
  return tanf(x);
}

static inline f16 futrts_acos16(f16 x) {
  return acosf(x);
}

static inline f16 futrts_asin16(f16 x) {
  return asinf(x);
}

static inline f16 futrts_atan16(f16 x) {
  return atanf(x);
}

static inline f16 futrts_cosh16(f16 x) {
  return coshf(x);
}

static inline f16 futrts_sinh16(f16 x) {
  return sinhf(x);
}

static inline f16 futrts_tanh16(f16 x) {
  return tanhf(x);
}

static inline f16 futrts_acosh16(f16 x) {
  return acoshf(x);
}

static inline f16 futrts_asinh16(f16 x) {
  return asinhf(x);
}

static inline f16 futrts_atanh16(f16 x) {
  return atanhf(x);
}

static inline f16 futrts_atan2_16(f16 x, f16 y) {
  return atan2f(x, y);
}

static inline f16 futrts_hypot16(f16 x, f16 y) {
  return hypotf(x, y);
}

static inline f16 futrts_gamma16(f16 x) {
  return tgammaf(x);
}

static inline f16 futrts_lgamma16(f16 x) {
  return lgammaf(x);
}

static inline f16 futrts_erf16(f16 x) {
  return erff(x);
}

static inline f16 futrts_erfc16(f16 x) {
  return erfcf(x);
}

static inline f16 fmod16(f16 x, f16 y) {
  return fmodf(x, y);
}

static inline f16 futrts_round16(f16 x) {
  return rintf(x);
}

static inline f16 futrts_floor16(f16 x) {
  return hfloor(x);
}

static inline f16 futrts_ceil16(f16 x) {
  return hceil(x);
}

static inline f16 futrts_lerp16(f16 v0, f16 v1, f16 t) {
  return v0 + (v1 - v0) * t;
}

static inline f16 futrts_mad16(f16 a, f16 b, f16 c) {
  return a * b + c;
}

static inline f16 futrts_fma16(f16 a, f16 b, f16 c) {
  return fmaf(a, b, c);
}

#endif

// The CUDA __half type cannot be put in unions for some reason, so we
// use bespoke conversion functions instead.
#ifdef __CUDA_ARCH__
static inline int16_t futrts_to_bits16(f16 x) {
  return __half_as_ushort(x);
}
static inline f16 futrts_from_bits16(int16_t x) {
  return __ushort_as_half(x);
}
#else
static inline int16_t futrts_to_bits16(f16 x) {
  union {
    f16 f;
    int16_t t;
  } p;

  p.f = x;
  return p.t;
}

static inline f16 futrts_from_bits16(int16_t x) {
  union {
    int16_t f;
    f16 t;
  } p;

  p.f = x;
  return p.t;
}
#endif

#else // No native f16 - emulate.

static inline f16 fabs16(f16 x) {
  return fabs32(x);
}

static inline f16 fmax16(f16 x, f16 y) {
  return fmax32(x, y);
}

static inline f16 fmin16(f16 x, f16 y) {
  return fmin32(x, y);
}

static inline f16 fpow16(f16 x, f16 y) {
  return fpow32(x, y);
}

static inline bool futrts_isnan16(f16 x) {
  return futrts_isnan32(x);
}

static inline bool futrts_isinf16(f16 x) {
  return futrts_isinf32(x);
}

static inline f16 futrts_log16(f16 x) {
  return futrts_log32(x);
}

static inline f16 futrts_log2_16(f16 x) {
  return futrts_log2_32(x);
}

static inline f16 futrts_log10_16(f16 x) {
  return futrts_log10_32(x);
}

static inline f16 futrts_sqrt16(f16 x) {
  return futrts_sqrt32(x);
}

static inline f16 futrts_cbrt16(f16 x) {
  return futrts_cbrt32(x);
}

static inline f16 futrts_exp16(f16 x) {
  return futrts_exp32(x);
}

static inline f16 futrts_cos16(f16 x) {
  return futrts_cos32(x);
}

static inline f16 futrts_sin16(f16 x) {
  return futrts_sin32(x);
}

static inline f16 futrts_tan16(f16 x) {
  return futrts_tan32(x);
}

static inline f16 futrts_acos16(f16 x) {
  return futrts_acos32(x);
}

static inline f16 futrts_asin16(f16 x) {
  return futrts_asin32(x);
}

static inline f16 futrts_atan16(f16 x) {
  return futrts_atan32(x);
}

static inline f16 futrts_cosh16(f16 x) {
  return futrts_cosh32(x);
}

static inline f16 futrts_sinh16(f16 x) {
  return futrts_sinh32(x);
}

static inline f16 futrts_tanh16(f16 x) {
  return futrts_tanh32(x);
}

static inline f16 futrts_acosh16(f16 x) {
  return futrts_acosh32(x);
}

static inline f16 futrts_asinh16(f16 x) {
  return futrts_asinh32(x);
}

static inline f16 futrts_atanh16(f16 x) {
  return futrts_atanh32(x);
}

static inline f16 futrts_atan2_16(f16 x, f16 y) {
  return futrts_atan2_32(x, y);
}

static inline f16 futrts_hypot16(f16 x, f16 y) {
  return futrts_hypot32(x, y);
}

static inline f16 futrts_gamma16(f16 x) {
  return futrts_gamma32(x);
}

static inline f16 futrts_lgamma16(f16 x) {
  return futrts_lgamma32(x);
}

static inline f16 futrts_erf16(f16 x) {
  return futrts_erf32(x);
}

static inline f16 futrts_erfc16(f16 x) {
  return futrts_erfc32(x);
}

static inline f16 fmod16(f16 x, f16 y) {
  return fmod32(x, y);
}

static inline f16 futrts_round16(f16 x) {
  return futrts_round32(x);
}

static inline f16 futrts_floor16(f16 x) {
  return futrts_floor32(x);
}

static inline f16 futrts_ceil16(f16 x) {
  return futrts_ceil32(x);
}

static inline f16 futrts_lerp16(f16 v0, f16 v1, f16 t) {
  return futrts_lerp32(v0, v1, t);
}

static inline f16 futrts_mad16(f16 a, f16 b, f16 c) {
  return futrts_mad32(a, b, c);
}

static inline f16 futrts_fma16(f16 a, f16 b, f16 c) {
  return futrts_fma32(a, b, c);
}

// Even when we are using an OpenCL that does not support cl_khr_fp16,
// it must still support vload_half for actually creating a
// half-precision number, which can then be efficiently converted to a
// float.  Similarly for vstore_half.
#ifdef __OPENCL_VERSION__

static inline int16_t futrts_to_bits16(f16 x) {
  int16_t y;
  // Violating strict aliasing here.
  vstore_half((float)x, 0, (half*)&y);
  return y;
}

static inline f16 futrts_from_bits16(int16_t x) {
  return (f16)vload_half(0, (half*)&x);
}

#else

static inline int16_t futrts_to_bits16(f16 x) {
  return (int16_t)float2halfbits(x);
}

static inline f16 futrts_from_bits16(int16_t x) {
  return halfbits2float((uint16_t)x);
}

static inline f16 fsignum16(f16 x) {
  return futrts_isnan16(x) ? x : (x > 0) - (x < 0);
}

#endif

#endif

static inline float fpconv_f16_f16(f16 x) {
  return x;
}

static inline float fpconv_f16_f32(f16 x) {
  return x;
}

static inline f16 fpconv_f32_f16(float x) {
  return x;
}

#ifdef FUTHARK_F64_ENABLED

static inline double fpconv_f16_f64(f16 x) {
  return (double) x;
}

static inline f16 fpconv_f64_f16(double x) {
  return (f16) x;
}

#endif


// End of scalar_f16.h.
