// Start of prelude.metal

// The Futhark kernel code is OpenCL C with a few hooks.  This prelude
// makes it valid Metal Shading Language.

#include <metal_stdlib>
using namespace metal;

// Generated code may have unused variables and similar.
#pragma clang diagnostic ignored "-Wunused-variable"
#pragma clang diagnostic ignored "-Wunused-but-set-variable"
#pragma clang diagnostic ignored "-Wunused-function"
#pragma clang diagnostic ignored "-Wunused-label"
#pragma clang diagnostic ignored "-Wparentheses"
#pragma clang diagnostic ignored "-Wunknown-pragmas"

// scalar.h and scalar_f16.h select the OpenCL builtins (fabs,
// popcount, mix, ...) when this is defined.  The MSL math library
// mirrors the OpenCL one closely enough, and the functions it lacks
// are defined below.
#define __OPENCL_VERSION__ 120

// Apple GPUs support half precision natively.
#define cl_khr_fp16

#define SCALAR_FUN_ATTR static inline
#define FUTHARK_FUN_ATTR static

#define __global device
#define __local threadgroup
#define __private thread
#define __constant constant

// Every kernel takes its arguments as a struct in buffer 0, because
// Metal allows at most 31 buffer bindings, and Futhark kernels may
// take many more arguments than that.  Device memory is passed as GPU
// addresses.  The dynamically sized shared memory and the thread
// geometry are separate parameters.
#define FUTHARK_KERNEL_PARAMS(args_t)                                   \
  constant struct args_t *futhark_args [[buffer(0)]],                   \
  threadgroup uint64_t *shared_mem_aligned [[threadgroup(0)]],          \
  uint3 futhark_lid [[thread_position_in_threadgroup]],                 \
  uint3 futhark_tblock_id [[threadgroup_position_in_grid]],             \
  uint3 futhark_tblock_size [[threads_per_threadgroup]],                \
  uint3 futhark_num_tblocks [[threadgroups_per_grid]]

#define FUTHARK_KERNEL kernel
#define FUTHARK_KERNEL_SIZED(a,b,c) [[max_total_threads_per_threadgroup((a)*(b)*(c))]] kernel

#define get_local_id(d) ((int32_t)futhark_lid[d])
#define get_tblock_id(d) ((int32_t)futhark_tblock_id[d])
#define get_local_size(d) ((int32_t)futhark_tblock_size[d])
#define get_num_tblocks(d) ((int32_t)futhark_num_tblocks[d])
#define get_global_id(d) ((int32_t)(futhark_tblock_id[d] * futhark_tblock_size[d] + futhark_lid[d]))
#define get_global_size(d) ((int32_t)(futhark_num_tblocks[d] * futhark_tblock_size[d]))

#define CLK_LOCAL_MEM_FENCE 1
#define CLK_GLOBAL_MEM_FENCE 2

static inline void barrier(int flags) {
  if (flags & CLK_GLOBAL_MEM_FENCE) {
    threadgroup_barrier(mem_flags::mem_device | mem_flags::mem_threadgroup);
  } else {
    threadgroup_barrier(mem_flags::mem_threadgroup);
  }
}

static inline void barrier_local() {
  threadgroup_barrier(mem_flags::mem_threadgroup);
}

static inline void mem_fence_global() {
  atomic_thread_fence(mem_flags::mem_device | mem_flags::mem_threadgroup,
                      memory_order_seq_cst, thread_scope_device);
}

static inline void mem_fence_local() {
  atomic_thread_fence(mem_flags::mem_threadgroup,
                      memory_order_seq_cst, thread_scope_threadgroup);
}

// OpenCL names for MSL builtins.
#define as_float(x) as_type<float>(x)
#define as_int(x) as_type<int>(x)
#define as_uint(x) as_type<uint>(x)
#define as_long(x) as_type<long>(x)
#define as_ulong(x) as_type<ulong>(x)
#define as_half(x) as_type<half>(x)
#define as_short(x) as_type<short>(x)
#define as_ushort(x) as_type<ushort>(x)
#define mul_hi(a, b) mulhi(a, b)
#define mad_hi(a, b, c) (mulhi(a, b) + (c))
#define mad(a, b, c) fma(a, b, c)

// In MSL, int8_t is 'signed char', which is a different type from
// 'char', and the integer builtins are only overloaded for the
// latter.  Without these, the argument would be promoted to int.
static inline char popcount(signed char x) { return popcount((char)x); }
static inline char clz(signed char x) { return clz((char)x); }
static inline char ctz(signed char x) { return ctz((char)x); }
static inline char mulhi(signed char x, signed char y) { return mulhi((char)x, (char)y); }

// OpenCL math functions that MSL lacks.  The single-precision
// versions are also used for half precision.

static inline float log1p(float x) {
  float u = 1.0f + x;
  if (u == 1.0f || u == INFINITY) {
    return u == 1.0f ? x : u;
  }
  return log(u) * (x / (u - 1.0f));
}

static inline float expm1(float x) {
  float u = exp(x);
  if (u == 1.0f || u == INFINITY) {
    return u == 1.0f ? x : u;
  }
  if (u - 1.0f == -1.0f) {
    return -1.0f;
  }
  return (u - 1.0f) * (x / log(u));
}

static inline float cbrt(float x) {
  return copysign(pow(fabs(x), 1.0f/3.0f), x);
}

static inline float hypot(float x, float y) {
  x = fabs(x);
  y = fabs(y);
  float a = fmax(x, y), b = fmin(x, y);
  if (isinf(a)) {
    return a;
  }
  if (a == 0.0f) {
    return 0.0f;
  }
  float r = b / a;
  return a * sqrt(1.0f + r * r);
}

// Abramowitz and Stegun 7.1.26; absolute error at most 1.5e-7.
static inline float erf(float x) {
  float t = 1.0f / (1.0f + 0.3275911f * fabs(x));
  float y = 1.0f - (((((1.061405429f * t - 1.453152027f) * t) + 1.421413741f) * t
                     - 0.284496736f) * t + 0.254829592f) * t * exp(-x * x);
  return copysign(y, x);
}

static inline float erfc(float x) {
  return 1.0f - erf(x);
}

// Recurrence up to x >= 7, then the Stirling series.
static inline float lgamma(float x) {
  float r = 0.0f;
  while (x < 7.0f) {
    r -= log(fabs(x));
    x += 1.0f;
  }
  float x2 = 1.0f / (x * x);
  return r + (x - 0.5f) * log(x) - x + 0.91893853320467274f
    + (1.0f / 12.0f - x2 * (1.0f / 360.0f - x2 / 1260.0f)) / x;
}

static inline float tgamma(float x) {
  float sign = (x > 0.0f || ((int64_t)floor(x)) % 2 == 0) ? 1.0f : -1.0f;
  return sign * exp(lgamma(x));
}

static inline float acospi(float x) { return acos(x) / M_PI_F; }
static inline float asinpi(float x) { return asin(x) / M_PI_F; }
static inline float atanpi(float x) { return atan(x) / M_PI_F; }
static inline float atan2pi(float y, float x) { return atan2(y, x) / M_PI_F; }

// End of prelude.metal
