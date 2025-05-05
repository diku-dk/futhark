// Start of atomics.h

SCALAR_FUN_ATTR int32_t atomic_xchg_i32_global(volatile __global int32_t *p, int32_t x);
SCALAR_FUN_ATTR int32_t atomic_xchg_i32_shared(volatile __local int32_t *p, int32_t x);
SCALAR_FUN_ATTR int32_t atomic_cmpxchg_i32_global(volatile __global int32_t *p,
                                                  int32_t cmp, int32_t val);
SCALAR_FUN_ATTR int32_t atomic_cmpxchg_i32_shared(volatile __local int32_t *p,
                                                  int32_t cmp, int32_t val);
SCALAR_FUN_ATTR int32_t atomic_add_i32_global(volatile __global int32_t *p, int32_t x);
SCALAR_FUN_ATTR int32_t atomic_add_i32_shared(volatile __local int32_t *p, int32_t x);
SCALAR_FUN_ATTR float atomic_fadd_f32_global(volatile __global float *p, float x);
SCALAR_FUN_ATTR float atomic_fadd_f32_shared(volatile __local float *p, float x);
SCALAR_FUN_ATTR int32_t atomic_smax_i32_global(volatile __global int32_t *p, int32_t x);
SCALAR_FUN_ATTR int32_t atomic_smax_i32_shared(volatile __local int32_t *p, int32_t x);
SCALAR_FUN_ATTR int32_t atomic_smin_i32_global(volatile __global int32_t *p, int32_t x);
SCALAR_FUN_ATTR int32_t atomic_smin_i32_shared(volatile __local int32_t *p, int32_t x);
SCALAR_FUN_ATTR uint32_t atomic_umax_i32_global(volatile __global uint32_t *p, uint32_t x);
SCALAR_FUN_ATTR uint32_t atomic_umax_i32_shared(volatile __local uint32_t *p, uint32_t x);
SCALAR_FUN_ATTR uint32_t atomic_umin_i32_global(volatile __global uint32_t *p, uint32_t x);
SCALAR_FUN_ATTR uint32_t atomic_umin_i32_shared(volatile __local uint32_t *p, uint32_t x);
SCALAR_FUN_ATTR int32_t atomic_and_i32_global(volatile __global int32_t *p, int32_t x);
SCALAR_FUN_ATTR int32_t atomic_and_i32_shared(volatile __local int32_t *p, int32_t x);
SCALAR_FUN_ATTR int32_t atomic_or_i32_global(volatile __global int32_t *p, int32_t x);
SCALAR_FUN_ATTR int32_t atomic_or_i32_shared(volatile __local int32_t *p, int32_t x);
SCALAR_FUN_ATTR int32_t atomic_xor_i32_global(volatile __global int32_t *p, int32_t x);
SCALAR_FUN_ATTR int32_t atomic_xor_i32_shared(volatile __local int32_t *p, int32_t x);

SCALAR_FUN_ATTR int32_t atomic_xchg_i32_global(volatile __global int32_t *p, int32_t x) {
#if defined(FUTHARK_CUDA) || defined(FUTHARK_HIP)
  return atomicExch((int32_t*)p, x);
#else
  return atomic_xor(p, x);
#endif
}

SCALAR_FUN_ATTR int32_t atomic_xchg_i32_shared(volatile __local int32_t *p, int32_t x) {
#if defined(FUTHARK_CUDA) || defined(FUTHARK_HIP)
  return atomicExch((int32_t*)p, x);
#else
  return atomic_xor(p, x);
#endif
}

SCALAR_FUN_ATTR int32_t atomic_cmpxchg_i32_global(volatile __global int32_t *p,
                                                  int32_t cmp, int32_t val) {
#if defined(FUTHARK_CUDA) || defined(FUTHARK_HIP)
  return atomicCAS((int32_t*)p, cmp, val);
#else
  return atomic_cmpxchg(p, cmp, val);
#endif
}

SCALAR_FUN_ATTR int32_t atomic_cmpxchg_i32_shared(volatile __local int32_t *p,
                                                  int32_t cmp, int32_t val) {
#if defined(FUTHARK_CUDA) || defined(FUTHARK_HIP)
  return atomicCAS((int32_t*)p, cmp, val);
#else
  return atomic_cmpxchg(p, cmp, val);
#endif
}

SCALAR_FUN_ATTR int32_t atomic_add_i32_global(volatile __global int32_t *p, int32_t x) {
#if defined(FUTHARK_CUDA) || defined(FUTHARK_HIP)
  return atomicAdd((int32_t*)p, x);
#else
  return atomic_add(p, x);
#endif
}

SCALAR_FUN_ATTR int32_t atomic_add_i32_shared(volatile __local int32_t *p, int32_t x) {
#if defined(FUTHARK_CUDA) || defined(FUTHARK_HIP)
  return atomicAdd((int32_t*)p, x);
#else
  return atomic_add(p, x);
#endif
}

SCALAR_FUN_ATTR float atomic_fadd_f32_global(volatile __global float *p, float x) {
#if defined(FUTHARK_CUDA) || defined(FUTHARK_HIP)
  return atomicAdd((float*)p, x);
  // On OpenCL, use technique from
  // https://pipinspace.github.io/blog/atomic-float-addition-in-opencl.html
#elif defined(cl_nv_pragma_unroll)
  // use hardware-supported atomic addition on Nvidia GPUs with inline
  // PTX assembly
  float ret;
  asm volatile("atom.global.add.f32 %0,[%1],%2;":"=f"(ret):"l"(p),"f"(x):"memory");
  return ret;
#elif defined(__opencl_c_ext_fp32_global_atomic_add)
  // use hardware-supported atomic addition on some Intel GPUs
  return atomic_fetch_add_explicit((volatile __global atomic_float*)p,
                                   x,
                                   memory_order_relaxed);
#elif __has_builtin(__builtin_amdgcn_global_atomic_fadd_f32)
  // use hardware-supported atomic addition on some AMD GPUs
  return __builtin_amdgcn_global_atomic_fadd_f32(p, x);
#else
  // fallback emulation:
  // https://forums.developer.nvidia.com/t/atomicadd-float-float-atomicmul-float-float/14639/5
  float old = x;
  float ret;
  while ((old=atomic_xchg(p, ret=atomic_xchg(p, 0.0f)+old))!=0.0f);
  return ret;
#endif
}

SCALAR_FUN_ATTR float atomic_fadd_f32_shared(volatile __local float *p, float x) {
#if defined(FUTHARK_CUDA) || defined(FUTHARK_HIP)
  return atomicAdd((float*)p, x);
#else
  union { int32_t i; float f; } old;
  union { int32_t i; float f; } assumed;
  old.f = *p;
  do {
    assumed.f = old.f;
    old.f = old.f + x;
    old.i = atomic_cmpxchg_i32_shared((volatile __local int32_t*)p, assumed.i, old.i);
  } while (assumed.i != old.i);
  return old.f;
#endif
}

SCALAR_FUN_ATTR int32_t atomic_smax_i32_global(volatile __global int32_t *p, int32_t x) {
#if defined(FUTHARK_CUDA) || defined(FUTHARK_HIP)
  return atomicMax((int32_t*)p, x);
#else
  return atomic_max(p, x);
#endif
}

SCALAR_FUN_ATTR int32_t atomic_smax_i32_shared(volatile __local int32_t *p, int32_t x) {
#if defined(FUTHARK_CUDA) || defined(FUTHARK_HIP)
  return atomicMax((int32_t*)p, x);
#else
  return atomic_max(p, x);
#endif
}

SCALAR_FUN_ATTR int32_t atomic_smin_i32_global(volatile __global int32_t *p, int32_t x) {
#if defined(FUTHARK_CUDA) || defined(FUTHARK_HIP)
  return atomicMin((int32_t*)p, x);
#else
  return atomic_min(p, x);
#endif
}

SCALAR_FUN_ATTR int32_t atomic_smin_i32_shared(volatile __local int32_t *p, int32_t x) {
#if defined(FUTHARK_CUDA) || defined(FUTHARK_HIP)
  return atomicMin((int32_t*)p, x);
#else
  return atomic_min(p, x);
#endif
}

SCALAR_FUN_ATTR uint32_t atomic_umax_i32_global(volatile __global uint32_t *p, uint32_t x) {
#if defined(FUTHARK_CUDA) || defined(FUTHARK_HIP)
  return atomicMax((uint32_t*)p, x);
#else
  return atomic_max(p, x);
#endif
}

SCALAR_FUN_ATTR uint32_t atomic_umax_i32_shared(volatile __local uint32_t *p, uint32_t x) {
#if defined(FUTHARK_CUDA) || defined(FUTHARK_HIP)
  return atomicMax((uint32_t*)p, x);
#else
  return atomic_max(p, x);
#endif
}

SCALAR_FUN_ATTR uint32_t atomic_umin_i32_global(volatile __global uint32_t *p, uint32_t x) {
#if defined(FUTHARK_CUDA) || defined(FUTHARK_HIP)
  return atomicMin((uint32_t*)p, x);
#else
  return atomic_min(p, x);
#endif
}

SCALAR_FUN_ATTR uint32_t atomic_umin_i32_shared(volatile __local uint32_t *p, uint32_t x) {
#if defined(FUTHARK_CUDA) || defined(FUTHARK_HIP)
  return atomicMin((uint32_t*)p, x);
#else
  return atomic_min(p, x);
#endif
}

SCALAR_FUN_ATTR int32_t atomic_and_i32_global(volatile __global int32_t *p, int32_t x) {
#if defined(FUTHARK_CUDA) || defined(FUTHARK_HIP)
  return atomicAnd((int32_t*)p, x);
#else
  return atomic_and(p, x);
#endif
}

SCALAR_FUN_ATTR int32_t atomic_and_i32_shared(volatile __local int32_t *p, int32_t x) {
#if defined(FUTHARK_CUDA) || defined(FUTHARK_HIP)
  return atomicAnd((int32_t*)p, x);
#else
  return atomic_and(p, x);
#endif
}

SCALAR_FUN_ATTR int32_t atomic_or_i32_global(volatile __global int32_t *p, int32_t x) {
#if defined(FUTHARK_CUDA) || defined(FUTHARK_HIP)
  return atomicOr((int32_t*)p, x);
#else
  return atomic_or(p, x);
#endif
}

SCALAR_FUN_ATTR int32_t atomic_or_i32_shared(volatile __local int32_t *p, int32_t x) {
#if defined(FUTHARK_CUDA) || defined(FUTHARK_HIP)
  return atomicOr((int32_t*)p, x);
#else
  return atomic_or(p, x);
#endif
}

SCALAR_FUN_ATTR int32_t atomic_xor_i32_global(volatile __global int32_t *p, int32_t x) {
#if defined(FUTHARK_CUDA) || defined(FUTHARK_HIP)
  return atomicXor((int32_t*)p, x);
#else
  return atomic_xor(p, x);
#endif
}

SCALAR_FUN_ATTR int32_t atomic_xor_i32_shared(volatile __local int32_t *p, int32_t x) {
#if defined(FUTHARK_CUDA) || defined(FUTHARK_HIP)
  return atomicXor((int32_t*)p, x);
#else
  return atomic_xor(p, x);
#endif
}

// End of atomics.h
