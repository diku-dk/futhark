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
#else
  union { int32_t i; float f; } old;
  union { int32_t i; float f; } assumed;
  old.f = *p;
  do {
    assumed.f = old.f;
    old.f = old.f + x;
    old.i = atomic_cmpxchg_i32_global((volatile __global int32_t*)p, assumed.i, old.i);
  } while (assumed.i != old.i);
  return old.f;
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

// Start of 64 bit atomics

#if defined(FUTHARK_CUDA) || defined(FUTHARK_HIP) || defined(cl_khr_int64_base_atomics) && defined(cl_khr_int64_extended_atomics)

SCALAR_FUN_ATTR int64_t atomic_xchg_i64_global(volatile __global int64_t *p, int64_t x);
SCALAR_FUN_ATTR int64_t atomic_xchg_i64_shared(volatile __local int64_t *p, int64_t x);
SCALAR_FUN_ATTR int64_t atomic_cmpxchg_i64_global(volatile __global int64_t *p,
                                                         int64_t cmp, int64_t val);
SCALAR_FUN_ATTR int64_t atomic_cmpxchg_i64_shared(volatile __local int64_t *p,
                                                        int64_t cmp, int64_t val);
SCALAR_FUN_ATTR int64_t atomic_add_i64_global(volatile __global int64_t *p, int64_t x);
SCALAR_FUN_ATTR int64_t atomic_add_i64_shared(volatile __local int64_t *p, int64_t x);
SCALAR_FUN_ATTR int64_t atomic_smax_i64_global(volatile __global int64_t *p, int64_t x);
SCALAR_FUN_ATTR int64_t atomic_smax_i64_shared(volatile __local int64_t *p, int64_t x);
SCALAR_FUN_ATTR int64_t atomic_smin_i64_global(volatile __global int64_t *p, int64_t x);
SCALAR_FUN_ATTR int64_t atomic_smin_i64_shared(volatile __local int64_t *p, int64_t x);
SCALAR_FUN_ATTR uint64_t atomic_umax_i64_global(volatile __global uint64_t *p, uint64_t x);
SCALAR_FUN_ATTR uint64_t atomic_umax_i64_shared(volatile __local uint64_t *p, uint64_t x);
SCALAR_FUN_ATTR uint64_t atomic_umin_i64_global(volatile __global uint64_t *p, uint64_t x);
SCALAR_FUN_ATTR uint64_t atomic_umin_i64_shared(volatile __local uint64_t *p, uint64_t x);
SCALAR_FUN_ATTR int64_t atomic_and_i64_global(volatile __global int64_t *p, int64_t x);
SCALAR_FUN_ATTR int64_t atomic_and_i64_shared(volatile __local int64_t *p, int64_t x);
SCALAR_FUN_ATTR int64_t atomic_or_i64_global(volatile __global int64_t *p, int64_t x);
SCALAR_FUN_ATTR int64_t atomic_or_i64_shared(volatile __local int64_t *p, int64_t x);
SCALAR_FUN_ATTR int64_t atomic_xor_i64_global(volatile __global int64_t *p, int64_t x);
SCALAR_FUN_ATTR int64_t atomic_xor_i64_shared(volatile __local int64_t *p, int64_t x);

#ifdef FUTHARK_F64_ENABLED
SCALAR_FUN_ATTR double atomic_fadd_f64_global(volatile __global double *p, double x);
SCALAR_FUN_ATTR double atomic_fadd_f64_shared(volatile __local double *p, double x);
#endif

SCALAR_FUN_ATTR int64_t atomic_xchg_i64_global(volatile __global int64_t *p, int64_t x) {
#if defined(FUTHARK_CUDA) || defined(FUTHARK_HIP)
  return atomicExch((uint64_t*)p, x);
#else
  return atom_xor(p, x);
#endif
}

SCALAR_FUN_ATTR int64_t atomic_xchg_i64_shared(volatile __local int64_t *p, int64_t x) {
#if defined(FUTHARK_CUDA) || defined(FUTHARK_HIP)
  return atomicExch((uint64_t*)p, x);
#else
  return atom_xor(p, x);
#endif
}

SCALAR_FUN_ATTR int64_t atomic_cmpxchg_i64_global(volatile __global int64_t *p,
                                                         int64_t cmp, int64_t val) {
#if defined(FUTHARK_CUDA) || defined(FUTHARK_HIP)
  return atomicCAS((uint64_t*)p, cmp, val);
#else
  return atom_cmpxchg(p, cmp, val);
#endif
}

SCALAR_FUN_ATTR int64_t atomic_cmpxchg_i64_shared(volatile __local int64_t *p,
                                                        int64_t cmp, int64_t val) {
#if defined(FUTHARK_CUDA) || defined(FUTHARK_HIP)
  return atomicCAS((uint64_t*)p, cmp, val);
#else
  return atom_cmpxchg(p, cmp, val);
#endif
}

SCALAR_FUN_ATTR int64_t atomic_add_i64_global(volatile __global int64_t *p, int64_t x) {
#if defined(FUTHARK_CUDA) || defined(FUTHARK_HIP)
  return atomicAdd((uint64_t*)p, x);
#else
  return atom_add(p, x);
#endif
}

SCALAR_FUN_ATTR int64_t atomic_add_i64_shared(volatile __local int64_t *p, int64_t x) {
#if defined(FUTHARK_CUDA) || defined(FUTHARK_HIP)
  return atomicAdd((uint64_t*)p, x);
#else
  return atom_add(p, x);
#endif
}

#ifdef FUTHARK_F64_ENABLED

SCALAR_FUN_ATTR double atomic_fadd_f64_global(volatile __global double *p, double x) {
#if defined(FUTHARK_CUDA) && __CUDA_ARCH__ >= 600 || defined(FUTHARK_HIP)
  return atomicAdd((double*)p, x);
#else
  union { int64_t i; double f; } old;
  union { int64_t i; double f; } assumed;
  old.f = *p;
  do {
    assumed.f = old.f;
    old.f = old.f + x;
    old.i = atomic_cmpxchg_i64_global((volatile __global int64_t*)p, assumed.i, old.i);
  } while (assumed.i != old.i);
  return old.f;
#endif
}

SCALAR_FUN_ATTR double atomic_fadd_f64_shared(volatile __local double *p, double x) {
#if defined(FUTHARK_CUDA) && __CUDA_ARCH__ >= 600 || defined(FUTHARK_HIP)
  return atomicAdd((double*)p, x);
#else
  union { int64_t i; double f; } old;
  union { int64_t i; double f; } assumed;
  old.f = *p;
  do {
    assumed.f = old.f;
    old.f = old.f + x;
    old.i = atomic_cmpxchg_i64_shared((volatile __local int64_t*)p, assumed.i, old.i);
  } while (assumed.i != old.i);
  return old.f;
#endif
}

#endif

SCALAR_FUN_ATTR int64_t atomic_smax_i64_global(volatile __global int64_t *p, int64_t x) {
#if defined(FUTHARK_CUDA)
  return atomicMax((int64_t*)p, x);
#elif defined(FUTHARK_HIP)
  // Currentely missing in HIP; probably a temporary oversight.
  int64_t old = *p, assumed;
  do {
    assumed = old;
    old = smax64(old, x);
    old = atomic_cmpxchg_i64_global((volatile __global int64_t*)p, assumed, old);
  } while (assumed != old);
  return old;
#else
  return atom_max(p, x);
#endif
}

SCALAR_FUN_ATTR int64_t atomic_smax_i64_shared(volatile __local int64_t *p, int64_t x) {
#if defined(FUTHARK_CUDA)
  return atomicMax((int64_t*)p, x);
#elif defined(FUTHARK_HIP)
  // Currentely missing in HIP; probably a temporary oversight.
  int64_t old = *p, assumed;
  do {
    assumed = old;
    old = smax64(old, x);
    old = atomic_cmpxchg_i64_shared((volatile __local int64_t*)p, assumed, old);
  } while (assumed != old);
  return old;
#else
  return atom_max(p, x);
#endif
}

SCALAR_FUN_ATTR int64_t atomic_smin_i64_global(volatile __global int64_t *p, int64_t x) {
#if defined(FUTHARK_CUDA)
  return atomicMin((int64_t*)p, x);
#elif defined(FUTHARK_HIP)
  // Currentely missing in HIP; probably a temporary oversight.
  int64_t old = *p, assumed;
  do {
    assumed = old;
    old = smin64(old, x);
    old = atomic_cmpxchg_i64_global((volatile __global int64_t*)p, assumed, old);
  } while (assumed != old);
  return old;
#else
  return atom_min(p, x);
#endif
}

SCALAR_FUN_ATTR int64_t atomic_smin_i64_shared(volatile __local int64_t *p, int64_t x) {
#if defined(FUTHARK_CUDA)
  return atomicMin((int64_t*)p, x);
#elif defined(FUTHARK_HIP)
  // Currentely missing in HIP; probably a temporary oversight.
  int64_t old = *p, assumed;
  do {
    assumed = old;
    old = smin64(old, x);
    old = atomic_cmpxchg_i64_shared((volatile __local int64_t*)p, assumed, old);
  } while (assumed != old);
  return old;
#else
  return atom_min(p, x);
#endif
}

SCALAR_FUN_ATTR uint64_t atomic_umax_i64_global(volatile __global uint64_t *p, uint64_t x) {
#if defined(FUTHARK_CUDA) || defined(FUTHARK_HIP)
  return atomicMax((uint64_t*)p, x);
#else
  return atom_max(p, x);
#endif
}

SCALAR_FUN_ATTR uint64_t atomic_umax_i64_shared(volatile __local uint64_t *p, uint64_t x) {
#if defined(FUTHARK_CUDA) || defined(FUTHARK_HIP)
  return atomicMax((uint64_t*)p, x);
#else
  return atom_max(p, x);
#endif
}

SCALAR_FUN_ATTR uint64_t atomic_umin_i64_global(volatile __global uint64_t *p, uint64_t x) {
#if defined(FUTHARK_CUDA) || defined(FUTHARK_HIP)
  return atomicMin((uint64_t*)p, x);
#else
  return atom_min(p, x);
#endif
}

SCALAR_FUN_ATTR uint64_t atomic_umin_i64_shared(volatile __local uint64_t *p, uint64_t x) {
#if defined(FUTHARK_CUDA) || defined(FUTHARK_HIP)
  return atomicMin((uint64_t*)p, x);
#else
  return atom_min(p, x);
#endif
}

SCALAR_FUN_ATTR int64_t atomic_and_i64_global(volatile __global int64_t *p, int64_t x) {
#if defined(FUTHARK_CUDA) || defined(FUTHARK_HIP)
  return atomicAnd((uint64_t*)p, x);
#else
  return atom_and(p, x);
#endif
}

SCALAR_FUN_ATTR int64_t atomic_and_i64_shared(volatile __local int64_t *p, int64_t x) {
#if defined(FUTHARK_CUDA) || defined(FUTHARK_HIP)
  return atomicAnd((uint64_t*)p, x);
#else
  return atom_and(p, x);
#endif
}

SCALAR_FUN_ATTR int64_t atomic_or_i64_global(volatile __global int64_t *p, int64_t x) {
#if defined(FUTHARK_CUDA) || defined(FUTHARK_HIP)
  return atomicOr((uint64_t*)p, x);
#else
  return atom_or(p, x);
#endif
}

SCALAR_FUN_ATTR int64_t atomic_or_i64_shared(volatile __local int64_t *p, int64_t x) {
#if defined(FUTHARK_CUDA) || defined(FUTHARK_HIP)
  return atomicOr((uint64_t*)p, x);
#else
  return atom_or(p, x);
#endif
}

SCALAR_FUN_ATTR int64_t atomic_xor_i64_global(volatile __global int64_t *p, int64_t x) {
#if defined(FUTHARK_CUDA) || defined(FUTHARK_HIP)
  return atomicXor((uint64_t*)p, x);
#else
  return atom_xor(p, x);
#endif
}

SCALAR_FUN_ATTR int64_t atomic_xor_i64_shared(volatile __local int64_t *p, int64_t x) {
#if defined(FUTHARK_CUDA) || defined(FUTHARK_HIP)
  return atomicXor((uint64_t*)p, x);
#else
  return atom_xor(p, x);
#endif
}

#endif // defined(FUTHARK_CUDA) || defined(FUTHARK_HIP) || defined(cl_khr_int64_base_atomics) && defined(cl_khr_int64_extended_atomics)

// End of atomics.h
