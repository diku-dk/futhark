// Start of atomics64.h

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
  return atomicExch((unsigned long long*)p, x);
#else
  return atom_xor(p, x);
#endif
}

SCALAR_FUN_ATTR int64_t atomic_xchg_i64_shared(volatile __local int64_t *p, int64_t x) {
#if defined(FUTHARK_CUDA) || defined(FUTHARK_HIP)
  return atomicExch((unsigned long long*)p, x);
#else
  return atom_xor(p, x);
#endif
}

SCALAR_FUN_ATTR int64_t atomic_cmpxchg_i64_global(volatile __global int64_t *p,
                                                         int64_t cmp, int64_t val) {
#if defined(FUTHARK_CUDA) || defined(FUTHARK_HIP)
  return atomicCAS((unsigned long long*)p, cmp, val);
#else
  return atom_cmpxchg(p, cmp, val);
#endif
}

SCALAR_FUN_ATTR int64_t atomic_cmpxchg_i64_shared(volatile __local int64_t *p,
                                                        int64_t cmp, int64_t val) {
#if defined(FUTHARK_CUDA) || defined(FUTHARK_HIP)
  return atomicCAS((unsigned long long*)p, cmp, val);
#else
  return atom_cmpxchg(p, cmp, val);
#endif
}

SCALAR_FUN_ATTR int64_t atomic_add_i64_global(volatile __global int64_t *p, int64_t x) {
#if defined(FUTHARK_CUDA) || defined(FUTHARK_HIP)
  return atomicAdd((unsigned long long*)p, x);
#else
  return atom_add(p, x);
#endif
}

SCALAR_FUN_ATTR int64_t atomic_add_i64_shared(volatile __local int64_t *p, int64_t x) {
#if defined(FUTHARK_CUDA) || defined(FUTHARK_HIP)
  return atomicAdd((unsigned long long*)p, x);
#else
  return atom_add(p, x);
#endif
}

#ifdef FUTHARK_F64_ENABLED

SCALAR_FUN_ATTR double atomic_fadd_f64_global(volatile __global double *p, double x) {
#if defined(FUTHARK_CUDA) && __CUDA_ARCH__ >= 600 || defined(FUTHARK_HIP)
  return atomicAdd((double*)p, x);
  // On OpenCL, use technique from
  // https://pipinspace.github.io/blog/atomic-float-addition-in-opencl.html
#elif defined(cl_nv_pragma_unroll)
  // use hardware-supported atomic addition on Nvidia GPUs with inline
  // PTX assembly
  double ret;
  asm volatile("atom.global.add.f64 %0,[%1],%2;":"=d"(ret):"l"(p),"d"(x):"memory");
  return ret;
#elif __has_builtin(__builtin_amdgcn_global_atomic_fadd_f64)
  // use hardware-supported atomic addition on some AMD GPUs
  return __builtin_amdgcn_global_atomic_fadd_f64(p, x);
#else
  // fallback emulation:
  // https://forums.developer.nvidia.com/t/atomicadd-float-float-atomicmul-float-float/14639/5
  union {int64_t i; double f;} old;
  union {int64_t i; double f;} ret;
  old.f = x;
  while (1) {
    ret.i = atom_xchg((volatile __global int64_t*)p, (int64_t)0);
    ret.f += old.f;
    old.i = atom_xchg((volatile __global int64_t*)p, ret.i);
    if (old.i == 0) {
      break;
    }
  }
  return ret.f;
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
  return atomicMax((long long*)p, x);
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
  return atomicMax((long long*)p, x);
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
  return atomicMin((long long*)p, x);
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
  return atomicMin((long long*)p, x);
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
  return atomicMax((unsigned long long*)p, x);
#else
  return atom_max(p, x);
#endif
}

SCALAR_FUN_ATTR uint64_t atomic_umax_i64_shared(volatile __local uint64_t *p, uint64_t x) {
#if defined(FUTHARK_CUDA) || defined(FUTHARK_HIP)
  return atomicMax((unsigned long long*)p, x);
#else
  return atom_max(p, x);
#endif
}

SCALAR_FUN_ATTR uint64_t atomic_umin_i64_global(volatile __global uint64_t *p, uint64_t x) {
#if defined(FUTHARK_CUDA) || defined(FUTHARK_HIP)
  return atomicMin((unsigned long long*)p, x);
#else
  return atom_min(p, x);
#endif
}

SCALAR_FUN_ATTR uint64_t atomic_umin_i64_shared(volatile __local uint64_t *p, uint64_t x) {
#if defined(FUTHARK_CUDA) || defined(FUTHARK_HIP)
  return atomicMin((unsigned long long*)p, x);
#else
  return atom_min(p, x);
#endif
}

SCALAR_FUN_ATTR int64_t atomic_and_i64_global(volatile __global int64_t *p, int64_t x) {
#if defined(FUTHARK_CUDA) || defined(FUTHARK_HIP)
  return atomicAnd((unsigned long long*)p, x);
#else
  return atom_and(p, x);
#endif
}

SCALAR_FUN_ATTR int64_t atomic_and_i64_shared(volatile __local int64_t *p, int64_t x) {
#if defined(FUTHARK_CUDA) || defined(FUTHARK_HIP)
  return atomicAnd((unsigned long long*)p, x);
#else
  return atom_and(p, x);
#endif
}

SCALAR_FUN_ATTR int64_t atomic_or_i64_global(volatile __global int64_t *p, int64_t x) {
#if defined(FUTHARK_CUDA) || defined(FUTHARK_HIP)
  return atomicOr((unsigned long long*)p, x);
#else
  return atom_or(p, x);
#endif
}

SCALAR_FUN_ATTR int64_t atomic_or_i64_shared(volatile __local int64_t *p, int64_t x) {
#if defined(FUTHARK_CUDA) || defined(FUTHARK_HIP)
  return atomicOr((unsigned long long*)p, x);
#else
  return atom_or(p, x);
#endif
}

SCALAR_FUN_ATTR int64_t atomic_xor_i64_global(volatile __global int64_t *p, int64_t x) {
#if defined(FUTHARK_CUDA) || defined(FUTHARK_HIP)
  return atomicXor((unsigned long long*)p, x);
#else
  return atom_xor(p, x);
#endif
}

SCALAR_FUN_ATTR int64_t atomic_xor_i64_shared(volatile __local int64_t *p, int64_t x) {
#if defined(FUTHARK_CUDA) || defined(FUTHARK_HIP)
  return atomicXor((unsigned long long*)p, x);
#else
  return atom_xor(p, x);
#endif
}

#endif // defined(FUTHARK_CUDA) || defined(FUTHARK_HIP) || defined(cl_khr_int64_base_atomics) && defined(cl_khr_int64_extended_atomics)

// End of atomics64.h
