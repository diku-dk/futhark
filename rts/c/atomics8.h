// Start of atomics8.h

SCALAR_FUN_ATTR int8_t atomic_cmpxchg_i8_global(volatile __global int8_t *p,
                                                int8_t cmp, int8_t val);
SCALAR_FUN_ATTR int8_t atomic_cmpxchg_i8_shared(volatile __local int8_t *p,
                                                int8_t cmp, int8_t val);
SCALAR_FUN_ATTR int8_t atomic_add_i8_global(volatile __global int8_t *p, int8_t x);
SCALAR_FUN_ATTR int8_t atomic_add_i8_shared(volatile __local int8_t *p, int8_t x);
SCALAR_FUN_ATTR int8_t atomic_smax_i8_global(volatile __global int8_t *p, int8_t x);
SCALAR_FUN_ATTR int8_t atomic_smax_i8_shared(volatile __local int8_t *p, int8_t x);
SCALAR_FUN_ATTR int8_t atomic_smin_i8_global(volatile __global int8_t *p, int8_t x);
SCALAR_FUN_ATTR int8_t atomic_smin_i8_shared(volatile __local int8_t *p, int8_t x);
SCALAR_FUN_ATTR uint8_t atomic_umax_i8_global(volatile __global uint8_t *p, uint8_t x);
SCALAR_FUN_ATTR uint8_t atomic_umax_i8_shared(volatile __local uint8_t *p, uint8_t x);
SCALAR_FUN_ATTR uint8_t atomic_umin_i8_global(volatile __global uint8_t *p, uint8_t x);
SCALAR_FUN_ATTR uint8_t atomic_umin_i8_shared(volatile __local uint8_t *p, uint8_t x);
SCALAR_FUN_ATTR int8_t atomic_and_i8_global(volatile __global int8_t *p, int8_t x);
SCALAR_FUN_ATTR int8_t atomic_and_i8_shared(volatile __local int8_t *p, int8_t x);
SCALAR_FUN_ATTR int8_t atomic_or_i8_global(volatile __global int8_t *p, int8_t x);
SCALAR_FUN_ATTR int8_t atomic_or_i8_shared(volatile __local int8_t *p, int8_t x);
SCALAR_FUN_ATTR int8_t atomic_xor_i8_global(volatile __global int8_t *p, int8_t x);
SCALAR_FUN_ATTR int8_t atomic_xor_i8_shared(volatile __local int8_t *p, int8_t x);

SCALAR_FUN_ATTR int8_t atomic_cmpxchg_i8_global(volatile __global int8_t *p,
                                                int8_t cmp, int8_t val) {
  int offset = ((uintptr_t)p & 3);
  volatile __global int32_t *p32 = (volatile __global int32_t*)((uintptr_t)p & ~0x3);

  int shift = offset * 8;
  int32_t mask = 0xff << shift;
  int32_t shifted_val = val << shift;
  int32_t shifted_cmp = cmp << shift;

  uint32_t old = shifted_cmp;
  uint32_t upd = shifted_val;
  uint32_t got;

  while ((got=atomic_cmpxchg_i32_global(p32, old, upd)) != old) {
    old = got;
    upd = (old & ~mask) | shifted_val;
  }

  return old >> shift;
}

SCALAR_FUN_ATTR int8_t atomic_cmpxchg_i8_shared(volatile __local int8_t *p,
                                                int8_t cmp, int8_t val) {
  int offset = ((uintptr_t)p >> 1 & 3);
  volatile __local int32_t *p32 = (volatile __local int32_t*)((uintptr_t)p & ~0x3);

  int shift = offset * 8;
  int32_t mask = 0xff << shift;
  int32_t shifted_val = val << shift;
  int32_t shifted_cmp = cmp << shift;

  uint32_t old = shifted_cmp;
  uint32_t upd = shifted_val;
  uint32_t got;

  while ((got=atomic_cmpxchg_i32_shared(p32, old, upd)) != old) {
    old = got;
    upd = (old & ~mask) | shifted_val;
  }

  return old >> shift;
}

// Convenience macro for arithmetic.
#define DEFINE_8BIT_ATOMIC(name, T, op)                                 \
  SCALAR_FUN_ATTR T                                                     \
  atomic_##name##_i8_global(volatile __global T *p, T val) {            \
    int offset = ((uintptr_t)p & 3);                                    \
    volatile __global int32_t *p32 = (volatile __global int32_t*)((uintptr_t)p & ~0x3); \
    int shift = offset * 8;                                             \
    int32_t mask = 0xff << shift;                                     \
    int32_t old = 0;                                                    \
    int32_t upd = (old & ~mask) | mask & (op(old >> shift, val) << shift); \
    int32_t saw;                                                        \
    while ((saw=atomic_cmpxchg_i32_global(p32, old, upd)) != old) {     \
      old = saw;                                                        \
      upd = (old & ~mask) | ((op(old >> shift, val)) << shift);         \
    }                                                                   \
    return old >> shift;                                                \
  }                                                                     \
  SCALAR_FUN_ATTR T                                                     \
  atomic_##name##_i8_shared(volatile __local T *p, T val) {             \
    int offset = ((uintptr_t)p & 3);                                    \
    volatile __local int32_t *p32 = (volatile __local int32_t*)((uintptr_t)p & ~0x3); \
    int shift = offset * 8;                                             \
    int32_t mask = 0xff << shift;                                     \
    int32_t old = 0;                                                    \
    int32_t upd = (old & ~mask) | mask & ((op(old >> shift, val)) << shift); \
    int32_t saw;                                                        \
    while ((saw=atomic_cmpxchg_i32_shared(p32, old, upd)) != old) {     \
      old = saw;                                                        \
      upd = (old & ~mask) | ((op(old >> shift, val)) << shift);         \
    }                                                                   \
    return old >> shift;                                                \
  }

DEFINE_8BIT_ATOMIC(add, int8_t, add8);
DEFINE_8BIT_ATOMIC(smax, int8_t, smax8);
DEFINE_8BIT_ATOMIC(smin, int8_t, smin8);
DEFINE_8BIT_ATOMIC(umax, uint8_t, umax8);
DEFINE_8BIT_ATOMIC(umin, uint8_t, umin8);

SCALAR_FUN_ATTR int8_t atomic_and_i8_global(volatile __global int8_t *p, int8_t val) {
  volatile __global int32_t *p32 = (volatile __global int32_t*)((uintptr_t)p & ~0x3);
  int shift = ((uintptr_t)p & 3) * 8;
  int32_t mask = 0xff << shift;
  return atomic_and_i32_global(p32, ~mask | (val<<shift)) >> shift;
}

SCALAR_FUN_ATTR int8_t atomic_and_i8_shared(volatile __local int8_t *p, int8_t val) {
  volatile __local int32_t *p32 = (volatile __local int32_t*)((uintptr_t)p & ~0x3);
  int shift = ((uintptr_t)p & 3) * 8;
  int32_t mask = 0xff << shift;
  return atomic_and_i32_shared(p32, ~mask | (val<<shift)) >> shift;
}

SCALAR_FUN_ATTR int8_t atomic_or_i8_global(volatile __global int8_t *p, int8_t val) {
  volatile __global int32_t *p32 = (volatile __global int32_t*)((uintptr_t)p & ~0x3);
  int shift = ((uintptr_t)p & 3) * 8;
  return atomic_or_i32_global(p32, (uint8_t)val<<shift) >> shift;
}

SCALAR_FUN_ATTR int8_t atomic_or_i8_shared(volatile __local int8_t *p, int8_t val) {
  volatile __local int32_t *p32 = (volatile __local int32_t*)((uintptr_t)p & ~0x3);
  int shift = ((uintptr_t)p & 3) * 8;
  return atomic_or_i32_shared(p32, (uint8_t)val<<shift) >> shift;
}

SCALAR_FUN_ATTR int8_t atomic_xor_i8_global(volatile __global int8_t *p, int8_t val) {
  volatile __global int32_t *p32 = (volatile __global int32_t*)((uintptr_t)p & ~0x3);
  int shift = ((uintptr_t)p & 3) * 8;
  return atomic_xor_i32_global(p32, (uint8_t)val<<shift) >> shift;
}

SCALAR_FUN_ATTR int8_t atomic_xor_i8_shared(volatile __local int8_t *p, int8_t val) {
  volatile __local int32_t *p32 = (volatile __local int32_t*)((uintptr_t)p & ~0x3);
  int shift = ((uintptr_t)p & 3) * 8;
  return atomic_xor_i32_shared(p32, (uint8_t)val<<shift) >> shift;
}

// End of atomics8.h
