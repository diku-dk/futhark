// Start of atomics16.h

SCALAR_FUN_ATTR int16_t atomic_cmpxchg_i16_global(volatile __global int16_t *p,
                                                  int16_t cmp, int16_t val);
SCALAR_FUN_ATTR int16_t atomic_cmpxchg_i16_shared(volatile __local int16_t *p,
                                                  int16_t cmp, int16_t val);
SCALAR_FUN_ATTR int16_t atomic_add_i16_global(volatile __global int16_t *p, int16_t x);
SCALAR_FUN_ATTR int16_t atomic_add_i16_shared(volatile __local int16_t *p, int16_t x);
SCALAR_FUN_ATTR int16_t atomic_smax_i16_global(volatile __global int16_t *p, int16_t x);
SCALAR_FUN_ATTR int16_t atomic_smax_i16_shared(volatile __local int16_t *p, int16_t x);
SCALAR_FUN_ATTR int16_t atomic_smin_i16_global(volatile __global int16_t *p, int16_t x);
SCALAR_FUN_ATTR int16_t atomic_smin_i16_shared(volatile __local int16_t *p, int16_t x);
SCALAR_FUN_ATTR uint16_t atomic_umax_i16_global(volatile __global uint16_t *p, uint16_t x);
SCALAR_FUN_ATTR uint16_t atomic_umax_i16_shared(volatile __local uint16_t *p, uint16_t x);
SCALAR_FUN_ATTR uint16_t atomic_umin_i16_global(volatile __global uint16_t *p, uint16_t x);
SCALAR_FUN_ATTR uint16_t atomic_umin_i16_shared(volatile __local uint16_t *p, uint16_t x);
SCALAR_FUN_ATTR int16_t atomic_and_i16_global(volatile __global int16_t *p, int16_t x);
SCALAR_FUN_ATTR int16_t atomic_and_i16_shared(volatile __local int16_t *p, int16_t x);
SCALAR_FUN_ATTR int16_t atomic_or_i16_global(volatile __global int16_t *p, int16_t x);
SCALAR_FUN_ATTR int16_t atomic_or_i16_shared(volatile __local int16_t *p, int16_t x);
SCALAR_FUN_ATTR int16_t atomic_xor_i16_global(volatile __global int16_t *p, int16_t x);
SCALAR_FUN_ATTR int16_t atomic_xor_i16_shared(volatile __local int16_t *p, int16_t x);

SCALAR_FUN_ATTR int16_t atomic_cmpxchg_i16_global(volatile __global int16_t *p,
                                                  int16_t cmp, int16_t val) {
  int offset = ((uintptr_t)p >> 1 & 1);
  volatile __global int32_t *p32 = (volatile __global int32_t*)((uintptr_t)p & ~0x3);

  int shift = offset * 16;
  int32_t mask = 0xffff << shift;
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

SCALAR_FUN_ATTR int16_t atomic_cmpxchg_i16_shared(volatile __local int16_t *p,
                                                  int16_t cmp, int16_t val) {
  int offset = ((uintptr_t)p >> 1 & 1);
  volatile __local int32_t *p32 = (volatile __local int32_t*)((uintptr_t)p & ~0x3);

  int shift = offset * 16;
  int32_t mask = 0xffff << shift;
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
#define DEFINE_16BIT_ATOMIC(name, T, op)                                \
  SCALAR_FUN_ATTR T                                                     \
  atomic_##name##_i16_global(volatile __global T *p, T val) {           \
    int offset = ((uintptr_t)p >> 1 & 1);                               \
    volatile __global int32_t *p32 = (volatile __global int32_t*)((uintptr_t)p & ~0x3); \
    int shift = offset * 16;                                            \
    int32_t mask = 0xffff << shift;                                     \
    int32_t shifted_val = val << shift;                                 \
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
  atomic_##name##_i16_shared(volatile __local T *p, T val) {            \
    int offset = ((uintptr_t)p >> 1 & 1);                               \
    volatile __local int32_t *p32 = (volatile __local int32_t*)((uintptr_t)p & ~0x3); \
    int shift = offset * 16;                                            \
    int32_t mask = 0xffff << shift;                                     \
    int32_t shifted_val = val << shift;                                 \
    int32_t old = 0;                                                    \
    int32_t upd = (old & ~mask) | mask & ((op(old >> shift, val)) << shift); \
    int32_t saw;                                                        \
    while ((saw=atomic_cmpxchg_i32_shared(p32, old, upd)) != old) {     \
      old = saw;                                                        \
      upd = (old & ~mask) | ((op(old >> shift, val)) << shift);         \
    }                                                                   \
    return old >> shift;                                                \
  }

DEFINE_16BIT_ATOMIC(add, int16_t, add16);
DEFINE_16BIT_ATOMIC(smax, int16_t, smax16);
DEFINE_16BIT_ATOMIC(smin, int16_t, smin16);
DEFINE_16BIT_ATOMIC(umax, uint16_t, umax16);
DEFINE_16BIT_ATOMIC(umin, uint16_t, umin16);

SCALAR_FUN_ATTR int16_t atomic_and_i16_global(volatile __global int16_t *p, int16_t val) {
  volatile __global int32_t *p32 = (volatile __global int32_t*)((uintptr_t)p & ~0x3);
  int shift = ((uintptr_t)p >> 1 & 1) * 16;
  int32_t mask = 0xffff << shift;
  return atomic_and_i32_global(p32, ~mask | (val<<shift)) >> shift;
}

SCALAR_FUN_ATTR int16_t atomic_and_i16_shared(volatile __local int16_t *p, int16_t val) {
  volatile __local int32_t *p32 = (volatile __local int32_t*)((uintptr_t)p & ~0x3);
  int shift = ((uintptr_t)p >> 1 & 1) * 16;
  int32_t mask = 0xffff << shift;
  return atomic_and_i32_shared(p32, ~mask | (val<<shift)) >> shift;
}

SCALAR_FUN_ATTR int16_t atomic_or_i16_global(volatile __global int16_t *p, int16_t val) {
  volatile __global int32_t *p32 = (volatile __global int32_t*)((uintptr_t)p & ~0x3);
  int shift = ((uintptr_t)p >> 1 & 1) * 16;
  return atomic_or_i32_global(p32, (uint16_t)val<<shift) >> shift;
}

SCALAR_FUN_ATTR int16_t atomic_or_i16_shared(volatile __local int16_t *p, int16_t val) {
  volatile __local int32_t *p32 = (volatile __local int32_t*)((uintptr_t)p & ~0x3);
  int shift = ((uintptr_t)p >> 1 & 1) * 16;
  return atomic_or_i32_shared(p32, (uint16_t)val<<shift) >> shift;
}

SCALAR_FUN_ATTR int16_t atomic_xor_i16_global(volatile __global int16_t *p, int16_t val) {
  volatile __global int32_t *p32 = (volatile __global int32_t*)((uintptr_t)p & ~0x3);
  int shift = ((uintptr_t)p >> 1 & 1) * 16;
  return atomic_xor_i32_global(p32, (uint16_t)val<<shift) >> shift;
}

SCALAR_FUN_ATTR int16_t atomic_xor_i16_shared(volatile __local int16_t *p, int16_t val) {
  volatile __local int32_t *p32 = (volatile __local int32_t*)((uintptr_t)p & ~0x3);
  int shift = ((uintptr_t)p >> 1 & 1) * 16;
  return atomic_xor_i32_shared(p32, (uint16_t)val<<shift) >> shift;
}


// End of atomics16.h
