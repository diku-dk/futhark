// Start of atomics16.h

SCALAR_FUN_ATTR int16_t atomic_add_i16_global(volatile __global int16_t *p, int16_t x);
SCALAR_FUN_ATTR int16_t atomic_add_i16_shared(volatile __local int16_t *p, int16_t x);

SCALAR_FUN_ATTR int16_t atomic_cmpxchg_i16_global(volatile __global int16_t *p,
                                                  int16_t cmp, int16_t val);
SCALAR_FUN_ATTR int16_t atomic_cmpxchg_i16_shared(volatile __local int16_t *p,
                                                  int16_t cmp, int16_t val);


SCALAR_FUN_ATTR int16_t atomic_add_i16_global(volatile __global int16_t *p, int16_t val) {
  int offset = ((uintptr_t)p >> 1 & 1);
  volatile __global int32_t *p32 = (volatile __global int32_t*)((uintptr_t)p & ~0x3);

  int shift = offset * 16;
  int32_t mask = 0xffff << shift;
  int32_t shifted_val = val << shift;

  int32_t old = *p32;
  int32_t new = (old & ~mask) | mask & (((old >> shift) + val) << shift);

  while (atomic_cmpxchg_i32_global(p32, old, new) != old) {
    old = *p32;
    new = (old & ~mask) | (((old >> shift) + val) << shift);
  }
  return old >> shift;
}

SCALAR_FUN_ATTR int16_t atomic_add_i16_shared(volatile __local int16_t *p, int16_t val) {
  int offset = ((uintptr_t)p >> 1 & 1);
  volatile __local int32_t *p32 = (volatile __local int32_t*)((uintptr_t)p & ~0x3);

  int shift = offset * 16;
  int32_t mask = 0xffff << shift;
  int32_t shifted_val = val << shift;

  int32_t old = *p32;
  int32_t new = (old & ~mask) | mask & (((old >> shift) + val) << shift);

  while (atomic_cmpxchg_i32_shared(p32, old, new) != old) {
    old = *p32;
    new = (old & ~mask) | (((old >> shift) + val) << shift);
  }
  return old >> shift;
}

SCALAR_FUN_ATTR int16_t atomic_cmpxchg_i16_global(volatile __global int16_t *p,
                                                  int16_t cmp, int16_t val) {
  int offset = ((uintptr_t)p >> 1 & 1);
  volatile __global int32_t *p32 = (volatile __global int32_t*)((uintptr_t)p & ~0x3);

  int shift = offset * 16;
  int32_t mask = 0xffff << shift;
  int32_t shifted_val = val << shift;
  int32_t shifted_cmp = cmp << shift;

  uint32_t old = (*p32 & ~mask) | shifted_cmp;
  uint32_t new = (old & ~mask) | shifted_val;

  while (atomic_cmpxchg_i32_global(p32, old, new) != old) {
    old = (*p32 & ~mask) | shifted_cmp;
    new = (old & ~mask) | shifted_val;
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

  uint32_t old = (*p32 & ~mask) | shifted_cmp;
  uint32_t new = (old & ~mask) | shifted_val;

  while (atomic_cmpxchg_i32_shared(p32, old, new) != old) {
    old = (*p32 & ~mask) | shifted_cmp;
    new = (old & ~mask) | shifted_val;
  }
  return old >> shift;
}

// End of atomics16.h
