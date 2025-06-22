// Start of ispc_util.h.

// This header file implements various operations that are useful only when
// generating ISPC code. This includes wrappers for parts of Futhark's C runtime.

// Expose gang size
export uniform int64_t get_gang_size() {
  return programCount;
}

// Generate missing overloads for extract on pointers
#define make_extract(ty)                                                                \
static inline uniform ty * uniform extract(uniform ty * varying ptr, uniform int idx) { \
    int64 c = (int64)ptr;                                                               \
    uniform int64 r = extract(c, idx);                                                  \
    return (uniform ty * uniform)r;                                                     \
}

make_extract(int8)
make_extract(int16)
make_extract(int32)
make_extract(int64)
make_extract(uint8)
make_extract(uint16)
make_extract(uint32)
make_extract(uint64)
make_extract(float16)
make_extract(float)
make_extract(double)
/* make_extract(int8* uniform) */
/* make_extract(int16* uniform) */
/* make_extract(int32* uniform) */
/* make_extract(int64* uniform) */
/* make_extract(uint8* uniform) */
/* make_extract(uint16* uniform) */
/* make_extract(uint32* uniform) */
/* make_extract(uint64* uniform) */
/* make_extract(float16* uniform) */
/* make_extract(float* uniform) */
/* make_extract(double* uniform) */
make_extract(struct futhark_context)
make_extract(struct memblock)


// Handling of atomics
// Atomic CAS acts differently in GCC and ISPC, so we emulate it.
#define make_atomic_compare_exchange_wrapper(ty)                                     \
static inline uniform bool atomic_compare_exchange_wrapper(uniform ty * uniform mem, \
                                                           uniform ty * uniform old, \
                                                           const uniform ty val){    \
  uniform ty actual = atomic_compare_exchange_global(mem, *old, val);                \
  if (actual == *old){                                                               \
    return 1;                                                                        \
  }                                                                                  \
  *old = actual;                                                                     \
  return 0;                                                                          \
}                                                                                    \
static inline varying bool atomic_compare_exchange_wrapper(uniform ty * varying mem, \
                                                           varying ty * uniform old, \
                                                           const varying ty val){    \
  varying ty actual = atomic_compare_exchange_global(mem, *old, val);                \
  bool res = 0;                                                                      \
  if(actual == *old){                                                                \
    res = 1;                                                                         \
  } else {                                                                           \
    *old = actual;                                                                   \
  }                                                                                  \
  return res;                                                                        \
}                                                                                    \
static inline varying bool atomic_compare_exchange_wrapper(varying ty * uniform mem, \
                                                           varying ty * uniform old, \
                                                           const varying ty val){    \
  uniform ty * uniform base_mem = (uniform ty * uniform)mem;                         \
  uniform ty * uniform base_old = (uniform ty * uniform)old;                         \
  bool res = 0;                                                                      \
  foreach_active (i) {                                                               \
    uniform ty * uniform curr_mem = base_mem + i;                                    \
    uniform ty * uniform curr_old = base_old + i;                                    \
    uniform ty curr_val = extract(val, i);                                           \
    uniform bool curr = atomic_compare_exchange_wrapper(                             \
                            curr_mem, curr_old, curr_val);                           \
    res = insert(res, i, curr);                                                      \
  }                                                                                  \
  return res;                                                                        \
}                                                                                    \
static inline uniform bool atomic_compare_exchange_wrapper(uniform ty * uniform mem, \
                                                           uniform ty * uniform old, \
                                                           const varying ty val){    \
  uniform ty v = 0;                                                                  \
  foreach_active (i) v = extract(val, i);                                            \
  return atomic_compare_exchange_wrapper(mem, old, v);                               \
}

make_atomic_compare_exchange_wrapper(int32)
make_atomic_compare_exchange_wrapper(int64)
make_atomic_compare_exchange_wrapper(uint32)
make_atomic_compare_exchange_wrapper(uint64)
make_atomic_compare_exchange_wrapper(float)
make_atomic_compare_exchange_wrapper(double)

// This code generates missing overloads for atomic operations on uniform
// pointers to varying values.
#define make_single_atomic(name, ty)                                        \
static inline ty atomic_##name##_global(varying ty * uniform mem, ty val) { \
  uniform ty * uniform base_mem = (uniform ty * uniform)mem;                \
  ty res = 0;                                                               \
  foreach_active (i) {                                                      \
    uniform ty * uniform curr_mem = base_mem + i;                           \
    uniform ty curr_val = extract(val, i);                                  \
    uniform ty curr = atomic_##name##_global(curr_mem, curr_val);           \
    res = insert(res, i, curr);                                             \
  }                                                                         \
  return res;                                                               \
}

#define make_all_atomic(name)    \
make_single_atomic(name, int32)  \
make_single_atomic(name, int64)  \
make_single_atomic(name, uint32) \
make_single_atomic(name, uint64)

make_all_atomic(add)
make_all_atomic(subtract)
make_all_atomic(and)
make_all_atomic(or)
make_all_atomic(xor)
make_all_atomic(swap)

// This is a hack to prevent literals (which have unbound variability)
// from causing us to pick the wrong overload for atomic operations.
static inline varying int32  make_varying(uniform int32  x) { return x; }
static inline varying int32  make_varying(varying int32  x) { return x; }
static inline varying int64  make_varying(uniform int64  x) { return x; }
static inline varying int64  make_varying(varying int64  x) { return x; }
static inline varying uint32 make_varying(uniform uint32 x) { return x; }
static inline varying uint32 make_varying(varying uint32 x) { return x; }
static inline varying uint64 make_varying(uniform uint64 x) { return x; }
static inline varying uint64 make_varying(varying uint64 x) { return x; }

// Redirect atomic operations to the relevant ISPC overloads.
#define __atomic_fetch_add(x,y,z) atomic_add_global(x,make_varying(y))
#define __atomic_fetch_sub(x,y,z) atomic_sub_global(x,make_varying(y))
#define __atomic_fetch_and(x,y,z) atomic_and_global(x,make_varying(y))
#define __atomic_fetch_or(x,y,z) atomic_or_global(x,make_varying(y))
#define __atomic_fetch_xor(x,y,z) atomic_xor_global(x,make_varying(y))
#define __atomic_exchange_n(x,y,z) atomic_swap_global(x,make_varying(y))
#define __atomic_compare_exchange_n(x,y,z,h,j,k) atomic_compare_exchange_wrapper(x,y,z)


// Memory allocation handling
struct memblock {
    int32_t * references;
    uint8_t * mem;
    int64_t size;
    const int8_t * desc;
};

static inline void free(void* ptr) {
  delete ptr;
}

static inline void free(void* uniform ptr) {
  delete ptr;
}

extern "C" unmasked uniform unsigned char * uniform realloc(uniform unsigned char * uniform ptr, uniform int64_t new_size);
extern "C" unmasked uniform char * uniform lexical_realloc_error(uniform struct futhark_context * uniform ctx, uniform int64_t new_size);

static inline uniform int lexical_realloc(uniform struct futhark_context * uniform ctx,
                                          unsigned char uniform * uniform * uniform ptr,
                                          int64_t uniform * uniform old_size,
                                          uniform int64_t new_size) {
  uniform unsigned char * uniform memptr = realloc(*ptr, new_size);
  if (memptr == NULL) {
    lexical_realloc_error(ctx, new_size);
    return FUTHARK_OUT_OF_MEMORY;
  } else {
    *ptr = memptr;
    *old_size = new_size;
    return FUTHARK_SUCCESS;
  }
}


static inline uniform int lexical_realloc(uniform struct futhark_context *ctx,
                                          unsigned char uniform * uniform * uniform ptr,
                                          int64_t uniform * uniform old_size,
                                          varying int64_t new_size) {
  return lexical_realloc(ctx, ptr, old_size, reduce_max(new_size));
}

static inline uniform int lexical_realloc(uniform struct futhark_context * uniform ctx,
                                          unsigned char uniform * varying * uniform ptr,
                                          int64_t uniform * varying old_size,
                                          varying int64_t new_size) {
  uniform int err = FUTHARK_SUCCESS;
  foreach_active(i){
    uniform unsigned char * uniform memptr = realloc(extract(*ptr,i), extract(new_size,i));
    if (memptr == NULL) {
      lexical_realloc_error(ctx, extract(new_size,i));
      err = FUTHARK_OUT_OF_MEMORY;
    } else {
      *ptr = (uniform unsigned char * varying)insert((int64_t)*ptr, i, (uniform int64_t) memptr);
      *old_size = new_size;
    }
  }
  return err;
}

static inline uniform int lexical_realloc(uniform struct futhark_context * uniform ctx,
                                          unsigned char uniform * varying * uniform ptr,
                                          int64_t varying * uniform old_size,
                                          varying int64_t new_size) {
  uniform int err = FUTHARK_SUCCESS;
  foreach_active(i){
    uniform unsigned char * uniform memptr = realloc(extract(*ptr,i), extract(new_size,i));
    if (memptr == NULL) {
      lexical_realloc_error(ctx, extract(new_size,i));
      err = FUTHARK_OUT_OF_MEMORY;
    } else {
      *ptr = (uniform unsigned char * varying)insert((int64_t)*ptr, i, (uniform int64_t) memptr);
      *old_size = new_size;
    }
  }
  return err;
}

static inline uniform int lexical_realloc(uniform struct futhark_context * uniform ctx,
                                          unsigned char uniform * varying * uniform ptr,
                                          size_t varying * uniform old_size,
                                          varying int64_t new_size) {
  return lexical_realloc(ctx, ptr, (varying int64_t * uniform)old_size, new_size);
}

static inline uniform int lexical_realloc(uniform struct futhark_context * uniform ctx,
                                          unsigned char varying * uniform * uniform ptr,
                                          size_t varying * uniform old_size,
                                          uniform int64_t new_size) {
  uniform int err = FUTHARK_SUCCESS;
  uniform unsigned char * uniform memptr = realloc((uniform unsigned char * uniform )*ptr,
                                                        new_size*programCount);
  if (memptr == NULL) {
    lexical_realloc_error(ctx, new_size);
    err = FUTHARK_OUT_OF_MEMORY;
  } else {
    *ptr = (varying unsigned char * uniform)memptr;
    *old_size = new_size;
  }

  return err;
}

static inline uniform int lexical_realloc(uniform struct futhark_context * uniform ctx,
                                          unsigned char varying * uniform * uniform ptr,
                                          size_t varying * uniform old_size,
                                          varying int64_t new_size) {
  return lexical_realloc(ctx, ptr, old_size, reduce_max(new_size));
}

extern "C" unmasked uniform int memblock_unref(uniform struct futhark_context * uniform ctx,
                                               uniform struct memblock * uniform lhs,
                                               uniform const char * uniform lhs_desc);

static uniform int memblock_unref(uniform struct futhark_context * varying ctx,
                                  uniform struct memblock * varying lhs,
                                  uniform const char * uniform lhs_desc)
{
  uniform int err = 0;

  foreach_active(i) {
    err |= memblock_unref(extract(ctx,i), extract(lhs,i), lhs_desc);
  }

  return err;
}
static uniform int memblock_unref(uniform struct futhark_context * uniform ctx,
                                  varying struct memblock * uniform lhs,
                                  uniform const char * uniform lhs_desc)
{
  uniform int err = 0;

  varying struct memblock _lhs = *lhs;
  uniform struct memblock aos[programCount];
  aos[programIndex] = _lhs;

  foreach_active(i){
    err |= memblock_unref(ctx,
           &aos[i],
           lhs_desc);
  }

  *lhs = aos[programIndex];

  return err;
}

extern "C" unmasked uniform int memblock_alloc(uniform struct futhark_context * uniform ctx,
                                               uniform struct memblock * uniform block,
                                               uniform int64_t size,
                                               uniform const char * uniform block_desc);

static uniform int memblock_alloc(uniform struct futhark_context * varying ctx,
                                  uniform struct memblock * varying block,
                                  varying int64_t size,
                                  uniform const char * uniform block_desc) {
  uniform int err = 0;

  foreach_active(i){
    err |= memblock_alloc(extract(ctx,i), extract(block,i), extract(size, i), block_desc);
  }

  return err;
}

static uniform int memblock_alloc(uniform struct futhark_context * uniform ctx,
                                  varying struct memblock * uniform block,
                                  uniform int64_t size,
                                  uniform const char * uniform block_desc) {
  uniform int err = 0;

  varying struct memblock _block = *block;
  uniform struct memblock aos[programCount];
  aos[programIndex] = _block;

  foreach_active(i){
    err |= memblock_alloc(ctx, &aos[i], size, block_desc);
  }
  *block = aos[programIndex];

  return err;
}

static uniform int memblock_alloc(uniform struct futhark_context * uniform ctx,
                                  varying struct memblock * uniform block,
                                  varying int64_t size,
                                  uniform const char * uniform block_desc) {
  uniform int err = 0;

  varying struct memblock _block = *block;
  uniform struct memblock aos[programCount];
  aos[programIndex] = _block;
  foreach_active(i){
    err |= memblock_alloc(ctx, &aos[i], extract(size, i), block_desc);
  }
  *block = aos[programIndex];

  return err;
}

extern "C" unmasked uniform int memblock_set(uniform struct futhark_context * uniform ctx,
                                             uniform struct memblock * uniform lhs,
                                             uniform struct memblock * uniform rhs,
                                             uniform const char * uniform lhs_desc);

static uniform int memblock_set (uniform struct futhark_context * uniform ctx,
                                 varying struct memblock * uniform lhs,
                                 varying struct memblock * uniform rhs,
                                 uniform const char * uniform lhs_desc) {
  uniform int err = 0;

  varying struct memblock _lhs = *lhs;
  varying struct memblock _rhs = *rhs;
  uniform struct memblock aos1[programCount];
  aos1[programIndex] = _lhs;

  uniform struct memblock aos2[programCount];
  aos2[programIndex] = _rhs;

  foreach_active(i) {
      err |= memblock_set(ctx,
      &aos1[i],
      &aos2[i],
      lhs_desc);
  }
  *lhs = aos1[programIndex];
  *rhs = aos2[programIndex];

  return err;
}

static uniform int memblock_set (uniform struct futhark_context * uniform ctx,
                                 varying struct memblock * uniform lhs,
                                 uniform struct memblock * uniform rhs,
                                 uniform const char * uniform lhs_desc) {
  uniform int err = 0;

  varying struct memblock _lhs = *lhs;
  uniform struct memblock aos1[programCount];
  aos1[programIndex] = _lhs;

  foreach_active(i) {
      err |= memblock_set(ctx,
      &aos1[i],
      rhs,
      lhs_desc);
  }
  *lhs = aos1[programIndex];

  return err;
}

// End of ispc_util.h.
