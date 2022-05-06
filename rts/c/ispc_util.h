// Start of ispc_util.h.

// This header file implements various operations that are useful only when
// generating ISPC code. This includes wrappers for parts of Futhark's C runtime.

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
make_extract(int8* uniform)
make_extract(int16* uniform)
make_extract(int32* uniform)
make_extract(int64* uniform)
make_extract(uint8* uniform)
make_extract(uint16* uniform)
make_extract(uint32* uniform)
make_extract(uint64* uniform)
make_extract(float16* uniform)
make_extract(float* uniform)
make_extract(double* uniform)
make_extract(struct futhark_context)
make_extract(struct memblock)


// Handling of atomics
#define make_atomic_compare_exchange_wrapper(ty)                                     \
static inline uniform bool atomic_compare_exchange_wrapper(uniform ty * uniform mem, \
                               uniform ty * uniform old,                             \
                                                           const uniform ty val){    \
  uniform ty actual = atomic_compare_exchange_global(mem, *old, val);                \
  if (actual == *old){                                                               \
    return 1;                                                                        \
  }                                                                                  \
  *old = val;                                                                        \
  return 0;                                                                          \
}                                                                                    \
static inline varying bool atomic_compare_exchange_wrapper(uniform ty * varying mem, \
                              varying ty * uniform old,                              \
                              const varying ty val){                                 \
  varying ty actual = atomic_compare_exchange_global(mem, *old, val);                \
  if(actual == *old){                                                                \
    return 1;                                                                        \
  }                                                                                  \
  *old = val;                                                                        \
  return 0;                                                                          \
}

make_atomic_compare_exchange_wrapper(int32)
make_atomic_compare_exchange_wrapper(int64)
make_atomic_compare_exchange_wrapper(uint32)
make_atomic_compare_exchange_wrapper(uint64)
make_atomic_compare_exchange_wrapper(float)
make_atomic_compare_exchange_wrapper(double)

#define __atomic_fetch_add(x,y,z) atomic_add_global(x,y)
#define __atomic_fetch_sub(x,y,z) atomic_sub_global(x,y)
#define __atomic_fetch_and(x,y,z) atomic_and_global(x,y)
#define __atomic_fetch_or(x,y,z) atomic_or_global(x,y)
#define __atomic_fetch_xor(x,y,z) atomic_xor_global(x,y)
#define __atomic_exchange_n(x,y,z) atomic_swap_global(x,y)
#define __atomic_compare_exchange_n(x,y,z,h,j,k) atomic_compare_exchange_wrapper(x,y,z)


// Memory allocation handling
#ifndef __ISPC_STRUCT_memblock__
#define __ISPC_STRUCT_memblock__
struct memblock {
    int32_t * references;
    uint8_t * mem;
    int64_t size;
    const int8_t * desc;
};
#endif

static inline void free(void* ptr) {
  delete ptr;
}

static inline void free(void* uniform ptr) {
  delete ptr;
}

extern "C" unmasked uniform unsigned char * uniform realloc(uniform unsigned char * uniform ptr, uniform int64_t new_size);
extern "C" unmasked uniform char * uniform lexical_realloc_error(uniform int64_t new_size);
extern "C" unmasked uniform char * uniform * uniform futhark_context_get_error_ref(uniform struct futhark_context * uniform ctx);

static inline uniform int lexical_realloc(uniform char * uniform * uniform error,
                                          unsigned char uniform * uniform * uniform ptr,
                                          int64_t uniform * uniform old_size,
                                          uniform int64_t new_size) {
  uniform unsigned char * uniform memptr = realloc(*ptr, new_size);
  if (memptr == NULL) {
    *error = lexical_realloc_error(new_size);
    return FUTHARK_OUT_OF_MEMORY;
  } else {
    *ptr = memptr;
    *old_size = new_size;
    return FUTHARK_SUCCESS;
  }
}

static inline uniform int lexical_realloc(uniform char * uniform * uniform error,
                                          unsigned char uniform * uniform * uniform ptr,
                                          int64_t uniform * uniform old_size,
                                          varying int64_t new_size) {
  return lexical_realloc(error, ptr, old_size, reduce_max(new_size));
}

static inline uniform int lexical_realloc(uniform char * uniform * uniform error,
                                          unsigned char uniform * varying * uniform ptr,
                                          int64_t uniform * varying old_size,
                                          varying int64_t new_size) {
  uniform int err = FUTHARK_SUCCESS;
  foreach_active(i){
    uniform unsigned char * uniform memptr = realloc(extract(*ptr,i), extract(new_size,i));
    if (memptr == NULL) {
      *error = lexical_realloc_error(extract(new_size,i));
      err = FUTHARK_OUT_OF_MEMORY;
    } else {
      *ptr = (uniform unsigned char * varying)insert((int64_t)*ptr, i, (uniform int64_t) memptr);
      *old_size = new_size;
    }
  }
  return err;
}

static inline uniform int lexical_realloc(uniform char * uniform * uniform error,
                                          unsigned char uniform * varying * uniform ptr,
                                          int64_t varying * uniform old_size,
                                          varying int64_t new_size) {
  uniform int err = FUTHARK_SUCCESS;
  foreach_active(i){
    uniform unsigned char * uniform memptr = realloc(extract(*ptr,i), extract(new_size,i));
    if (memptr == NULL) {
      *error = lexical_realloc_error(extract(new_size,i));
      err = FUTHARK_OUT_OF_MEMORY;
    } else {
      *ptr = (uniform unsigned char * varying)insert((int64_t)*ptr, i, (uniform int64_t) memptr);
      *old_size = new_size;
    }
  }
  return err;
}

static inline uniform int lexical_realloc(uniform char * uniform * uniform error,
                                          unsigned char uniform * varying * uniform ptr,
                                          size_t varying * uniform old_size,
                                          varying int64_t new_size) {
  return lexical_realloc(error, ptr, (varying int64_t * uniform)old_size, new_size);
}

static inline uniform int lexical_realloc(uniform char * uniform * uniform error,
                                          unsigned char varying * uniform * uniform ptr,
                                          size_t varying * uniform old_size,
                                          uniform int64_t new_size) {
  uniform int err = FUTHARK_SUCCESS;
  uniform unsigned char * uniform memptr = realloc((uniform unsigned char * uniform )*ptr,
                                                        new_size*programCount);
  if (memptr == NULL) {
    *error = lexical_realloc_error(new_size);
    err = FUTHARK_OUT_OF_MEMORY;
  } else {
    *ptr = (varying unsigned char * uniform)memptr;
    *old_size = new_size;
  }

  return err;
}

static inline uniform int lexical_realloc(uniform char * uniform * uniform error,
                                          unsigned char varying * uniform * uniform ptr,
                                          size_t varying * uniform old_size,
                                          varying int64_t new_size) {
  return lexical_realloc(error, ptr, old_size, reduce_max(new_size));
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
