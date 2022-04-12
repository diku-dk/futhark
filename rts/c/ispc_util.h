// TODO(pema): Error handling

#ifndef __ISPC_STRUCT_memblock__
#define __ISPC_STRUCT_memblock__
struct memblock {
    int32_t * references;
    uint8_t * mem;
    int64_t size;
    const int8_t * desc;
};
#endif



typedef unsigned char uchar;

static inline void free(void* ptr) {
  delete ptr;
}

static inline void free(void* uniform ptr) {
  if (programIndex == 0) {
    delete ptr;
  }
}

extern "C" unmasked uniform unsigned char * uniform realloc_ispc(uniform unsigned char * uniform ptr, uniform int64_t new_size);
extern "C" unmasked uniform char * uniform msgprintf_ispc(uniform int64_t new_size);
extern "C" unmasked uniform char * uniform msgprintf_ispc2(uniform int64_t new_size);
extern "C" unmasked uniform char * uniform * uniform futhark_get_error_ref(uniform struct futhark_context * uniform ctx);

static inline uniform int lexical_realloc_ispc(uniform char * uniform * uniform error,
                                        unsigned char uniform * uniform * uniform ptr,
                                        int64_t uniform * uniform old_size,
                                        uniform int64_t new_size) {
  uniform unsigned char * uniform memptr = realloc_ispc(*ptr, new_size);
  if (memptr == NULL) {
    *error = msgprintf_ispc(new_size);
    return FUTHARK_OUT_OF_MEMORY;
  } else {
    *ptr = memptr;
    *old_size = new_size;
    return FUTHARK_SUCCESS;
  }
}

static inline uniform int lexical_realloc_ispc(uniform char * uniform * uniform error,
                                        unsigned char uniform * uniform * uniform ptr,
                                        int64_t uniform * uniform old_size,
                                        varying int64_t new_size) {
  return lexical_realloc_ispc(error, ptr, old_size, reduce_max(new_size));
}

static inline uniform int lexical_realloc_ispc(uniform char * uniform * uniform error,
                                        unsigned char uniform * varying * uniform ptr,
                                        int64_t uniform * varying old_size,
                                        varying int64_t new_size) {
  uniform int err = FUTHARK_SUCCESS;
  foreach_active(i){
    uniform unsigned char * uniform memptr = realloc_ispc((uniform unsigned char * uniform)extract((int64_t)*ptr,i), 
                                                       extract(new_size,i));
    if (memptr == NULL) {
      *error = msgprintf_ispc(extract(new_size,i));
      err = FUTHARK_OUT_OF_MEMORY;
    } else {
      *ptr = (uniform unsigned char * varying)insert((int64_t)*ptr, i, (uniform int64_t) memptr);
      *old_size = new_size;
    }
  }
  return err;
}

static inline uniform int lexical_realloc_ispc(uniform char * uniform * uniform error,
                                        unsigned char uniform * varying * uniform ptr,
                                        int64_t varying * uniform old_size,
                                        varying int64_t new_size) {
  uniform int err = FUTHARK_SUCCESS;
  foreach_active(i){
    uniform unsigned char * uniform memptr = realloc_ispc((uniform unsigned char * uniform)extract((int64_t)*ptr,i), 
                                                       extract(new_size,i));
    if (memptr == NULL) {
      *error = msgprintf_ispc(extract(new_size,i));
      err = FUTHARK_OUT_OF_MEMORY;
    } else {
      *ptr = (uniform unsigned char * varying)insert((int64_t)*ptr, i, (uniform int64_t) memptr);
      *old_size = new_size;
    }
  }
  return err;
}

static inline uniform int lexical_realloc_ispc(uniform char * uniform * uniform error,
                                        unsigned char uniform * varying * uniform ptr,
                                        size_t varying * uniform old_size,
                                        varying int64_t new_size) {
  return lexical_realloc_ispc(error, ptr, (varying int64_t * uniform)old_size, new_size);
}

static inline uniform int lexical_realloc_ispc(uniform char * uniform * uniform error,
                                        unsigned char varying * uniform * uniform ptr,
                                        size_t varying * uniform old_size,
                                        uniform int64_t new_size) {
  //TODO(LOUIS): FIX
  //if (*ptr != NULL) free(*ptr);
  //varying unsigned char* uniform alloc = uniform new varying uchar[new_size];
  //*ptr = alloc;
  //*old_size = new_size;
  //return FUTHARK_SUCCESS;
  uniform int err = FUTHARK_SUCCESS;
  uniform unsigned char * uniform memptr = realloc_ispc((uniform unsigned char * uniform )*ptr, 
                                                        new_size*programCount);
  if (memptr == NULL) {
      *error = msgprintf_ispc(new_size);
      err = FUTHARK_OUT_OF_MEMORY;
  } else {
      *ptr = (varying unsigned char * uniform)memptr;
      *old_size = new_size;
  }
  
  return err;
}
extern "C" unmasked uniform unsigned char * uniform realloc_ispc(uniform unsigned char * uniform ptr, uniform int64_t new_size);
extern "C" unmasked uniform char * uniform msgprintf_ispc(uniform int64_t new_size);

static inline uniform int lexical_realloc_ispc(uniform char * uniform * uniform error,
                                        unsigned char varying * uniform * uniform ptr,
                                        size_t varying * uniform old_size,
                                        varying int64_t new_size) {
  return lexical_realloc_ispc(error, ptr, old_size, reduce_max(new_size));
}
extern "C" unmasked uniform int memblock_unref(uniform struct futhark_context * uniform ctx,
					                                     uniform struct memblock * uniform lhs,
					                                     uniform const char * uniform lhs_desc);

static uniform int memblock_unref(uniform struct futhark_context * varying ctx,
				                          uniform struct memblock * varying lhs,
                                  uniform const char * uniform lhs_desc)
{
  uniform int err = 0;

  foreach_active(i){
    err |= memblock_unref((uniform struct futhark_context * uniform)(extract((varying int64_t)ctx,i)),
		   (uniform struct memblock * uniform)(extract((varying int64_t)lhs,i)),
		   lhs_desc);
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
    err |= memblock_alloc((uniform struct futhark_context * uniform)(extract((varying int64_t)ctx,i)),
		   (uniform struct memblock * uniform)(extract((varying int64_t)block,i)),
		   extract(size, i),
		   block_desc);
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
