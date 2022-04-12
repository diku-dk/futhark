// TODO(pema): Error handling

#define __ATOMIC_RELAXED 0

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

static inline uniform int lexical_realloc_ispc(unsigned char uniform * uniform * uniform ptr,
                                        int64_t uniform * uniform old_size,
                                        uniform int64_t new_size) {
  if (*ptr != NULL) free(*ptr);
  unsigned char uniform * uniform alloc = uniform new uchar[new_size];
  *ptr = alloc;
  *old_size = new_size;
  return FUTHARK_SUCCESS;
}

static inline uniform int lexical_realloc_ispc(unsigned char uniform * uniform * uniform ptr,
                                        int64_t uniform * uniform old_size,
                                        varying int64_t new_size) {
  return lexical_realloc_ispc(ptr, old_size, extract(new_size, 0));
}

static inline uniform int lexical_realloc_ispc(unsigned char uniform * varying * uniform ptr,
                                        int64_t uniform * varying old_size,
                                        varying int64_t new_size) {
  if (*ptr != NULL) free(*ptr);
  unsigned char* alloc = new uchar[new_size];
  *ptr = alloc;
  *old_size = new_size;
  return FUTHARK_SUCCESS;
}

static inline uniform int lexical_realloc_ispc(unsigned char uniform * varying * uniform ptr,
                                        int64_t varying * uniform old_size,
                                        varying int64_t new_size) {
  return lexical_realloc_ispc(ptr, (uniform int64_t * varying)old_size, new_size);
}

static inline uniform int lexical_realloc_ispc(unsigned char uniform * varying * uniform ptr,
                                        size_t varying * uniform old_size,
                                        varying int64_t new_size) {
  if (*ptr != NULL) free(*ptr);
  unsigned char* alloc = new uchar[new_size];
  *ptr = alloc;
  *old_size = new_size;
  return FUTHARK_SUCCESS;
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
  uniform struct memblock aoss[programCount];
  aoss[programIndex] = _lhs;

  foreach_active(i){
    err |= memblock_unref(ctx,
		   &aoss[i],
		   lhs_desc);
  }

  *lhs = aoss[programIndex];

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
  uniform struct memblock aoss[programCount];
  aoss[programIndex] = _block;

  foreach_active(i){
    err |= memblock_alloc(ctx, &aoss[i], size, block_desc);
  }
  *block = aoss[programIndex];

  return err;
}

static uniform int memblock_alloc(uniform struct futhark_context * uniform ctx,
				                          varying struct memblock * uniform block,
				                          varying int64_t size,
                                  uniform const char * uniform block_desc) {
  uniform int err = 0;

  varying struct memblock _block = *block;
  uniform struct memblock aoss[programCount];
  aoss[programIndex] = _block;
  foreach_active(i){
    err |= memblock_alloc(ctx, &aoss[i], extract(size, i), block_desc);
  }
  *block = aoss[programIndex];

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
  uniform struct memblock aoss1[programCount];
  aoss1[programIndex] = _lhs;

  uniform struct memblock aoss2[programCount];
  aoss2[programIndex] = _rhs;

  foreach_active(i) {
      err |= memblock_set(ctx,
      &aoss1[i],
      &aoss2[i],
      lhs_desc);
  }
  *lhs = aoss1[programIndex];
  *rhs = aoss2[programIndex];

  return err;
}

static uniform int memblock_set (uniform struct futhark_context * uniform ctx,
                                 varying struct memblock * uniform lhs,
                                 uniform struct memblock * uniform rhs,
                                 uniform const char * uniform lhs_desc) {
  uniform int err = 0;

  varying struct memblock _lhs = *lhs;
  uniform struct memblock aoss1[programCount];
  aoss1[programIndex] = _lhs;

  foreach_active(i) {
      err |= memblock_set(ctx,
      &aoss1[i],
      rhs,
      lhs_desc);
  }
  *lhs = aoss1[programIndex];

  return err;
}


extern "C" unmasked uniform int atomic_fetch_add_int32(uniform int *, uniform int arg, uniform int dummy);
extern "C" unmasked uniform int atomic_fetch_sub_int32(uniform int *, uniform int arg, uniform int dummy);
extern "C" unmasked uniform int atomic_fetch_and_int32(uniform int *, uniform int arg, uniform int dummy);
extern "C" unmasked uniform int  atomic_fetch_or_int32(uniform int *, uniform int arg, uniform int dummy);
extern "C" unmasked uniform int atomic_fetch_xor_int32(uniform int *, uniform int arg, uniform int dummy);

uniform int __atomic_fetch_add(uniform int * uniform obj, uniform int arg, uniform int dummy){
  return atomic_fetch_add_int32(obj,arg,dummy);
}
uniform int __atomic_fetch_sub(uniform int * uniform obj, uniform int arg, uniform int dummy){
  return atomic_fetch_sub_int32(obj,arg,dummy);
}
uniform int __atomic_fetch_and(uniform int * uniform obj, uniform int arg, uniform int dummy){
  return atomic_fetch_and_int32(obj,arg,dummy);
}
uniform int __atomic_fetch_or(uniform int  * uniform obj, uniform int arg, uniform int dummy){
  return atomic_fetch_or_int32(obj,arg,dummy);
}
uniform int __atomic_fetch_xor(uniform int * uniform obj, uniform int arg, uniform int dummy){
  return atomic_fetch_xor_int32(obj,arg,dummy);
}

varying int __atomic_fetch_add(uniform int * obj, int arg, uniform int dummy){
  int temp = *obj;
  foreach_active(i){
    atomic_fetch_add_int32((int *)extract((int64)obj,i), extract(arg,i), dummy);
  }
  return temp;
}
varying int __atomic_fetch_sub(uniform int * obj, int arg, uniform int dummy){
  int temp = *obj;
  foreach_active(i){
    atomic_fetch_sub_int32((int *)extract((int64)obj,i), extract(arg,i), dummy);
  }
  return temp;
}
varying int __atomic_fetch_and(uniform int * obj, int arg, uniform int dummy){
  int temp = *obj;
  foreach_active(i){
    atomic_fetch_and_int32((int *)extract((int64)obj,i), extract(arg,i), dummy);
  }
  return temp;
}
varying int __atomic_fetch_or(uniform int  * obj, int arg, uniform int dummy){
  int temp = *obj;
  foreach_active(i){
    atomic_fetch_or_int32((int *)extract((int64)obj,i), extract(arg,i), dummy);
  }
  return temp;
}
varying int __atomic_fetch_xor(uniform int * obj, int arg, uniform int dummy){
  int temp = *obj;
  foreach_active(i){
    atomic_fetch_xor_int32((int *)extract((int64)obj,i), extract(arg,i), dummy);
  }
  return temp;
}


