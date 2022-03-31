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
  foreach_active(i){
    memblock_unref((uniform struct futhark_context * uniform)(extract((varying int64_t)ctx,i)),
		   (uniform struct memblock * uniform)(extract((varying int64_t)lhs,i)),
		   lhs_desc);
  }
}
static uniform int memblock_unref(uniform struct futhark_context * uniform ctx,
				    varying struct memblock * uniform lhs,
				    uniform const char * uniform lhs_desc)
{
  uniform struct memblock * varying _lhs = (uniform struct memblock * varying) lhs;    
  foreach_active(i){
    memblock_unref(ctx,
		   (uniform struct memblock * uniform)(extract((varying int64_t)_lhs,i)),
		   lhs_desc);
  }
}

extern "C" unmasked int memblock_alloc(uniform struct futhark_context * uniform ctx,
				  uniform struct memblock * uniform block,
				  uniform int64_t size,
				  uniform const char * uniform block_desc);

static uniform int memblock_alloc(uniform struct futhark_context * varying ctx,
				  uniform struct memblock * varying block,
				  varying int64_t size,
				  uniform const char * uniform block_desc)
{
  foreach_active(i){
    memblock_alloc((uniform struct futhark_context * uniform)(extract((varying int64_t)ctx,i)),
		   (uniform struct memblock * uniform)(extract((varying int64_t)block,i)),
		   extract(size, i),
		   block_desc);
  }
}
static uniform int memblock_alloc(uniform struct futhark_context * uniform ctx,
				  varying struct memblock * uniform block,
				  uniform int64_t size,
				  uniform const char * uniform block_desc)
{
  // foreach_active(i){
  //   memblock_alloc((uniform struct futhark_context * uniform)(extract((varying int64_t)ctx,i)),
	// 	   (uniform struct memblock * uniform)(extract((varying int64_t)block,i)),
	// 	   extract(size, i),
	// 	   block_desc);
  // }
  uniform struct memblock * varying _block = (uniform struct memblock * varying) block;
  foreach_active(i){
    memblock_alloc(ctx,
		   (uniform struct memblock * uniform)(extract((varying int64_t)_block,i)),
		   size,
		   block_desc);
  }
}

// TODO (obp) : does this even make sense?
//static uniform int memblock_alloc(uniform struct futhark_context * uniform ctx,
//				  varying struct memblock * uniform block,
//				  uniform int64_t size,
//				  uniform const char * uniform block_desc)

// static int memblock_set (struct futhark_context *ctx, struct memblock *lhs, struct memblock *rhs, const char *lhs_desc) {
//   int ret = memblock_unref(ctx,  lhs, 0); //TODO(K, O): Make error handling
//   if (rhs->references != NULL) {
//     (*(rhs->references))++;
//   }
//   *lhs = *rhs;
//   return ret;
// }

extern "C" unmasked int memblock_set(uniform struct futhark_context * uniform ctx,
                                 uniform struct memblock * uniform lhs,
                                 uniform struct memblock * uniform rhs,
                                 uniform const char * uniform lhs_desc);

static uniform int memblock_set (uniform struct futhark_context * uniform ctx, 
                        varying struct memblock * uniform lhs,
                        varying struct memblock * uniform rhs,
                        uniform const char * uniform lhs_desc) {    
  uniform struct memblock * varying _lhs = (uniform struct memblock * varying) lhs;    
  uniform struct memblock * varying _rhs = (uniform struct memblock * varying) rhs;
  foreach_active(i) {
      memblock_set(ctx, 
      (uniform struct memblock * uniform)extract((varying int64_t ) _lhs, i), 
      (uniform struct memblock * uniform)extract((varying int64_t) _rhs, i),
        lhs_desc);
  }
}