// Start of copy.cl

// The kernel parameters are given by macros, because Metal kernels
// take their arguments as a struct.
#ifdef FUTHARK_METAL
struct lmad_copy_args {
  device uint8_t *dst_mem;
  int64_t dst_offset;
  device uint8_t *src_mem;
  int64_t src_offset;
  int64_t n;
  int r;
  int64_t shape0, dst_stride0, src_stride0;
  int64_t shape1, dst_stride1, src_stride1;
  int64_t shape2, dst_stride2, src_stride2;
  int64_t shape3, dst_stride3, src_stride3;
  int64_t shape4, dst_stride4, src_stride4;
  int64_t shape5, dst_stride5, src_stride5;
  int64_t shape6, dst_stride6, src_stride6;
  int64_t shape7, dst_stride7, src_stride7;
};

#define COPY_PARAMS(ELEM_TYPE) FUTHARK_KERNEL_PARAMS(lmad_copy_args)

#define COPY_UNPACK(ELEM_TYPE)                                          \
  device ELEM_TYPE *dst_mem = (device ELEM_TYPE*)futhark_args->dst_mem; \
  int64_t dst_offset = futhark_args->dst_offset;                        \
  device ELEM_TYPE *src_mem = (device ELEM_TYPE*)futhark_args->src_mem; \
  int64_t src_offset = futhark_args->src_offset;                        \
  int64_t n = futhark_args->n;                                          \
  int r = futhark_args->r;                                              \
  int64_t shape0 = futhark_args->shape0;                                \
  int64_t dst_stride0 = futhark_args->dst_stride0;                      \
  int64_t src_stride0 = futhark_args->src_stride0;                      \
  int64_t shape1 = futhark_args->shape1;                                \
  int64_t dst_stride1 = futhark_args->dst_stride1;                      \
  int64_t src_stride1 = futhark_args->src_stride1;                      \
  int64_t shape2 = futhark_args->shape2;                                \
  int64_t dst_stride2 = futhark_args->dst_stride2;                      \
  int64_t src_stride2 = futhark_args->src_stride2;                      \
  int64_t shape3 = futhark_args->shape3;                                \
  int64_t dst_stride3 = futhark_args->dst_stride3;                      \
  int64_t src_stride3 = futhark_args->src_stride3;                      \
  int64_t shape4 = futhark_args->shape4;                                \
  int64_t dst_stride4 = futhark_args->dst_stride4;                      \
  int64_t src_stride4 = futhark_args->src_stride4;                      \
  int64_t shape5 = futhark_args->shape5;                                \
  int64_t dst_stride5 = futhark_args->dst_stride5;                      \
  int64_t src_stride5 = futhark_args->src_stride5;                      \
  int64_t shape6 = futhark_args->shape6;                                \
  int64_t dst_stride6 = futhark_args->dst_stride6;                      \
  int64_t src_stride6 = futhark_args->src_stride6;                      \
  int64_t shape7 = futhark_args->shape7;                                \
  int64_t dst_stride7 = futhark_args->dst_stride7;                      \
  int64_t src_stride7 = futhark_args->src_stride7;
#else
#define COPY_PARAMS(ELEM_TYPE)                                          \
  SHARED_MEM_PARAM                                                      \
  __global ELEM_TYPE *dst_mem,                                          \
  int64_t dst_offset,                                                   \
  __global ELEM_TYPE *src_mem,                                          \
  int64_t src_offset,                                                   \
  int64_t n,                                                            \
  int r,                                                                \
  int64_t shape0, int64_t dst_stride0, int64_t src_stride0,             \
  int64_t shape1, int64_t dst_stride1, int64_t src_stride1,             \
  int64_t shape2, int64_t dst_stride2, int64_t src_stride2,             \
  int64_t shape3, int64_t dst_stride3, int64_t src_stride3,             \
  int64_t shape4, int64_t dst_stride4, int64_t src_stride4,             \
  int64_t shape5, int64_t dst_stride5, int64_t src_stride5,             \
  int64_t shape6, int64_t dst_stride6, int64_t src_stride6,             \
  int64_t shape7, int64_t dst_stride7, int64_t src_stride7

#define COPY_UNPACK(ELEM_TYPE)
#endif

#define GEN_COPY_KERNEL(NAME, ELEM_TYPE) \
FUTHARK_KERNEL void lmad_copy_##NAME(COPY_PARAMS(ELEM_TYPE)) {          \
  COPY_UNPACK(ELEM_TYPE)                                                \
  int64_t gtid = get_global_id(0);                                      \
  int64_t remainder = gtid;                                             \
                                                                        \
  if (gtid >= n) {                                                      \
    return;                                                             \
  }                                                                     \
                                                                        \
  if (r > 0) {                                                          \
    int64_t i = remainder % shape0;                                     \
    dst_offset += i * dst_stride0;                                      \
    src_offset += i * src_stride0;                                      \
    remainder /= shape0;                                                \
  }                                                                     \
  if (r > 1) {                                                          \
    int64_t i = remainder % shape1;                                     \
    dst_offset += i * dst_stride1;                                      \
    src_offset += i * src_stride1;                                      \
    remainder /= shape1;                                                \
  }                                                                     \
  if (r > 2) {                                                          \
    int64_t i = remainder % shape2;                                     \
    dst_offset += i * dst_stride2;                                      \
    src_offset += i * src_stride2;                                      \
    remainder /= shape2;                                                \
  }                                                                     \
  if (r > 3) {                                                          \
    int64_t i = remainder % shape3;                                     \
    dst_offset += i * dst_stride3;                                      \
    src_offset += i * src_stride3;                                      \
    remainder /= shape3;                                                \
  }                                                                     \
  if (r > 4) {                                                          \
    int64_t i = remainder % shape4;                                     \
    dst_offset += i * dst_stride4;                                      \
    src_offset += i * src_stride4;                                      \
    remainder /= shape4;                                                \
  }                                                                     \
  if (r > 5) {                                                          \
    int64_t i = remainder % shape5;                                     \
    dst_offset += i * dst_stride5;                                      \
    src_offset += i * src_stride5;                                      \
    remainder /= shape5;                                                \
  }                                                                     \
  if (r > 6) {                                                          \
    int64_t i = remainder % shape6;                                     \
    dst_offset += i * dst_stride6;                                      \
    src_offset += i * src_stride6;                                      \
    remainder /= shape6;                                                \
  }                                                                     \
  if (r > 7) {                                                          \
    int64_t i = remainder % shape7;                                     \
    dst_offset += i * dst_stride7;                                      \
    src_offset += i * src_stride7;                                      \
    remainder /= shape7;                                                \
  }                                                                     \
                                                                        \
  dst_mem[dst_offset] = src_mem[src_offset];                            \
}

GEN_COPY_KERNEL(1b, uint8_t)
GEN_COPY_KERNEL(2b, uint16_t)
GEN_COPY_KERNEL(4b, uint32_t)
GEN_COPY_KERNEL(8b, uint64_t)

// End of copy.cl
