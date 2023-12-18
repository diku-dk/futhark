// Start of copy.cl

#define GEN_COPY_KERNEL(NAME, ELEM_TYPE) \
FUTHARK_KERNEL void lmad_copy_##NAME(SHARED_MEM_PARAM                   \
                               __global ELEM_TYPE *dst_mem,             \
                               int64_t dst_offset,                      \
                               __global ELEM_TYPE *src_mem,             \
                               int64_t src_offset,                      \
                               int64_t n,                               \
                               int r,                                   \
                               int64_t shape0, int64_t dst_stride0, int64_t src_stride0, \
                               int64_t shape1, int64_t dst_stride1, int64_t src_stride1, \
                               int64_t shape2, int64_t dst_stride2, int64_t src_stride2, \
                               int64_t shape3, int64_t dst_stride3, int64_t src_stride3, \
                               int64_t shape4, int64_t dst_stride4, int64_t src_stride4, \
                               int64_t shape5, int64_t dst_stride5, int64_t src_stride5, \
                               int64_t shape6, int64_t dst_stride6, int64_t src_stride6, \
                               int64_t shape7, int64_t dst_stride7, int64_t src_stride7) { \
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
