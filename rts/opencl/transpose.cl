// Start of transpose.cl

#define GEN_TRANSPOSE_KERNELS(NAME, ELEM_TYPE)                          \
__attribute__((reqd_work_group_size(TR_BLOCK_DIM*2, TR_TILE_DIM/TR_ELEMS_PER_THREAD, 1))) \
__kernel void map_transpose_##NAME(__global ELEM_TYPE *dst_mem,       \
                                   int64_t dst_offset,                  \
                                   __global ELEM_TYPE *src_mem,         \
                                   int64_t src_offset,                  \
                                   int32_t num_arrays,                  \
                                   int32_t x_elems,                     \
                                   int32_t y_elems,                     \
                                   int32_t mulx,                        \
                                   int32_t muly) {                      \
  (void)mulx; (void)muly;                                               \
  __local ELEM_TYPE block[TR_TILE_DIM*(TR_TILE_DIM+1)];                 \
  int32_t our_array_offset = get_group_id(2) * x_elems * y_elems;       \
  int32_t odata_offset = dst_offset + our_array_offset;                 \
  int32_t idata_offset = src_offset + our_array_offset;                 \
  int32_t x_index = get_global_id(0);                                   \
  int32_t y_index = get_group_id(1) * TR_TILE_DIM + get_local_id(1);    \
  if (x_index < x_elems) {                                              \
    for (int32_t j = 0; j < TR_ELEMS_PER_THREAD; j++) {                 \
      int32_t index_i = (y_index + j * (TR_TILE_DIM/TR_ELEMS_PER_THREAD)) * x_elems + x_index; \
      if (y_index + j * (TR_TILE_DIM/TR_ELEMS_PER_THREAD) < y_elems) {  \
        block[(get_local_id(1) + j * (TR_TILE_DIM/TR_ELEMS_PER_THREAD)) * (TR_TILE_DIM+1) + \
              get_local_id(0)] =                                        \
          src_mem[idata_offset + index_i];                              \
      }                                                                 \
    }                                                                   \
  }                                                                     \
  barrier_local();                                                      \
  x_index = get_group_id(1) * TR_TILE_DIM + get_local_id(0);            \
  y_index = get_group_id(0) * TR_TILE_DIM + get_local_id(1);            \
  if (x_index < y_elems) {                                              \
    for (int32_t j = 0; j < TR_ELEMS_PER_THREAD; j++) {                 \
      int32_t index_out = (y_index + j * (TR_TILE_DIM/TR_ELEMS_PER_THREAD)) * y_elems + x_index; \
      if (y_index + j * (TR_TILE_DIM/TR_ELEMS_PER_THREAD) < x_elems) {  \
        dst_mem[(odata_offset + index_out)] =                           \
          block[get_local_id(0) * (TR_TILE_DIM+1) +                     \
                get_local_id(1) + j * (TR_TILE_DIM/TR_ELEMS_PER_THREAD)]; \
      }                                                                 \
    }                                                                   \
  }                                                                     \
}                                                                       \
                                                                        \
__attribute__((reqd_work_group_size(TR_BLOCK_DIM, TR_BLOCK_DIM, 1)))  \
__kernel void map_transpose_##NAME##_low_height(__global ELEM_TYPE *dst_mem, \
                                                int64_t dst_offset,     \
                                                __global ELEM_TYPE *src_mem, \
                                                int64_t src_offset,     \
                                                int32_t num_arrays,     \
                                                int32_t x_elems,        \
                                                int32_t y_elems,        \
                                                int32_t mulx,           \
                                                int32_t muly) {         \
  __local ELEM_TYPE block[TR_BLOCK_DIM*(TR_BLOCK_DIM+1)];               \
                                                                        \
  int32_t our_array_offset = get_group_id(2) * x_elems * y_elems;       \
  int32_t odata_offset = dst_offset + our_array_offset;                 \
  int32_t idata_offset = src_offset + our_array_offset;                 \
  int32_t x_index =                                                     \
    get_group_id(0) * TR_BLOCK_DIM * mulx +                             \
    get_local_id(0) +                                                   \
    get_local_id(1)%mulx * TR_BLOCK_DIM;                                \
  int32_t y_index = get_group_id(1) * TR_BLOCK_DIM + get_local_id(1)/mulx; \
  int32_t index_in = y_index * x_elems + x_index;                       \
                                                                        \
  if (x_index < x_elems && y_index < y_elems) {                         \
    block[get_local_id(1) * (TR_BLOCK_DIM+1) + get_local_id(0)] =       \
      src_mem[idata_offset + index_in];                                 \
  }                                                                     \
  barrier_local();                                                      \
  x_index = get_group_id(1) * TR_BLOCK_DIM + get_local_id(0)/mulx;      \
  y_index =                                                             \
    get_group_id(0) * TR_BLOCK_DIM * mulx +                             \
    get_local_id(1) +                                                   \
    (get_local_id(0)%mulx) * TR_BLOCK_DIM;                              \
                                                                        \
  int32_t index_out = y_index * y_elems + x_index;                      \
                                                                        \
  if (x_index < y_elems && y_index < x_elems) {                         \
    dst_mem[odata_offset + index_out] =                                 \
      block[get_local_id(0) * (TR_BLOCK_DIM+1) + get_local_id(1)];      \
  }                                                                     \
}                                                                       \
                                                                        \
  __attribute__((reqd_work_group_size(TR_BLOCK_DIM, TR_BLOCK_DIM, 1)))  \
  __kernel void map_transpose_##NAME##_low_width(__global ELEM_TYPE *dst_mem, \
                                                 int64_t dst_offset,    \
                                                 __global ELEM_TYPE *src_mem, \
                                                 int64_t src_offset,    \
                                                 int32_t num_arrays,    \
                                                 int32_t x_elems,       \
                                                 int32_t y_elems,       \
                                                 int32_t mulx,          \
                                                 int32_t muly)          \
  {                                                                     \
    __local ELEM_TYPE block[TR_BLOCK_DIM*(TR_BLOCK_DIM+1)];             \
                                                                        \
    int32_t our_array_offset = get_group_id(2) * x_elems * y_elems;     \
    int32_t odata_offset = dst_offset + our_array_offset;               \
    int32_t idata_offset = src_offset + our_array_offset;               \
    int32_t x_index = get_group_id(0) * TR_BLOCK_DIM + get_local_id(0)/muly; \
    int32_t y_index =                                                   \
      get_group_id(1) * TR_BLOCK_DIM * muly +                           \
      get_local_id(1) + (get_local_id(0)%(muly)) * TR_BLOCK_DIM;        \
    int32_t index_in = y_index * x_elems + x_index;                     \
                                                                        \
    if (x_index < x_elems && y_index < y_elems) {                       \
      block[get_local_id(1) * (TR_BLOCK_DIM+1) + get_local_id(0)] =     \
        src_mem[idata_offset + index_in];                               \
    }                                                                   \
    barrier_local();                                                    \
    x_index = get_group_id(1) * TR_BLOCK_DIM * muly +                   \
      get_local_id(0) + (get_local_id(1)%muly) * TR_BLOCK_DIM;          \
    y_index = get_group_id(0) * TR_BLOCK_DIM + get_local_id(1)/muly;    \
                                                                        \
    int32_t index_out = y_index * y_elems + x_index;                    \
                                                                        \
    if (x_index < y_elems && y_index < x_elems) {                       \
      dst_mem[odata_offset + index_out] =                               \
        block[get_local_id(0) * (TR_BLOCK_DIM+1) + get_local_id(1)];    \
    }                                                                   \
  }                                                                     \
                                                                        \
__attribute__((reqd_work_group_size(TR_BLOCK_DIM*TR_BLOCK_DIM, 1, 1))) \
__kernel void map_transpose_##NAME##_small(__global ELEM_TYPE *dst_mem, \
                                           int64_t dst_offset,          \
                                           __global ELEM_TYPE *src_mem, \
                                           int64_t src_offset,          \
                                           int32_t num_arrays,          \
                                           int32_t x_elems,             \
                                           int32_t y_elems,             \
                                           int32_t mulx,                \
                                           int32_t muly) {              \
    (void)mulx; (void)muly;                                             \
    int32_t our_array_offset = get_global_id(0)/(y_elems * x_elems) * y_elems * x_elems; \
    int32_t x_index = (get_global_id(0) % (y_elems * x_elems))/y_elems; \
    int32_t y_index = get_global_id(0)%y_elems;                         \
    int32_t odata_offset = dst_offset + our_array_offset;               \
    int32_t idata_offset = src_offset + our_array_offset;               \
    int32_t index_in = y_index * x_elems + x_index;                     \
    int32_t index_out = x_index * y_elems + y_index;                    \
                                                                        \
    if (get_global_id(0) < x_elems * y_elems * num_arrays) {            \
      dst_mem[odata_offset + index_out] = src_mem[idata_offset + index_in]; \
    }                                                                   \
  }                                                                     \
                                                                        \
__attribute__((reqd_work_group_size(TR_BLOCK_DIM*2, TR_TILE_DIM/TR_ELEMS_PER_THREAD, 1))) \
 __kernel void map_transpose_##NAME##_large(__global ELEM_TYPE *dst_mem,  \
                                            int64_t dst_offset,         \
                                            __global ELEM_TYPE *src_mem, \
                                            int64_t src_offset,         \
                                            int64_t num_arrays,         \
                                            int64_t x_elems,            \
                                            int64_t y_elems,            \
                                            int64_t mulx,               \
                                            int64_t muly) {             \
  (void)mulx; (void)muly;                                               \
  __local ELEM_TYPE block[TR_TILE_DIM*(TR_TILE_DIM+1)];                 \
  int64_t our_array_offset = get_group_id(2) * x_elems * y_elems;       \
  int64_t odata_offset = dst_offset + our_array_offset;                 \
  int64_t idata_offset = src_offset + our_array_offset;                 \
  int64_t x_index = get_global_id(0);                                   \
  int64_t y_index = get_group_id(1) * TR_TILE_DIM + get_local_id(1);    \
  if (x_index < x_elems) {                                              \
    for (int64_t j = 0; j < TR_ELEMS_PER_THREAD; j++) {                 \
      int64_t index_i = (y_index + j * (TR_TILE_DIM/TR_ELEMS_PER_THREAD)) * x_elems + x_index; \
      if (y_index + j * (TR_TILE_DIM/TR_ELEMS_PER_THREAD) < y_elems) {  \
        block[(get_local_id(1) + j * (TR_TILE_DIM/TR_ELEMS_PER_THREAD)) * (TR_TILE_DIM+1) + \
              get_local_id(0)] =                                        \
          src_mem[idata_offset + index_i];                              \
      }                                                                 \
    }                                                                   \
  }                                                                     \
  barrier_local();                                                      \
  x_index = get_group_id(1) * TR_TILE_DIM + get_local_id(0);            \
  y_index = get_group_id(0) * TR_TILE_DIM + get_local_id(1);            \
  if (x_index < y_elems) {                                              \
    for (int64_t j = 0; j < TR_ELEMS_PER_THREAD; j++) {                 \
      int64_t index_out = (y_index + j * (TR_TILE_DIM/TR_ELEMS_PER_THREAD)) * y_elems + x_index; \
      if (y_index + j * (TR_TILE_DIM/TR_ELEMS_PER_THREAD) < x_elems) {  \
        dst_mem[(odata_offset + index_out)] =                           \
          block[get_local_id(0) * (TR_TILE_DIM+1) +                     \
                get_local_id(1) + j * (TR_TILE_DIM/TR_ELEMS_PER_THREAD)]; \
      }                                                                 \
    }                                                                   \
  }                                                                     \
}                                                                       \

GEN_TRANSPOSE_KERNELS(1b, uint8_t)
GEN_TRANSPOSE_KERNELS(2b, uint16_t)
GEN_TRANSPOSE_KERNELS(4b, uint32_t)
GEN_TRANSPOSE_KERNELS(8b, uint64_t)

// End of transpose.cl
