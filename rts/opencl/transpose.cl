// Start of transpose.cl

#define GEN_TRANSPOSE_KERNELS(NAME, ELEM_TYPE)                          \
REQD_GROUP_SIZE(TR_BLOCK_DIM*2, TR_TILE_DIM/TR_ELEMS_PER_THREAD, 1)     \
__kernel void map_transpose_##NAME(LOCAL_MEM_PARAM                      \
                                   __global ELEM_TYPE *dst_mem,         \
                                   int64_t dst_offset,                  \
                                   __global ELEM_TYPE *src_mem,         \
                                   int64_t src_offset,                  \
                                   int32_t num_arrays,                  \
                                   int32_t x_elems,                     \
                                   int32_t y_elems,                     \
                                   int32_t mulx,                        \
                                   int32_t muly,                        \
                                   int32_t repeat_1,                    \
                                   int32_t repeat_2) {                  \
  (void)mulx; (void)muly;                                               \
  __local ELEM_TYPE* block = (__local ELEM_TYPE*)local_mem;             \
  int group_id_0 = get_group_id(0);                                     \
  int global_id_0 = get_global_id(0);                                   \
  int group_id_1 = get_group_id(1);                                     \
  int global_id_1 = get_global_id(1);                                   \
  for (int i1 = 0; i1 <= repeat_1; i1++) {                              \
    int group_id_2 = get_group_id(2);                                   \
    int global_id_2 = get_global_id(2);                                 \
    for (int i2 = 0; i2 <= repeat_2; i2++) {                            \
      int32_t our_array_offset = group_id_2 * x_elems * y_elems;        \
      int32_t odata_offset = dst_offset + our_array_offset;             \
      int32_t idata_offset = src_offset + our_array_offset;             \
      int32_t x_index = global_id_0;                                    \
      int32_t y_index = group_id_1 * TR_TILE_DIM + get_local_id(1);     \
      if (x_index < x_elems) {                                          \
        for (int32_t j = 0; j < TR_ELEMS_PER_THREAD; j++) {             \
          int32_t index_i = (y_index + j * (TR_TILE_DIM/TR_ELEMS_PER_THREAD)) * x_elems + x_index; \
          if (y_index + j * (TR_TILE_DIM/TR_ELEMS_PER_THREAD) < y_elems) { \
            block[(get_local_id(1) + j * (TR_TILE_DIM/TR_ELEMS_PER_THREAD)) * (TR_TILE_DIM+1) + \
                  get_local_id(0)] =                                    \
              src_mem[idata_offset + index_i];                          \
          }                                                             \
        }                                                               \
      }                                                                 \
      barrier_local();                                                  \
      x_index = group_id_1 * TR_TILE_DIM + get_local_id(0);             \
      y_index = group_id_0 * TR_TILE_DIM + get_local_id(1);             \
      if (x_index < y_elems) {                                          \
        for (int32_t j = 0; j < TR_ELEMS_PER_THREAD; j++) {             \
          int32_t index_out = (y_index + j * (TR_TILE_DIM/TR_ELEMS_PER_THREAD)) * y_elems + x_index; \
          if (y_index + j * (TR_TILE_DIM/TR_ELEMS_PER_THREAD) < x_elems) { \
            dst_mem[(odata_offset + index_out)] =                       \
              block[get_local_id(0) * (TR_TILE_DIM+1) +                 \
                    get_local_id(1) + j * (TR_TILE_DIM/TR_ELEMS_PER_THREAD)]; \
          }                                                             \
        }                                                               \
      }                                                                 \
      group_id_2 += get_num_groups(2);                                  \
      global_id_2 += get_global_size(2);                                \
    }                                                                   \
    group_id_1 += get_num_groups(1);                                    \
    global_id_1 += get_global_size(1);                                  \
  }                                                                     \
}                                                                       \
                                                                        \
__attribute__((reqd_work_group_size(TR_BLOCK_DIM, TR_BLOCK_DIM, 1)))   \
__kernel void map_transpose_##NAME##_low_height(LOCAL_MEM_PARAM        \
                                                __global ELEM_TYPE *dst_mem, \
                                                int64_t dst_offset,     \
                                                __global ELEM_TYPE *src_mem, \
                                                int64_t src_offset,     \
                                                int32_t num_arrays,     \
                                                int32_t x_elems,        \
                                                int32_t y_elems,        \
                                                int32_t mulx,           \
                                                int32_t muly,           \
                                                int32_t repeat_1,       \
                                                int32_t repeat_2) {     \
  __local ELEM_TYPE* block = (__local ELEM_TYPE*)local_mem; \
  int group_id_0 = get_group_id(0);                                     \
  int global_id_0 = get_global_id(0);                                   \
  int group_id_1 = get_group_id(1);                                     \
  int global_id_1 = get_global_id(1);                                   \
  for (int i1 = 0; i1 <= repeat_1; i1++) {                              \
    int group_id_2 = get_group_id(2);                                   \
    int global_id_2 = get_global_id(2);                                 \
    for (int i2 = 0; i2 <= repeat_2; i2++) {                            \
      int32_t our_array_offset = group_id_2 * x_elems * y_elems;        \
      int32_t odata_offset = dst_offset + our_array_offset;             \
      int32_t idata_offset = src_offset + our_array_offset;             \
      int32_t x_index =                                                 \
        group_id_0 * TR_BLOCK_DIM * mulx +                              \
        get_local_id(0) +                                               \
        get_local_id(1)%mulx * TR_BLOCK_DIM;                            \
      int32_t y_index = group_id_1 * TR_BLOCK_DIM + get_local_id(1)/mulx; \
      int32_t index_in = y_index * x_elems + x_index;                   \
      if (x_index < x_elems && y_index < y_elems) {                     \
        block[get_local_id(1) * (TR_BLOCK_DIM+1) + get_local_id(0)] =   \
          src_mem[idata_offset + index_in];                             \
      }                                                                 \
      barrier_local();                                                  \
      x_index = group_id_1 * TR_BLOCK_DIM + get_local_id(0)/mulx;       \
      y_index =                                                         \
        group_id_0 * TR_BLOCK_DIM * mulx +                              \
        get_local_id(1) +                                               \
        (get_local_id(0)%mulx) * TR_BLOCK_DIM;                          \
      int32_t index_out = y_index * y_elems + x_index;                  \
      if (x_index < y_elems && y_index < x_elems) {                     \
        dst_mem[odata_offset + index_out] =                             \
          block[get_local_id(0) * (TR_BLOCK_DIM+1) + get_local_id(1)];  \
      }                                                                 \
      group_id_2 += get_num_groups(2);                                  \
      global_id_2 += get_global_size(2);                                \
    }                                                                   \
    group_id_1 += get_num_groups(1);                                    \
    global_id_1 += get_global_size(1);                                  \
  }                                                                     \
}                                                                       \
                                                                        \
REQD_GROUP_SIZE(TR_BLOCK_DIM, TR_BLOCK_DIM, 1)  \
__kernel void map_transpose_##NAME##_low_width(LOCAL_MEM_PARAM \
                                               __global ELEM_TYPE *dst_mem, \
                                               int64_t dst_offset,      \
                                               __global ELEM_TYPE *src_mem, \
                                               int64_t src_offset,      \
                                               int32_t num_arrays,      \
                                               int32_t x_elems,         \
                                               int32_t y_elems,         \
                                               int32_t mulx,            \
                                               int32_t muly,            \
                                               int32_t repeat_1,        \
                                               int32_t repeat_2) {      \
  __local ELEM_TYPE* block = (__local ELEM_TYPE*)local_mem; \
  int group_id_0 = get_group_id(0);                                     \
  int global_id_0 = get_global_id(0);                                   \
  int group_id_1 = get_group_id(1);                                     \
  int global_id_1 = get_global_id(1);                                   \
  for (int i1 = 0; i1 <= repeat_1; i1++) {                              \
    int group_id_2 = get_group_id(2);                                   \
    int global_id_2 = get_global_id(2);                                 \
    for (int i2 = 0; i2 <= repeat_2; i2++) {                            \
      int32_t our_array_offset = group_id_2 * x_elems * y_elems;        \
      int32_t odata_offset = dst_offset + our_array_offset;             \
      int32_t idata_offset = src_offset + our_array_offset;             \
      int32_t x_index = group_id_0 * TR_BLOCK_DIM + get_local_id(0)/muly; \
      int32_t y_index =                                                 \
        group_id_1 * TR_BLOCK_DIM * muly +                              \
        get_local_id(1) + (get_local_id(0)%muly) * TR_BLOCK_DIM;        \
      int32_t index_in = y_index * x_elems + x_index;                   \
      if (x_index < x_elems && y_index < y_elems) {                     \
        block[get_local_id(1) * (TR_BLOCK_DIM+1) + get_local_id(0)] =   \
          src_mem[idata_offset + index_in];                             \
      }                                                                 \
      barrier_local();                                                  \
      x_index = group_id_1 * TR_BLOCK_DIM * muly +                      \
        get_local_id(0) + (get_local_id(1)%muly) * TR_BLOCK_DIM;        \
      y_index = group_id_0 * TR_BLOCK_DIM + get_local_id(1)/muly;       \
      int32_t index_out = y_index * y_elems + x_index;                  \
      if (x_index < y_elems && y_index < x_elems) {                     \
        dst_mem[odata_offset + index_out] =                             \
          block[get_local_id(0) * (TR_BLOCK_DIM+1) + get_local_id(1)];  \
      }                                                                 \
      group_id_2 += get_num_groups(2);                                  \
      global_id_2 += get_num_groups(2) * get_local_size(2);             \
    }                                                                   \
    group_id_1 += get_num_groups(1);                                    \
    global_id_1 += get_num_groups(1) * get_local_size(1);               \
  }                                                                     \
}                                                                       \
                                                                        \
REQD_GROUP_SIZE(TR_BLOCK_DIM*TR_BLOCK_DIM, 1, 1)                       \
__kernel void map_transpose_##NAME##_small(LOCAL_MEM_PARAM    \
                                           __global ELEM_TYPE *dst_mem, \
                                           int64_t dst_offset,          \
                                           __global ELEM_TYPE *src_mem, \
                                           int64_t src_offset,          \
                                           int32_t num_arrays,          \
                                           int32_t x_elems,             \
                                           int32_t y_elems,             \
                                           int32_t mulx,                \
                                           int32_t muly,                \
                                           int32_t repeat_1,            \
                                           int32_t repeat_2) {          \
  (void)mulx; (void)muly; \
  __local ELEM_TYPE* block = (__local ELEM_TYPE*)local_mem;             \
  int group_id_0 = get_group_id(0);                                     \
  int global_id_0 = get_global_id(0);                                   \
  int group_id_1 = get_group_id(1);                                     \
  int global_id_1 = get_global_id(1);                                   \
  for (int i1 = 0; i1 <= repeat_1; i1++) {                              \
    int group_id_2 = get_group_id(2);                                   \
    int global_id_2 = get_global_id(2);                                 \
    for (int i2 = 0; i2 <= repeat_2; i2++) {                            \
      int32_t our_array_offset = global_id_0/(y_elems * x_elems) * y_elems * x_elems; \
      int32_t x_index = (global_id_0 % (y_elems * x_elems))/y_elems;    \
      int32_t y_index = global_id_0%y_elems;                            \
      int32_t odata_offset = dst_offset + our_array_offset;             \
      int32_t idata_offset = src_offset + our_array_offset;             \
      int32_t index_in = y_index * x_elems + x_index;                   \
      int32_t index_out = x_index * y_elems + y_index;                  \
      if (global_id_0 < x_elems * y_elems * num_arrays) {               \
        dst_mem[odata_offset + index_out] = src_mem[idata_offset + index_in]; \
      }                                                                 \
      group_id_2 += get_num_groups(2);                                  \
      global_id_2 += get_global_size(2);                                \
    }                                                                   \
    group_id_1 += get_num_groups(1);                                    \
    global_id_1 += get_global_size(1);                                  \
  }                                                                     \
}                                                                       \
                                                                        \
REQD_GROUP_SIZE(TR_BLOCK_DIM*2, TR_TILE_DIM/TR_ELEMS_PER_THREAD, 1)     \
__kernel void map_transpose_##NAME##_large(LOCAL_MEM_PARAM   \
                                           __global ELEM_TYPE *dst_mem, \
                                           int64_t dst_offset,          \
                                           __global ELEM_TYPE *src_mem, \
                                           int64_t src_offset,          \
                                           int64_t num_arrays,          \
                                           int64_t x_elems,             \
                                           int64_t y_elems,             \
                                           int64_t mulx,                \
                                           int64_t muly,                \
                                           int32_t repeat_1,            \
                                           int32_t repeat_2) {          \
  (void)mulx; (void)muly;                                               \
  __local ELEM_TYPE* block = (__local ELEM_TYPE*)local_mem;             \
  int group_id_0 = get_group_id(0);                                     \
  int global_id_0 = get_global_id(0);                                   \
  int group_id_1 = get_group_id(1);                                     \
  int global_id_1 = get_global_id(1);                                   \
  for (int i1 = 0; i1 <= repeat_1; i1++) {                              \
    int group_id_2 = get_group_id(2);                                   \
    int global_id_2 = get_global_id(2);                                 \
    for (int i2 = 0; i2 <= repeat_2; i2++) {                            \
      int64_t our_array_offset = group_id_2 * x_elems * y_elems;        \
      int64_t odata_offset = dst_offset + our_array_offset;             \
      int64_t idata_offset = src_offset + our_array_offset;             \
      int64_t x_index = global_id_0;                                    \
      int64_t y_index = group_id_1 * TR_TILE_DIM + get_local_id(1);     \
      if (x_index < x_elems) {                                          \
        for (int64_t j = 0; j < TR_ELEMS_PER_THREAD; j++) {             \
          int64_t index_i = (y_index + j * (TR_TILE_DIM/TR_ELEMS_PER_THREAD)) * x_elems + x_index; \
          if (y_index + j * (TR_TILE_DIM/TR_ELEMS_PER_THREAD) < y_elems) { \
            block[(get_local_id(1) + j * (TR_TILE_DIM/TR_ELEMS_PER_THREAD)) * (TR_TILE_DIM+1) + \
                  get_local_id(0)] =                                    \
              src_mem[idata_offset + index_i];                          \
          }                                                             \
        }                                                               \
      }                                                                 \
      barrier_local();                                                  \
      x_index = group_id_1 * TR_TILE_DIM + get_local_id(0);             \
      y_index = group_id_0 * TR_TILE_DIM + get_local_id(1);             \
      if (x_index < y_elems) {                                          \
        for (int64_t j = 0; j < TR_ELEMS_PER_THREAD; j++) {             \
          int64_t index_out = (y_index + j * (TR_TILE_DIM/TR_ELEMS_PER_THREAD)) * y_elems + x_index; \
          if (y_index + j * (TR_TILE_DIM/TR_ELEMS_PER_THREAD) < x_elems) { \
            dst_mem[(odata_offset + index_out)] =                       \
              block[get_local_id(0) * (TR_TILE_DIM+1) +                 \
                    get_local_id(1) + j * (TR_TILE_DIM/TR_ELEMS_PER_THREAD)]; \
          }                                                             \
        }                                                               \
      }                                                                 \
      group_id_2 += get_num_groups(2);                                  \
      global_id_2 += get_global_size(2);                                \
    }                                                                   \
    group_id_1 += get_num_groups(1);                                    \
    global_id_1 += get_global_size(1);                                  \
  }                                                                     \
}                                                                       \

GEN_TRANSPOSE_KERNELS(1b, uint8_t)
GEN_TRANSPOSE_KERNELS(2b, uint16_t)
GEN_TRANSPOSE_KERNELS(4b, uint32_t)
GEN_TRANSPOSE_KERNELS(8b, uint64_t)

// End of transpose.cl
