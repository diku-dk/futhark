// Start of transpose.cl

// The kernel parameters are given by macros, because Metal kernels
// take their arguments as a struct.
#ifdef FUTHARK_METAL
#define GEN_TRANSPOSE_ARGS(INT_TYPE)                                    \
  struct transpose_args_##INT_TYPE {                                    \
    device uint8_t *dst_mem;                                            \
    int64_t dst_offset;                                                 \
    device uint8_t *src_mem;                                            \
    int64_t src_offset;                                                 \
    INT_TYPE num_arrays;                                                \
    INT_TYPE x_elems;                                                   \
    INT_TYPE y_elems;                                                   \
    INT_TYPE mulx;                                                      \
    INT_TYPE muly;                                                      \
    int32_t repeat_1;                                                   \
    int32_t repeat_2;                                                   \
  };

GEN_TRANSPOSE_ARGS(int32_t)
GEN_TRANSPOSE_ARGS(int64_t)

#define TRANSPOSE_PARAMS(ELEM_TYPE, INT_TYPE)                           \
  FUTHARK_KERNEL_PARAMS(transpose_args_##INT_TYPE)

#define TRANSPOSE_UNPACK(ELEM_TYPE, INT_TYPE)                           \
  threadgroup uint64_t *shared_mem = shared_mem_aligned;                \
  device ELEM_TYPE *dst_mem = (device ELEM_TYPE*)futhark_args->dst_mem; \
  int64_t dst_offset = futhark_args->dst_offset;                        \
  device ELEM_TYPE *src_mem = (device ELEM_TYPE*)futhark_args->src_mem; \
  int64_t src_offset = futhark_args->src_offset;                        \
  INT_TYPE num_arrays = futhark_args->num_arrays;                       \
  INT_TYPE x_elems = futhark_args->x_elems;                             \
  INT_TYPE y_elems = futhark_args->y_elems;                             \
  INT_TYPE mulx = futhark_args->mulx;                                   \
  INT_TYPE muly = futhark_args->muly;                                   \
  int32_t repeat_1 = futhark_args->repeat_1;                            \
  int32_t repeat_2 = futhark_args->repeat_2;
#else
#define TRANSPOSE_PARAMS(ELEM_TYPE, INT_TYPE)                           \
  SHARED_MEM_PARAM                                                      \
  __global ELEM_TYPE *dst_mem,                                          \
  int64_t dst_offset,                                                   \
  __global ELEM_TYPE *src_mem,                                          \
  int64_t src_offset,                                                   \
  INT_TYPE num_arrays,                                                  \
  INT_TYPE x_elems,                                                     \
  INT_TYPE y_elems,                                                     \
  INT_TYPE mulx,                                                        \
  INT_TYPE muly,                                                        \
  int32_t repeat_1,                                                     \
  int32_t repeat_2

#define TRANSPOSE_UNPACK(ELEM_TYPE, INT_TYPE)
#endif

#define GEN_TRANSPOSE_KERNELS(NAME, ELEM_TYPE)                          \
FUTHARK_KERNEL_SIZED(TR_BLOCK_DIM*2, TR_TILE_DIM/TR_ELEMS_PER_THREAD, 1)\
void map_transpose_##NAME(TRANSPOSE_PARAMS(ELEM_TYPE, int32_t)) {       \
  TRANSPOSE_UNPACK(ELEM_TYPE, int32_t)                                  \
  (void)mulx; (void)muly;                                               \
  __local ELEM_TYPE* block = (__local ELEM_TYPE*)shared_mem;            \
  int tblock_id_0 = get_tblock_id(0);                                   \
  int global_id_0 = get_global_id(0);                                   \
  int tblock_id_1 = get_tblock_id(1);                                   \
  int global_id_1 = get_global_id(1);                                   \
  for (int i1 = 0; i1 <= repeat_1; i1++) {                              \
    int tblock_id_2 = get_tblock_id(2);                                 \
    int global_id_2 = get_global_id(2);                                 \
    for (int i2 = 0; i2 <= repeat_2; i2++) {                            \
      if (tblock_id_2 >= num_arrays) { break; }                         \
      int32_t our_array_offset = tblock_id_2 * x_elems * y_elems;       \
      int32_t odata_offset = dst_offset + our_array_offset;             \
      int32_t idata_offset = src_offset + our_array_offset;             \
      int32_t x_index = global_id_0;                                    \
      int32_t y_index = tblock_id_1 * TR_TILE_DIM + get_local_id(1);    \
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
      x_index = tblock_id_1 * TR_TILE_DIM + get_local_id(0);            \
      y_index = tblock_id_0 * TR_TILE_DIM + get_local_id(1);            \
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
      tblock_id_2 += get_num_tblocks(2);                                \
      global_id_2 += get_global_size(2);                                \
    }                                                                   \
    tblock_id_1 += get_num_tblocks(1);                                  \
    global_id_1 += get_global_size(1);                                  \
  }                                                                     \
}                                                                       \
                                                                        \
FUTHARK_KERNEL_SIZED(TR_BLOCK_DIM, TR_BLOCK_DIM, 1)                     \
void map_transpose_##NAME##_low_height(TRANSPOSE_PARAMS(ELEM_TYPE, int32_t)) { \
  TRANSPOSE_UNPACK(ELEM_TYPE, int32_t)                                  \
  __local ELEM_TYPE* block = (__local ELEM_TYPE*)shared_mem;            \
  int tblock_id_0 = get_tblock_id(0);                                   \
  int global_id_0 = get_global_id(0);                                   \
  int tblock_id_1 = get_tblock_id(1);                                   \
  int global_id_1 = get_global_id(1);                                   \
  for (int i1 = 0; i1 <= repeat_1; i1++) {                              \
    int tblock_id_2 = get_tblock_id(2);                                 \
    int global_id_2 = get_global_id(2);                                 \
    for (int i2 = 0; i2 <= repeat_2; i2++) {                            \
      if (tblock_id_2 >= num_arrays) { break; }                         \
      int32_t our_array_offset = tblock_id_2 * x_elems * y_elems;       \
      int32_t odata_offset = dst_offset + our_array_offset;             \
      int32_t idata_offset = src_offset + our_array_offset;             \
      int32_t x_index =                                                 \
        tblock_id_0 * TR_BLOCK_DIM * mulx +                             \
        get_local_id(0) +                                               \
        get_local_id(1)%mulx * TR_BLOCK_DIM;                            \
      int32_t y_index = tblock_id_1 * TR_BLOCK_DIM + get_local_id(1)/mulx; \
      int32_t index_in = y_index * x_elems + x_index;                   \
      if (x_index < x_elems && y_index < y_elems) {                     \
        block[get_local_id(1) * (TR_BLOCK_DIM+1) + get_local_id(0)] =   \
          src_mem[idata_offset + index_in];                             \
      }                                                                 \
      barrier_local();                                                  \
      x_index = tblock_id_1 * TR_BLOCK_DIM + get_local_id(0)/mulx;      \
      y_index =                                                         \
        tblock_id_0 * TR_BLOCK_DIM * mulx +                             \
        get_local_id(1) +                                               \
        (get_local_id(0)%mulx) * TR_BLOCK_DIM;                          \
      int32_t index_out = y_index * y_elems + x_index;                  \
      if (x_index < y_elems && y_index < x_elems) {                     \
        dst_mem[odata_offset + index_out] =                             \
          block[get_local_id(0) * (TR_BLOCK_DIM+1) + get_local_id(1)];  \
      }                                                                 \
      tblock_id_2 += get_num_tblocks(2);                                \
      global_id_2 += get_global_size(2);                                \
    }                                                                   \
    tblock_id_1 += get_num_tblocks(1);                                  \
    global_id_1 += get_global_size(1);                                  \
  }                                                                     \
}                                                                       \
                                                                        \
FUTHARK_KERNEL_SIZED(TR_BLOCK_DIM, TR_BLOCK_DIM, 1)                     \
void map_transpose_##NAME##_low_width(TRANSPOSE_PARAMS(ELEM_TYPE, int32_t)) { \
  TRANSPOSE_UNPACK(ELEM_TYPE, int32_t)                                  \
  __local ELEM_TYPE* block = (__local ELEM_TYPE*)shared_mem;            \
  int tblock_id_0 = get_tblock_id(0);                                   \
  int global_id_0 = get_global_id(0);                                   \
  int tblock_id_1 = get_tblock_id(1);                                   \
  int global_id_1 = get_global_id(1);                                   \
  for (int i1 = 0; i1 <= repeat_1; i1++) {                              \
    int tblock_id_2 = get_tblock_id(2);                                 \
    int global_id_2 = get_global_id(2);                                 \
    for (int i2 = 0; i2 <= repeat_2; i2++) {                            \
      if (tblock_id_2 >= num_arrays) { break; }                         \
      int32_t our_array_offset = tblock_id_2 * x_elems * y_elems;       \
      int32_t odata_offset = dst_offset + our_array_offset;             \
      int32_t idata_offset = src_offset + our_array_offset;             \
      int32_t x_index = tblock_id_0 * TR_BLOCK_DIM + get_local_id(0)/muly; \
      int32_t y_index =                                                 \
        tblock_id_1 * TR_BLOCK_DIM * muly +                             \
        get_local_id(1) + (get_local_id(0)%muly) * TR_BLOCK_DIM;        \
      int32_t index_in = y_index * x_elems + x_index;                   \
      if (x_index < x_elems && y_index < y_elems) {                     \
        block[get_local_id(1) * (TR_BLOCK_DIM+1) + get_local_id(0)] =   \
          src_mem[idata_offset + index_in];                             \
      }                                                                 \
      barrier_local();                                                  \
      x_index = tblock_id_1 * TR_BLOCK_DIM * muly +                     \
        get_local_id(0) + (get_local_id(1)%muly) * TR_BLOCK_DIM;        \
      y_index = tblock_id_0 * TR_BLOCK_DIM + get_local_id(1)/muly;      \
      int32_t index_out = y_index * y_elems + x_index;                  \
      if (x_index < y_elems && y_index < x_elems) {                     \
        dst_mem[odata_offset + index_out] =                             \
          block[get_local_id(0) * (TR_BLOCK_DIM+1) + get_local_id(1)];  \
      }                                                                 \
      tblock_id_2 += get_num_tblocks(2);                                \
      global_id_2 += get_num_tblocks(2) * get_local_size(2);            \
    }                                                                   \
    tblock_id_1 += get_num_tblocks(1);                                  \
    global_id_1 += get_num_tblocks(1) * get_local_size(1);              \
  }                                                                     \
}                                                                       \
                                                                        \
FUTHARK_KERNEL_SIZED(TR_BLOCK_DIM*TR_BLOCK_DIM, 1, 1)                   \
void map_transpose_##NAME##_small(TRANSPOSE_PARAMS(ELEM_TYPE, int32_t)) { \
  TRANSPOSE_UNPACK(ELEM_TYPE, int32_t)                                  \
  (void)mulx; (void)muly;                                               \
  __local ELEM_TYPE* block = (__local ELEM_TYPE*)shared_mem;            \
  int tblock_id_0 = get_tblock_id(0);                                   \
  int global_id_0 = get_global_id(0);                                   \
  int tblock_id_1 = get_tblock_id(1);                                   \
  int global_id_1 = get_global_id(1);                                   \
  for (int i1 = 0; i1 <= repeat_1; i1++) {                              \
    int tblock_id_2 = get_tblock_id(2);                                 \
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
      tblock_id_2 += get_num_tblocks(2);                                \
      global_id_2 += get_global_size(2);                                \
    }                                                                   \
    tblock_id_1 += get_num_tblocks(1);                                  \
    global_id_1 += get_global_size(1);                                  \
  }                                                                     \
}                                                                       \
                                                                        \
FUTHARK_KERNEL_SIZED(TR_BLOCK_DIM*2, TR_TILE_DIM/TR_ELEMS_PER_THREAD, 1)\
void map_transpose_##NAME##_large(TRANSPOSE_PARAMS(ELEM_TYPE, int64_t)) { \
  TRANSPOSE_UNPACK(ELEM_TYPE, int64_t)                                  \
  (void)mulx; (void)muly;                                               \
  __local ELEM_TYPE* block = (__local ELEM_TYPE*)shared_mem;             \
  int tblock_id_0 = get_tblock_id(0);                                   \
  int global_id_0 = get_global_id(0);                                   \
  int tblock_id_1 = get_tblock_id(1);                                   \
  int global_id_1 = get_global_id(1);                                   \
  for (int i1 = 0; i1 <= repeat_1; i1++) {                              \
    int tblock_id_2 = get_tblock_id(2);                                 \
    int global_id_2 = get_global_id(2);                                 \
    for (int i2 = 0; i2 <= repeat_2; i2++) {                            \
      if (tblock_id_2 >= num_arrays) { break; }                         \
      int64_t our_array_offset = tblock_id_2 * x_elems * y_elems;       \
      int64_t odata_offset = dst_offset + our_array_offset;             \
      int64_t idata_offset = src_offset + our_array_offset;             \
      int64_t x_index = global_id_0;                                    \
      int64_t y_index = tblock_id_1 * TR_TILE_DIM + get_local_id(1);    \
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
      x_index = tblock_id_1 * TR_TILE_DIM + get_local_id(0);            \
      y_index = tblock_id_0 * TR_TILE_DIM + get_local_id(1);            \
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
      tblock_id_2 += get_num_tblocks(2);                                \
      global_id_2 += get_global_size(2);                                \
    }                                                                   \
    tblock_id_1 += get_num_tblocks(1);                                  \
    global_id_1 += get_global_size(1);                                  \
  }                                                                     \
}                                                                       \

GEN_TRANSPOSE_KERNELS(1b, uint8_t)
GEN_TRANSPOSE_KERNELS(2b, uint16_t)
GEN_TRANSPOSE_KERNELS(4b, uint32_t)
GEN_TRANSPOSE_KERNELS(8b, uint64_t)

// End of transpose.cl
