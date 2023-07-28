__attribute__((reqd_work_group_size(TR_BLOCK_DIM*2, TR_TILE_DIM/TR_ELEMS_PER_THREAD, 1)))
__kernel void map_transpose_4b(__global uint32_t *dstmem,
                               int64_t dstoffset,
                               __global uint32_t *srcmem,
                               int64_t srcoffset,
                               int32_t num_arrays,
                               int32_t x_elems,
                               int32_t y_elems,
                               int32_t mulx,
                               int32_t muly) {
  (void)mulx; (void)muly;
  __local uint32_t block[TR_TILE_DIM*(TR_TILE_DIM+1)];
  int32_t our_array_offset = get_group_id(2) * x_elems * y_elems;
  int32_t odata_offset = dstoffset + our_array_offset;
  int32_t idata_offset = srcoffset + our_array_offset;
  int32_t x_index = get_global_id(0);
  int32_t y_index = get_group_id(1) * TR_TILE_DIM + get_local_id(1);
  if (x_index < x_elems) {
    for (int32_t j = 0; j < TR_ELEMS_PER_THREAD; j++) {
      int32_t index_i = (y_index + j * (TR_TILE_DIM/TR_ELEMS_PER_THREAD)) * x_elems + x_index;
      if (y_index + j * (TR_TILE_DIM/TR_ELEMS_PER_THREAD) < y_elems) {
        block[(get_local_id(1) + j * (TR_TILE_DIM/TR_ELEMS_PER_THREAD)) * (TR_TILE_DIM+1) +
              get_local_id(0)] =
          srcmem[idata_offset + index_i];
      }
    }
  }
  barrier(CLK_LOCAL_MEM_FENCE);
  x_index = get_group_id(1) * TR_TILE_DIM + get_local_id(0);
  y_index = get_group_id(0) * TR_TILE_DIM + get_local_id(1);
  if (x_index < y_elems) {
    for (int32_t j = 0; j < TR_ELEMS_PER_THREAD; j++) {
      int32_t index_out = (y_index + j * (TR_TILE_DIM/TR_ELEMS_PER_THREAD)) * y_elems + x_index;
      if (y_index + j * (TR_TILE_DIM/TR_ELEMS_PER_THREAD) < x_elems) {
        dstmem[(odata_offset + index_out)] =
          block[get_local_id(0) * (TR_TILE_DIM+1) +
                get_local_id(1) + j * (TR_TILE_DIM/TR_ELEMS_PER_THREAD)];
      }
    }
  }
}

__attribute__((reqd_work_group_size(TR_BLOCK_DIM, TR_BLOCK_DIM, 1)))
__kernel void map_transpose_4b_low_height(__global uint32_t *dstmem,
                                          int64_t dstoffset,
                                          __global uint32_t *srcmem,
                                          int64_t srcoffset,
                                          int32_t num_arrays,
                                          int32_t x_elems,
                                          int32_t y_elems,
                                          int32_t mulx,
                                          int32_t muly)
{
  __local uint32_t block[TR_BLOCK_DIM*(TR_BLOCK_DIM+1)];

  int32_t our_array_offset = get_group_id(2) * x_elems * y_elems;
  int32_t odata_offset = dstoffset + our_array_offset;
  int32_t idata_offset = srcoffset + our_array_offset;
  int32_t x_index = get_group_id(0) * TR_BLOCK_DIM * mulx + get_local_id(0) + get_local_id(1)%mulx * TR_BLOCK_DIM;
  int32_t y_index = get_group_id(1) * TR_BLOCK_DIM + get_local_id(1)/mulx;
  int32_t index_in = y_index * x_elems + x_index;

  if (x_index < x_elems && y_index < y_elems) {
    block[get_local_id(1) * (TR_BLOCK_DIM+1) + get_local_id(0)] =
      srcmem[idata_offset + index_in];
  }
  barrier(CLK_LOCAL_MEM_FENCE);
  x_index = get_group_id(1) * TR_BLOCK_DIM + get_local_id(0)/mulx;
  y_index = get_group_id(0) * TR_BLOCK_DIM * mulx + get_local_id(1) + (get_local_id(0)%mulx) * TR_BLOCK_DIM;

  int32_t index_out = y_index * y_elems + x_index;

  if (x_index < y_elems && y_index < x_elems) {
    dstmem[odata_offset + index_out] =
      block[get_local_id(0) * (TR_BLOCK_DIM+1) + get_local_id(1)];
  }
}

__attribute__((reqd_work_group_size(TR_BLOCK_DIM, TR_BLOCK_DIM, 1)))
__kernel void map_transpose_4b_low_width(__global uint32_t *destmem,
                                         int64_t destoffset,
                                         __global uint32_t *srcmem,
                                         int64_t srcoffset,
                                         int32_t num_arrays,
                                         int32_t x_elems,
                                         int32_t y_elems,
                                         int32_t mulx,
                                         int32_t muly)
{
  __local uint32_t block[TR_BLOCK_DIM*(TR_BLOCK_DIM+1)];

  int32_t our_array_offset = get_group_id(2) * x_elems * y_elems;
  int32_t odata_offset = destoffset + our_array_offset;
  int32_t idata_offset = srcoffset + our_array_offset;
  int32_t x_index = get_group_id(0) * TR_BLOCK_DIM + get_local_id(0)/muly;
  int32_t y_index = get_group_id(1) * TR_BLOCK_DIM * muly + get_local_id(1) + (get_local_id(0)%(muly)) * TR_BLOCK_DIM;
  int32_t index_in = y_index * x_elems + x_index;

  if (x_index < x_elems && y_index < y_elems) {
    block[get_local_id(1) * (TR_BLOCK_DIM+1) + get_local_id(0)] =
      srcmem[idata_offset + index_in];
  }
  barrier(CLK_LOCAL_MEM_FENCE);
  x_index = get_group_id(1) * TR_BLOCK_DIM * muly + get_local_id(0) + (get_local_id(1)%muly) * TR_BLOCK_DIM;
  y_index = get_group_id(0) * TR_BLOCK_DIM + get_local_id(1)/muly;

  int32_t index_out = y_index * y_elems + x_index;

  if (x_index < y_elems && y_index < x_elems) {
    destmem[odata_offset + index_out] =
      block[get_local_id(0) * (TR_BLOCK_DIM+1) + get_local_id(1)];
  }
}

__attribute__((reqd_work_group_size(TR_BLOCK_DIM*TR_BLOCK_DIM, 1, 1)))
__kernel void map_transpose_4b_small(__global uint32_t *dstmem,
                                     int64_t dstoffset,
                                     __global uint32_t *srcmem,
                                     int64_t srcoffset,
                                     int32_t num_arrays,
                                     int32_t x_elems,
                                     int32_t y_elems,
                                     int32_t mulx,
                                     int32_t muly)
{
  (void)mulx; (void)muly;
  int32_t our_array_offset = get_global_id(0)/(y_elems * x_elems) * y_elems * x_elems;
  int32_t x_index = (get_global_id(0) % (y_elems * x_elems))/y_elems;
  int32_t y_index = get_global_id(0)%y_elems;
  int32_t odata_offset = dstoffset + our_array_offset;
  int32_t idata_offset = srcoffset + our_array_offset;
  int32_t index_in = y_index * x_elems + x_index;
  int32_t index_out = x_index * y_elems + y_index;

  if (get_global_id(0) < x_elems * y_elems * num_arrays) {
    dstmem[odata_offset + index_out] = srcmem[idata_offset + index_in];
  }
}

__attribute__((reqd_work_group_size(TR_BLOCK_DIM*2, TR_TILE_DIM/TR_ELEMS_PER_THREAD, 1)))
__kernel void map_transpose_4b_large(__global uint32_t *dstmem,
                                     int64_t dstoffset,
                                     __global uint32_t *srcmem,
                                     int64_t srcoffset,
                                     int64_t num_arrays,
                                     int64_t x_elems,
                                     int64_t y_elems,
                                     int64_t mulx,
                                     int64_t muly) {
  (void)mulx; (void)muly;
  __local uint32_t block[TR_TILE_DIM*(TR_TILE_DIM+1)];
  int64_t our_array_offset = get_group_id(2) * x_elems * y_elems;
  int64_t odata_offset = dstoffset + our_array_offset;
  int64_t idata_offset = srcoffset + our_array_offset;
  int64_t x_index = get_global_id(0);
  int64_t y_index = get_group_id(1) * TR_TILE_DIM + get_local_id(1);
  if (x_index < x_elems) {
    for (int64_t j = 0; j < TR_ELEMS_PER_THREAD; j++) {
      int64_t index_i = (y_index + j * (TR_TILE_DIM/TR_ELEMS_PER_THREAD)) * x_elems + x_index;
      if (y_index + j * (TR_TILE_DIM/TR_ELEMS_PER_THREAD) < y_elems) {
        block[(get_local_id(1) + j * (TR_TILE_DIM/TR_ELEMS_PER_THREAD)) * (TR_TILE_DIM+1) +
              get_local_id(0)] =
          srcmem[idata_offset + index_i];
      }
    }
  }
  barrier(CLK_LOCAL_MEM_FENCE);
  x_index = get_group_id(1) * TR_TILE_DIM + get_local_id(0);
  y_index = get_group_id(0) * TR_TILE_DIM + get_local_id(1);
  if (x_index < y_elems) {
    for (int64_t j = 0; j < TR_ELEMS_PER_THREAD; j++) {
      int64_t index_out = (y_index + j * (TR_TILE_DIM/TR_ELEMS_PER_THREAD)) * y_elems + x_index;
      if (y_index + j * (TR_TILE_DIM/TR_ELEMS_PER_THREAD) < x_elems) {
        dstmem[(odata_offset + index_out)] =
          block[get_local_id(0) * (TR_TILE_DIM+1) +
                get_local_id(1) + j * (TR_TILE_DIM/TR_ELEMS_PER_THREAD)];
      }
    }
  }
}
