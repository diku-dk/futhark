// Start of gpu.h

// Generic functions that use our tiny GPU abstraction layer.  The
// entire context must be defined before this header is included.  In
// particular we expect the following functions to be available:

static int gpu_free_actual(struct futhark_context *ctx, gpu_mem mem);
static int gpu_alloc_actual(struct futhark_context *ctx, size_t size, gpu_mem *mem_out);
int gpu_launch_kernel(struct futhark_context* ctx,
                      gpu_kernel kernel, const char *name,
                      const int32_t grid[3],
                      const int32_t block[3],
                      unsigned int shared_mem_bytes,
                      int num_args,
                      void* args[num_args],
                      size_t args_sizes[num_args]);
int gpu_memcpy(struct futhark_context* ctx,
               gpu_mem dst, int64_t dst_offset,
               gpu_mem src, int64_t src_offset,
               int64_t nbytes);
int gpu_scalar_from_device(struct futhark_context* ctx,
                           void *dst,
                           gpu_mem src, size_t offset, size_t size);
int gpu_scalar_to_device(struct futhark_context* ctx,
                         gpu_mem dst, size_t offset, size_t size,
                         void *src);
void gpu_create_kernel(struct futhark_context *ctx,
                       gpu_kernel* kernel,
                       const char* name);

// Max number of thead blocks we allow along the second or third
// dimension for transpositions.
#define MAX_TR_THREAD_BLOCKS 65535

struct builtin_kernels {
  // We have a lot of ways to transpose arrays.
  gpu_kernel map_transpose_1b;
  gpu_kernel map_transpose_1b_low_height;
  gpu_kernel map_transpose_1b_low_width;
  gpu_kernel map_transpose_1b_small;
  gpu_kernel map_transpose_1b_large;
  gpu_kernel map_transpose_2b;
  gpu_kernel map_transpose_2b_low_height;
  gpu_kernel map_transpose_2b_low_width;
  gpu_kernel map_transpose_2b_small;
  gpu_kernel map_transpose_2b_large;
  gpu_kernel map_transpose_4b;
  gpu_kernel map_transpose_4b_low_height;
  gpu_kernel map_transpose_4b_low_width;
  gpu_kernel map_transpose_4b_small;
  gpu_kernel map_transpose_4b_large;
  gpu_kernel map_transpose_8b;
  gpu_kernel map_transpose_8b_low_height;
  gpu_kernel map_transpose_8b_low_width;
  gpu_kernel map_transpose_8b_small;
  gpu_kernel map_transpose_8b_large;

  // And a few ways of copying.
  gpu_kernel lmad_copy_1b;
  gpu_kernel lmad_copy_2b;
  gpu_kernel lmad_copy_4b;
  gpu_kernel lmad_copy_8b;
};

struct builtin_kernels* init_builtin_kernels(struct futhark_context* ctx) {
  struct builtin_kernels *kernels = malloc(sizeof(struct builtin_kernels));
  gpu_create_kernel(ctx, &kernels->map_transpose_1b, "map_transpose_1b");
  gpu_create_kernel(ctx, &kernels->map_transpose_1b_large, "map_transpose_1b_large");
  gpu_create_kernel(ctx, &kernels->map_transpose_1b_low_height, "map_transpose_1b_low_height");
  gpu_create_kernel(ctx, &kernels->map_transpose_1b_low_width, "map_transpose_1b_low_width");
  gpu_create_kernel(ctx, &kernels->map_transpose_1b_small, "map_transpose_1b_small");

  gpu_create_kernel(ctx, &kernels->map_transpose_2b, "map_transpose_2b");
  gpu_create_kernel(ctx, &kernels->map_transpose_2b_large, "map_transpose_2b_large");
  gpu_create_kernel(ctx, &kernels->map_transpose_2b_low_height, "map_transpose_2b_low_height");
  gpu_create_kernel(ctx, &kernels->map_transpose_2b_low_width, "map_transpose_2b_low_width");
  gpu_create_kernel(ctx, &kernels->map_transpose_2b_small, "map_transpose_2b_small");

  gpu_create_kernel(ctx, &kernels->map_transpose_4b, "map_transpose_4b");
  gpu_create_kernel(ctx, &kernels->map_transpose_4b_large, "map_transpose_4b_large");
  gpu_create_kernel(ctx, &kernels->map_transpose_4b_low_height, "map_transpose_4b_low_height");
  gpu_create_kernel(ctx, &kernels->map_transpose_4b_low_width, "map_transpose_4b_low_width");
  gpu_create_kernel(ctx, &kernels->map_transpose_4b_small, "map_transpose_4b_small");

  gpu_create_kernel(ctx, &kernels->map_transpose_8b, "map_transpose_8b");
  gpu_create_kernel(ctx, &kernels->map_transpose_8b_large, "map_transpose_8b_large");
  gpu_create_kernel(ctx, &kernels->map_transpose_8b_low_height, "map_transpose_8b_low_height");
  gpu_create_kernel(ctx, &kernels->map_transpose_8b_low_width, "map_transpose_8b_low_width");
  gpu_create_kernel(ctx, &kernels->map_transpose_8b_small, "map_transpose_8b_small");

  gpu_create_kernel(ctx, &kernels->lmad_copy_1b, "lmad_copy_1b");
  gpu_create_kernel(ctx, &kernels->lmad_copy_2b, "lmad_copy_2b");
  gpu_create_kernel(ctx, &kernels->lmad_copy_4b, "lmad_copy_4b");
  gpu_create_kernel(ctx, &kernels->lmad_copy_8b, "lmad_copy_8b");

  return kernels;
}

void free_builtin_kernels(struct futhark_context* ctx, struct builtin_kernels* kernels) {
  gpu_free_kernel(ctx, kernels->map_transpose_1b);
  gpu_free_kernel(ctx, kernels->map_transpose_1b_large);
  gpu_free_kernel(ctx, kernels->map_transpose_1b_low_height);
  gpu_free_kernel(ctx, kernels->map_transpose_1b_low_width);
  gpu_free_kernel(ctx, kernels->map_transpose_1b_small);

  gpu_free_kernel(ctx, kernels->map_transpose_2b);
  gpu_free_kernel(ctx, kernels->map_transpose_2b_large);
  gpu_free_kernel(ctx, kernels->map_transpose_2b_low_height);
  gpu_free_kernel(ctx, kernels->map_transpose_2b_low_width);
  gpu_free_kernel(ctx, kernels->map_transpose_2b_small);

  gpu_free_kernel(ctx, kernels->map_transpose_4b);
  gpu_free_kernel(ctx, kernels->map_transpose_4b_large);
  gpu_free_kernel(ctx, kernels->map_transpose_4b_low_height);
  gpu_free_kernel(ctx, kernels->map_transpose_4b_low_width);
  gpu_free_kernel(ctx, kernels->map_transpose_4b_small);

  gpu_free_kernel(ctx, kernels->map_transpose_8b);
  gpu_free_kernel(ctx, kernels->map_transpose_8b_large);
  gpu_free_kernel(ctx, kernels->map_transpose_8b_low_height);
  gpu_free_kernel(ctx, kernels->map_transpose_8b_low_width);
  gpu_free_kernel(ctx, kernels->map_transpose_8b_small);

  gpu_free_kernel(ctx, kernels->lmad_copy_1b);
  gpu_free_kernel(ctx, kernels->lmad_copy_2b);
  gpu_free_kernel(ctx, kernels->lmad_copy_4b);
  gpu_free_kernel(ctx, kernels->lmad_copy_8b);

  free(kernels);
}

static int gpu_alloc(struct futhark_context *ctx, FILE *log,
                     size_t min_size, const char *tag,
                     gpu_mem *mem_out, size_t *size_out) {
  if (min_size < sizeof(int)) {
    min_size = sizeof(int);
  }

  gpu_mem* memptr;
  if (free_list_find(&ctx->gpu_free_list, min_size, tag, size_out, (fl_mem*)&memptr) == 0) {
    // Successfully found a free block.  Is it big enough?
    if (*size_out >= min_size) {
      if (ctx->cfg->debugging) {
        fprintf(log, "No need to allocate: Found a block in the free list.\n");
      }
      *mem_out = *memptr;
      free(memptr);
      return FUTHARK_SUCCESS;
    } else {
      if (ctx->cfg->debugging) {
        fprintf(log, "Found a free block, but it was too small.\n");
      }
      int error = gpu_free_actual(ctx, *memptr);
      free(memptr);
      if (error != FUTHARK_SUCCESS) {
        return error;
      }
    }
  }

  *size_out = min_size;

  // We have to allocate a new block from the driver.  If the
  // allocation does not succeed, then we might be in an out-of-memory
  // situation.  We now start freeing things from the free list until
  // we think we have freed enough that the allocation will succeed.
  // Since we don't know how far the allocation is from fitting, we
  // have to check after every deallocation.  This might be pretty
  // expensive.  Let's hope that this case is hit rarely.

  if (ctx->cfg->debugging) {
    fprintf(log, "Actually allocating the desired block.\n");
  }

  int error = gpu_alloc_actual(ctx, min_size, mem_out);

  while (error == FUTHARK_OUT_OF_MEMORY) {
    if (ctx->cfg->debugging) {
      fprintf(log, "Out of GPU memory: releasing entry from the free list...\n");
    }
    gpu_mem* memptr;
    if (free_list_first(&ctx->gpu_free_list, (fl_mem*)&memptr) == 0) {
      gpu_mem mem = *memptr;
      free(memptr);
      error = gpu_free_actual(ctx, mem);
      if (error != FUTHARK_SUCCESS) {
        return error;
      }
    } else {
      break;
    }
    error = gpu_alloc_actual(ctx, min_size, mem_out);
  }

  return error;
}

static int gpu_free(struct futhark_context *ctx,
                    gpu_mem mem, size_t size, const char *tag) {
  gpu_mem* memptr = malloc(sizeof(gpu_mem));
  *memptr = mem;
  free_list_insert(&ctx->gpu_free_list, size, (fl_mem)memptr, tag);
  return FUTHARK_SUCCESS;
}

static int gpu_free_all(struct futhark_context *ctx) {
  free_list_pack(&ctx->gpu_free_list);
  gpu_mem* memptr;
  while (free_list_first(&ctx->gpu_free_list, (fl_mem*)&memptr) == 0) {
    gpu_mem mem = *memptr;
    free(memptr);
    int error = gpu_free_actual(ctx, mem);
    if (error != FUTHARK_SUCCESS) {
      return error;
    }
  }

  return FUTHARK_SUCCESS;
}

static int gpu_map_transpose(struct futhark_context* ctx,
                             gpu_kernel kernel_default,
                             gpu_kernel kernel_low_height,
                             gpu_kernel kernel_low_width,
                             gpu_kernel kernel_small,
                             gpu_kernel kernel_large,
                             const char *name, size_t elem_size,
                             gpu_mem dst, int64_t dst_offset,
                             gpu_mem src, int64_t src_offset,
                             int64_t k, int64_t n, int64_t m) {
  int64_t mulx = TR_BLOCK_DIM / n;
  int64_t muly = TR_BLOCK_DIM / m;
  int32_t mulx32 = mulx;
  int32_t muly32 = muly;
  int32_t k32 = k;
  int32_t n32 = n;
  int32_t m32 = m;

  gpu_kernel kernel = kernel_default;
  int32_t grid[3];
  int32_t block[3];

  void* args[11];
  size_t args_sizes[11] = {
    sizeof(gpu_mem), sizeof(int64_t),
    sizeof(gpu_mem), sizeof(int64_t),
    sizeof(int32_t),
    sizeof(int32_t),
    sizeof(int32_t),
    sizeof(int32_t),
    sizeof(int32_t)
  };

  args[0] = &dst;
  args[1] = &dst_offset;
  args[2] = &src;
  args[3] = &src_offset;
  args[7] = &mulx;
  args[8] = &muly;

  if (dst_offset + k * n * m <= 2147483647L &&
      src_offset + k * n * m <= 2147483647L) {
    if (m <= TR_BLOCK_DIM/2 && n <= TR_BLOCK_DIM/2) {
      if (ctx->logging) { fprintf(ctx->log, "Using small kernel\n"); }
      kernel = kernel_small;
      grid[0] = ((k * n * m) + (TR_BLOCK_DIM*TR_BLOCK_DIM) - 1) / (TR_BLOCK_DIM*TR_BLOCK_DIM);
      grid[1] = 1;
      grid[2] = 1;
      block[0] = TR_BLOCK_DIM*TR_BLOCK_DIM;
      block[1] = 1;
      block[2] = 1;
    } else if (m <= TR_BLOCK_DIM/2 && TR_BLOCK_DIM < n) {
      if (ctx->logging) { fprintf(ctx->log, "Using low-width kernel\n"); }
      kernel = kernel_low_width;
      int64_t x_elems = m;
      int64_t y_elems = (n + muly - 1) / muly;
      grid[0] = (x_elems + TR_BLOCK_DIM - 1) / TR_BLOCK_DIM;
      grid[1] = (y_elems + TR_BLOCK_DIM - 1) / TR_BLOCK_DIM;
      grid[2] = k;
      block[0] = TR_BLOCK_DIM;
      block[1] = TR_BLOCK_DIM;
      block[2] = 1;
    } else if (n <= TR_BLOCK_DIM/2 && TR_BLOCK_DIM < m) {
      if (ctx->logging) { fprintf(ctx->log, "Using low-height kernel\n"); }
      kernel = kernel_low_height;
      int64_t x_elems = (m + mulx - 1) / mulx;
      int64_t y_elems = n;
      grid[0] = (x_elems + TR_BLOCK_DIM - 1) / TR_BLOCK_DIM;
      grid[1] = (y_elems + TR_BLOCK_DIM - 1) / TR_BLOCK_DIM;
      grid[2] = k;
      block[0] = TR_BLOCK_DIM;
      block[1] = TR_BLOCK_DIM;
      block[2] = 1;
    } else {
      if (ctx->logging) { fprintf(ctx->log, "Using default kernel\n"); }
      kernel = kernel_default;
      grid[0] = (m+TR_TILE_DIM-1)/TR_TILE_DIM;
      grid[1] = (n+TR_TILE_DIM-1)/TR_TILE_DIM;
      grid[2] = k;
      block[0] = TR_TILE_DIM;
      block[1] = TR_TILE_DIM/TR_ELEMS_PER_THREAD;
      block[2] = 1;
    }
    args[4] = &k32;
    args[5] = &m32;
    args[6] = &n32;
    args[7] = &mulx32;
    args[8] = &muly32;
  } else {
    if (ctx->logging) { fprintf(ctx->log, "Using large kernel\n"); }
    kernel = kernel_large;
    grid[0] = (m+TR_TILE_DIM-1)/TR_TILE_DIM;
    grid[1] = (n+TR_TILE_DIM-1)/TR_TILE_DIM;
    grid[2] = k;
    block[0] = TR_TILE_DIM;
    block[1] = TR_TILE_DIM/TR_ELEMS_PER_THREAD;
    block[2] = 1;
    args[4] = &k;
    args[5] = &m;
    args[6] = &n;
    args[7] = &mulx;
    args[8] = &muly;
    args_sizes[4] = sizeof(int64_t);
    args_sizes[5] = sizeof(int64_t);
    args_sizes[6] = sizeof(int64_t);
    args_sizes[7] = sizeof(int64_t);
    args_sizes[8] = sizeof(int64_t);
  }

  // Cap the number of thead blocks we launch and figure out how many
  // repeats we need alongside each dimension.
  int32_t repeat_1 = grid[1] / MAX_TR_THREAD_BLOCKS;
  int32_t repeat_2 = grid[2] / MAX_TR_THREAD_BLOCKS;
  grid[1] = repeat_1 > 0 ? MAX_TR_THREAD_BLOCKS : grid[1];
  grid[2] = repeat_2 > 0 ? MAX_TR_THREAD_BLOCKS : grid[2];
  args[9] = &repeat_1;
  args[10] = &repeat_2;
  args_sizes[9] = sizeof(repeat_1);
  args_sizes[10] = sizeof(repeat_2);

  if (ctx->logging) {
    fprintf(ctx->log, "\n");
  }

  return gpu_launch_kernel(ctx, kernel, name, grid, block,
                           TR_TILE_DIM*(TR_TILE_DIM+1)*elem_size,
                           sizeof(args)/sizeof(args[0]), args, args_sizes);
}

#define GEN_MAP_TRANSPOSE_GPU2GPU(NAME, ELEM_TYPE)                      \
  static int map_transpose_gpu2gpu_##NAME                               \
  (struct futhark_context* ctx,                                         \
   gpu_mem dst, int64_t dst_offset,                                     \
   gpu_mem src, int64_t src_offset,                                     \
   int64_t k, int64_t m, int64_t n)                                     \
  {                                                                     \
    return                                                              \
      gpu_map_transpose                                                 \
      (ctx,                                                             \
       ctx->kernels->map_transpose_##NAME,                              \
       ctx->kernels->map_transpose_##NAME##_low_height,                 \
       ctx->kernels->map_transpose_##NAME##_low_width,                  \
       ctx->kernels->map_transpose_##NAME##_small,                      \
       ctx->kernels->map_transpose_##NAME##_large,                      \
       "map_transpose_" #NAME, sizeof(ELEM_TYPE),                       \
       dst, dst_offset, src, src_offset,                                \
       k, n, m);                                                        \
  }

static int gpu_lmad_copy(struct futhark_context* ctx,
                         gpu_kernel kernel, int r,
                         gpu_mem dst, int64_t dst_offset, int64_t dst_strides[r],
                         gpu_mem src, int64_t src_offset, int64_t src_strides[r],
                         int64_t shape[r]) {
  if (r > 8) {
    set_error(ctx, strdup("Futhark runtime limitation:\nCannot copy array of greater than rank 8.\n"));
    return 1;
  }

  int64_t n = 1;
  for (int i = 0; i < r; i++) { n *= shape[i]; }

  void* args[6+(8*3)];
  size_t args_sizes[6+(8*3)];

  args[0] = &dst;
  args_sizes[0] = sizeof(gpu_mem);
  args[1] = &dst_offset;
  args_sizes[1] = sizeof(dst_offset);
  args[2] = &src;
  args_sizes[2] = sizeof(gpu_mem);
  args[3] = &src_offset;
  args_sizes[3] = sizeof(src_offset);
  args[4] = &n;
  args_sizes[4] = sizeof(n);
  args[5] = &r;
  args_sizes[5] = sizeof(r);

  int64_t zero = 0;

  for (int i = 0; i < 8; i++) {
    args_sizes[6+i*3] = sizeof(int64_t);
    args_sizes[6+i*3+1] = sizeof(int64_t);
    args_sizes[6+i*3+2] = sizeof(int64_t);
    if (i < r) {
      args[6+i*3] = &shape[i];
      args[6+i*3+1] = &dst_strides[i];
      args[6+i*3+2] = &src_strides[i];
    } else {
      args[6+i*3] = &zero;
      args[6+i*3+1] = &zero;
      args[6+i*3+2] = &zero;
    }
  }
  const size_t w = 256; // XXX: hardcoded thread block size.

  return gpu_launch_kernel(ctx, kernel, "copy_lmad_dev_to_dev",
                           (const int32_t[3]) {(n+w-1)/w,1,1},
                           (const int32_t[3]) {w,1,1},
                           0, 6+(8*3), args, args_sizes);
}

#define GEN_LMAD_COPY_ELEMENTS_GPU2GPU(NAME, ELEM_TYPE)                 \
  static int lmad_copy_elements_gpu2gpu_##NAME                          \
  (struct futhark_context* ctx,                                         \
   int r,                                                               \
   gpu_mem dst, int64_t dst_offset, int64_t dst_strides[r],             \
   gpu_mem src, int64_t src_offset, int64_t src_strides[r],             \
   int64_t shape[r]) {                                                  \
    return gpu_lmad_copy(ctx, ctx->kernels->lmad_copy_##NAME, r,        \
                         dst, dst_offset, dst_strides,                  \
                         src, src_offset, src_strides,                  \
                         shape);                                        \
  }                                                                     \

#define GEN_LMAD_COPY_GPU2GPU(NAME, ELEM_TYPE)                          \
  static int lmad_copy_gpu2gpu_##NAME                                   \
  (struct futhark_context* ctx,                                         \
   int r,                                                               \
   gpu_mem dst, int64_t dst_offset, int64_t dst_strides[r],             \
   gpu_mem src, int64_t src_offset, int64_t src_strides[r],             \
   int64_t shape[r]) {                                                  \
    log_copy(ctx, "GPU to GPU", r, dst_offset, dst_strides,             \
             src_offset, src_strides, shape);                           \
    int64_t size = 1;                                                   \
    for (int i = 0; i < r; i++) { size *= shape[i]; }                   \
    if (size == 0) { return FUTHARK_SUCCESS; }                          \
    int64_t k, n, m;                                                    \
    if (lmad_map_tr(&k, &n, &m,                                         \
                       r, dst_strides, src_strides, shape)) {           \
      log_transpose(ctx, k, n, m);                                      \
      return map_transpose_gpu2gpu_##NAME                               \
        (ctx, dst, dst_offset, src, src_offset, k, n, m);               \
    } else if (lmad_memcpyable(r, dst_strides, src_strides, shape)) {   \
      if (ctx->logging) {fprintf(ctx->log, "## Flat copy\n\n");}        \
      return gpu_memcpy(ctx,                                            \
                        dst, dst_offset*sizeof(ELEM_TYPE),              \
                        src, src_offset*sizeof(ELEM_TYPE),              \
                        size * sizeof(ELEM_TYPE));                      \
    } else {                                                            \
      if (ctx->logging) {fprintf(ctx->log, "## General copy\n\n");}     \
      return lmad_copy_elements_gpu2gpu_##NAME                          \
        (ctx, r,                                                        \
         dst, dst_offset, dst_strides,                                  \
         src, src_offset, src_strides,                                  \
         shape);                                                        \
    }                                                                   \
  }

static int
lmad_copy_elements_host2gpu(struct futhark_context *ctx, size_t elem_size,
                            int r,
                            gpu_mem dst, int64_t dst_offset, int64_t dst_strides[r],
                            unsigned char* src, int64_t src_offset, int64_t src_strides[r],
                            int64_t shape[r]) {
  (void)ctx; (void)elem_size; (void)r;
  (void)dst; (void)dst_offset; (void)dst_strides;
  (void)src; (void)src_offset; (void)src_strides;
  (void)shape;
  set_error(ctx, strdup("Futhark runtime limitation:\nCannot copy unstructured array from host to GPU.\n"));
  return 1;
}

static int
lmad_copy_elements_gpu2host (struct futhark_context *ctx, size_t elem_size,
                             int r,
                             unsigned char* dst, int64_t dst_offset, int64_t dst_strides[r],
                             gpu_mem src, int64_t src_offset, int64_t src_strides[r],
                             int64_t shape[r]) {
  (void)ctx; (void)elem_size; (void)r;
  (void)dst; (void)dst_offset; (void)dst_strides;
  (void)src; (void)src_offset; (void)src_strides;
  (void)shape;
  set_error(ctx, strdup("Futhark runtime limitation:\nCannot copy unstructured array from GPU to host.\n"));
  return 1;
}

#define GEN_LMAD_COPY_ELEMENTS_HOSTGPU(NAME, ELEM_TYPE)                 \
  static int lmad_copy_elements_gpu2gpu_##NAME                          \
  (struct futhark_context* ctx,                                         \
   int r,                                                               \
   gpu_mem dst, int64_t dst_offset, int64_t dst_strides[r],             \
   gpu_mem src, int64_t src_offset, int64_t src_strides[r],             \
   int64_t shape[r]) {                                                  \
    return (ctx, ctx->kernels->lmad_copy_##NAME, r,                     \
                         dst, dst_offset, dst_strides,                  \
                         src, src_offset, src_strides,                  \
                         shape);                                        \
  }                                                                     \


static int lmad_copy_host2gpu(struct futhark_context* ctx, size_t elem_size, bool sync,
                              int r,
                              gpu_mem dst, int64_t dst_offset, int64_t dst_strides[r],
                              unsigned char* src, int64_t src_offset, int64_t src_strides[r],
                              int64_t shape[r]) {
  log_copy(ctx, "Host to GPU", r, dst_offset, dst_strides,
           src_offset, src_strides, shape);
  int64_t size = elem_size;
  for (int i = 0; i < r; i++) { size *= shape[i]; }
  if (size == 0) { return FUTHARK_SUCCESS; }
  int64_t k, n, m;
  if (lmad_memcpyable(r, dst_strides, src_strides, shape)) {
    if (ctx->logging) {fprintf(ctx->log, "## Flat copy\n\n");}
    return memcpy_host2gpu(ctx, sync,
                           dst, dst_offset*elem_size,
                           src, src_offset*elem_size,
                           size);
  } else {
    if (ctx->logging) {fprintf(ctx->log, "## General copy\n\n");}
    int error;
    error = lmad_copy_elements_host2gpu
      (ctx, elem_size, r,
       dst, dst_offset, dst_strides,
       src, src_offset, src_strides,
       shape);
    if (error == 0 && sync) {
      error = futhark_context_sync(ctx);
    }
    return error;
  }
}

static int lmad_copy_gpu2host(struct futhark_context* ctx, size_t elem_size, bool sync,
                              int r,
                              unsigned char* dst, int64_t dst_offset, int64_t dst_strides[r],
                              gpu_mem src, int64_t src_offset, int64_t src_strides[r],
                              int64_t shape[r]) {
  log_copy(ctx, "Host to GPU", r, dst_offset, dst_strides,
           src_offset, src_strides, shape);
  int64_t size = elem_size;
  for (int i = 0; i < r; i++) { size *= shape[i]; }
  if (size == 0) { return FUTHARK_SUCCESS; }
  int64_t k, n, m;
  if (lmad_memcpyable(r, dst_strides, src_strides, shape)) {
    if (ctx->logging) {fprintf(ctx->log, "## Flat copy\n\n");}
    return memcpy_gpu2host(ctx, sync,
                           dst, dst_offset*elem_size,
                           src, src_offset*elem_size,
                           size);
  } else {
    if (ctx->logging) {fprintf(ctx->log, "## General copy\n\n");}
    int error;
    error = lmad_copy_elements_gpu2host
      (ctx, elem_size, r,
       dst, dst_offset, dst_strides,
       src, src_offset, src_strides,
       shape);
    if (error == 0 && sync) {
      error = futhark_context_sync(ctx);
    }
    return error;
  }
}

GEN_MAP_TRANSPOSE_GPU2GPU(1b, uint8_t)
GEN_MAP_TRANSPOSE_GPU2GPU(2b, uint16_t)
GEN_MAP_TRANSPOSE_GPU2GPU(4b, uint32_t)
GEN_MAP_TRANSPOSE_GPU2GPU(8b, uint64_t)

GEN_LMAD_COPY_ELEMENTS_GPU2GPU(1b, uint8_t)
GEN_LMAD_COPY_ELEMENTS_GPU2GPU(2b, uint16_t)
GEN_LMAD_COPY_ELEMENTS_GPU2GPU(4b, uint32_t)
GEN_LMAD_COPY_ELEMENTS_GPU2GPU(8b, uint64_t)

GEN_LMAD_COPY_GPU2GPU(1b, uint8_t)
GEN_LMAD_COPY_GPU2GPU(2b, uint16_t)
GEN_LMAD_COPY_GPU2GPU(4b, uint32_t)
GEN_LMAD_COPY_GPU2GPU(8b, uint64_t)

// End of gpu.h
