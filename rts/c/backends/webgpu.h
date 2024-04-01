// Start of backends/webgpu.h.

struct wgpu_map_sync_info {
  bool released;
  WGPUBufferMapAsyncStatus status;
};

void wgpu_map_sync_callback(WGPUBufferMapAsyncStatus status, void *info_v) {
  wgpu_map_sync_info *info = (wgpu_map_sync_info *)info_v;
  info->status = status;
  info->released = true;
}

WGPUBufferMapAsyncStatus wgpu_map_buffer_sync(WGPUBuffer buffer,
                                              WGPUMapModeFlags mode,
                                              size_t offset, size_t size) {
  wgpu_map_sync_info info = {};

  wgpuBufferMapAsync(buffer, mode, offset, size,
                     wgpu_map_sync_callback, (void *) &info);

  // TODO: Should this do some kind of volatile load?
  while (!info.released) {
    emscripten_sleep(0);
  }

  return info.status;
}

struct futhark_context_config {
  int in_use;
  int debugging;
  int profiling;
  int logging;
  char *cache_fname;
  int num_tuning_params;
  int64_t *tuning_params;
  const char** tuning_param_names;
  const char** tuning_param_vars;
  const char** tuning_param_classes;
  // Uniform fields above.
  
  char *program;
};

const char* futhark_context_config_get_program(struct futhark_context_config *cfg) {
  return cfg->program;
}

void futhark_context_config_set_program(struct futhark_context_config *cfg, const char *s) {
  free(cfg->program);
  cfg->program = strdup(s);
}

struct futhark_context {
  struct futhark_context_config* cfg;
  int detail_memory;
  int debugging;
  int profiling;
  int profiling_paused;
  int logging;
  lock_t lock;
  char *error;
  lock_t error_lock;
  FILE *log;
  struct constants *constants;
  struct free_list free_list;
  struct event_list event_list;
  int64_t peak_mem_usage_default;
  int64_t cur_mem_usage_default;
  struct program* program;
  bool program_initialised;
  // Uniform fields above.
 
  WGPUInstance instance;
  WGPUAdapter adapter;
  WGPUDevice device;
  WGPUQueue queue;
  // One module contains all the kernels as separate entry points.
  WGPUShaderModule module;

  struct free_list gpu_free_list;

  size_t lockstep_width;

  struct builtin_kernels* kernels;
};

// GPU ABSTRACTION LAYER

// Types.
struct wgpu_kernel {
  WGPUBindGroupLayout bind_group_layout;
  WGPUPipelineLayout pipeline_layout;
  WGPUComputePipeline pipeline;
};
typedef struct wgpu_kernel* gpu_kernel;
typedef WGPUBuffer gpu_mem;

static void gpu_create_kernel(struct futhark_context *ctx,
                              gpu_kernel *kernel,
                              const char *name) {
  if (ctx->debugging) {
    fprintf(ctx->log, "Creating kernel %s.\n", name);
  }

  // TODO: Need layout information here to create pipeline etc.
}

static void gpu_free_kernel(struct futhark_context *ctx,
                            gpu_kernel kernel) {
  (void)ctx;
  (void)kernel;
}

static int gpu_scalar_to_device(struct futhark_context *ctx,
                                gpu_mem dst, size_t offset, size_t size,
                                void *src) {
  wgpuQueueWriteBuffer(ctx->queue, dst, offset, src, size);
}

static int gpu_scalar_from_device(struct futhark_context *ctx,
                                  void *dst,
                                  gpu_mem src, size_t offset, size_t size) {
  // TODO: It would probably be nice to re-use a buffer here instead.
  // Is there a guarantee for a maximum size when the _scalar_ functions are
  // used?
  WGPUBufferDescriptor desc = {
    .label = "tmp_readback",
    .size = size,
    .usage = WGPUBufferUsage_MapRead | WGPUBufferUsage_CopyDst,
  };
  WGPUBuffer readback = wgpuDeviceCreateBuffer(ctx->device, &desc);
  
  WGPUCommandEncoder encoder = wgpuDeviceCreateCommandEncoder(ctx->device, NULL);
  wgpuCommandEncoderCopyBufferToBuffer(encoder,
    src, offset,
    readback, 0,
    size);

  WGPUCommandBuffer commandBuffer = wgpuCommandEncoderFinish(encoder, NULL);
  wgpuQueueSubmit(ctx->queue, 1, &commandBuffer);


  WGPUBufferMapAsyncStatus status = 
    wgpu_map_buffer_sync(readback, WGPUMapMode_Read, 0, size);
  if (status != WGPUBufferMapAsyncStatus_Success) {
    futhark_panic(-1, "Failed to read scalar from device memory with error %d\n",
                  status);
  }

  const void *mapped = wgpuBufferGetConstMappedRange(readback, 0, size);
  memcpy(dst, mapped, size);

  wgpuBufferUnmap(readback);
  wgpuBufferDestroy(readback);
  return FUTHARK_SUCCESS;
}

static int gpu_memcpy(struct futhark_context *ctx,
                      gpu_mem dst, int64_t dst_offset,
                      gpu_mem src, int64_t src_offset,
                      int64_t nbytes) {
  WGPUCommandEncoder encoder = wgpuDeviceCreateCommandEncoder(ctx->device, NULL);
  wgpuCommandEncoderCopyBufferToBuffer(encoder,
      src, src_offset, dst, dst_offset, nbytes);
  WGPUCommandBuffer commandBuffer = wgpuCommandEncoderFinish(encoder, NULL);
  wgpuQueueSubmit(ctx->queue, 1, &commandBuffer);
  return FUTHARK_SUCCESS:
}

// End of backends/webgpu.h.
