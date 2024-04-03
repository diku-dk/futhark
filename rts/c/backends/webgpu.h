// Start of backends/webgpu.h.

// Synchronous wrapper around asynchronous WebGPU APIs, based on looping with
// emscripten_sleep until the respective callback gets called.

struct wgpu_wait_info {
  bool released;
  void *result;
};

void wgpu_map_sync_callback(WGPUBufferMapAsyncStatus status, void *info_v) {
  wgpu_wait_info *info = (wgpu_wait_info *)info_v;
  *info->result = status;
  info->released = true;
}

WGPUBufferMapAsyncStatus wgpu_map_buffer_sync(WGPUBuffer buffer,
                                              WGPUMapModeFlags mode,
                                              size_t offset, size_t size) {
  WGPUBufferMapAsyncStatus status;
  wgpu_wait_info info = {
    .released = false,
    .result = (void *)&status,
  };

  wgpuBufferMapAsync(buffer, mode, offset, size,
                     wgpu_map_sync_callback, (void *) &info);

  // TODO: Should this do some kind of volatile load?
  // (Same for other _sync wrappers below.)
  while (!info.released) {
    emscripten_sleep(0);
  }

  return status;
}

struct wgpu_request_adapter_result {
  WGPURequestAdapterStatus status;
  WGPUAdapter adapter;
  char *message;
};

void wgpu_request_adapter_callback(WGPURequestAdapterStatus status,
                                   WGPUAdapter adapter,
                                   const char *message, void *userdata) {
  wgpu_wait_info *info = (wgpu_wait_info *)info_v;
  wgpu_request_adapter_result *result
    = (wgpu_request_adapter_result *)info->result;
  result->status = status;
  result->adapter = adapter;
  result->message = message;
  info->released = true;
}

wgpu_request_adapter_result wgpu_request_adapter_sync(
    WGPUInstance instance, WGPURequestAdapterOptions const * options) {
  wgpu_request_adapter_result result = {};
  wgpu_wait_info info = {
    .released = false,
    .result = (void *)&result,
  };

  wgpuInstanceRequestAdapter(instance, options, wgpu_request_adapter_callback,
                             (void *)&info;

  while (!info.released) {
    emscripten_sleep(0);
  }

  return result;
}

struct wgpu_request_device_result {
  WGPURequestDeviceStatus status;
  WGPUDevice device;
  char *message;
};

void wgpu_request_device_callback(WGPURequestDeviceStatus status,
                                   WGPUDevice device,
                                   const char *message, void *userdata) {
  wgpu_wait_info *info = (wgpu_wait_info *)info_v;
  wgpu_request_device_result *result
    = (wgpu_request_device_result *)info->result;
  result->status = status;
  result->device = device;
  result->message = message;
  info->released = true;
}

wgpu_request_device_result wgpu_request_device_sync(
    WGPUAdapter adapter, WGPUDeviceDescriptor const * descriptor) {
  wgpu_request_device_result result = {};
  wgpu_wait_info info = {
    .released = false,
    .result = (void *)&result,
  };

  wgpuAdapterRequestDevice(adapter, descriptor, wgpu_request_device_callback,
                           (void *)&info;

  while (!info.released) {
    emscripten_sleep(0);
  }

  return result;
}

void wgpu_on_uncaptured_error(WGPUErrorType error_type, const char *msg,
                              void *userdata) {
  futhark_panic(-1, "Uncaptured WebGPU error, type: %d\n%s\n", error_type, msg);
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

static void backend_context_config_setup(struct futhark_context_config *cfg) {
  cfg->program = strconcat(gpu_program);
}

static void backend_context_config_teardown(struct futhark_context_config *cfg) {
  free(cfg->program);
}

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
  // TODO: What are constants? Should I be using this overrides/macros?
  struct constants *constants;
  struct free_list free_list;
  struct event_list event_list;
  int64_t peak_mem_usage_default;
  int64_t cur_mem_usage_default;
  struct program* program;
  bool program_initialised;
  // Uniform fields above.

  int num_overrides;
  char **override_names;
  double *override_values;
 
  WGPUInstance instance;
  WGPUAdapter adapter;
  WGPUDevice device;
  WGPUQueue queue;
  // One module contains all the kernels as separate entry points.
  WGPUShaderModule module;

  WGPUBuffer scalar_readback_buffer;
  struct free_list gpu_free_list;

  size_t lockstep_width;

  struct builtin_kernels* kernels;
};

void wgpu_module_setup(struct futhark_context *ctx) {
  WGPUShaderModuleWGSLDescriptor wgsl_desc = {
    .chain = {
      .sType = WGPUSType_ShaderModuleWGSLDescriptor
    },
    .code = ctx->cfg->program
  };
  WGPUShaderModuleDescriptor desc = {
    .nextInChain = &wgsl_desc.chain
  };
  ctx->module = wgpuDeviceCreateShaderModule(ctx->device, &desc);
  // TODO: Report shader errors
  //wgpuShaderModuleGetCompilationInfo(shader_module, on_shader_compiled, NULL);
}

struct builtin_kernels* init_builtin_kernels(struct futhark_context* ctx);
void free_builtin_kernels(struct futhark_context* ctx, struct builtin_kernels* kernels);

int backend_context_setup(struct futhark_context *ctx) {
  ctx->kernels = NULL;

  ctx->instance = wgpuCreateInstance(NULL);

  wgpu_request_adapter_result adapter_result
    = wgpu_request_adapter_sync(ctx->instance, NULL);
  if (adapter_result.status != WGPURequestAdapterStatus_Success) {
    if (adapter_result.message != NULL) {
      futhark_panic(-1, "Could not get WebGPU adapter, status: %d\nMessage: %s\n",
                    adapter_result.status, adapter_result.message);
    } else {
      futhark_panic(-1, "Could not get WebGPU adapter, status: %d\n",
                    adapter_result.status);
    }
  }
  ctx->adapter = adapter_result.adapter;

  wgpu_request_device_result device_result
    = wgpu_request_device_sync(ctx->adapter, NULL);
  if (device_result.status != WGPURequestDeviceStatus_Success) {
    if (device_result.message != NULL) {
      futhark_panic(-1, "Could not get WebGPU device, status: %d\nMessage: %s\n",
                    device_result.status, device_result.message);
    } else {
      futhark_panic(-1, "Could not get WebGPU device, status: %d\n",
                    device_result.status);
    }
  }
  ctx->device = device_result.device;
  wgpuDeviceSetUncapturedErrorCallback(ctx->device,
                                       wgpu_on_uncaptured_error, NULL);

  ctx->queue = wgpuDeviceGetQueue(ctx->device);

  WGPUBufferDescriptor desc = {
    .label = "scalar_readback",
    .size = 8,
    .usage = WGPUBufferUsage_MapRead | WGPUBufferUsage_CopyDst,
  };
  ctx->scalar_readback_buffer = wgpuDeviceCreateBuffer(ctx->device, &desc);
  free_list_init(&ctx->gpu_free_list);

  if ((ctx->kernels = init_builtin_kernels(ctx)) == NULL) {
    return 1;
  }

  // We implement macros as override constants.
  int64_t *macro_vals;
  ctx->num_overrides = gpu_macros(ctx, &ctx->override_names,
                                  &macro_vals);
  ctx->override_vals = malloc(ctx->num_overrides * sizeof(double));
  for (int i = 0; i < ctx->num_overrides; i++) {
    ctx->override_vals[i] = (double) macro_vals[i];
  }
  free(macro_vals);

  wgpu_module_setup(ctx);

  return 0;
}

void backend_context_teardown(struct futhark_context *ctx) {
  if (ctx->kernels != NULL) {
    free_builtin_kernels(ctx, ctx->kernels);
    free(ctx->override_names);
    free(ctx->override_values);

    if (gpu_free_all(ctx) != FUTHARK_SUCCESS) {
      futhark_panic(-1, "gpu_free_all failed");
    }
    wgpuBufferDestroy(ctx->scalar_readback_buffer);
    wgpuDeviceDestroy(ctx->device);
  }
  free_list_destroy(&ctx->gpu_free_list);
}

// Definitions for these are included as part of code generation.
// wgpu_kernel_info contains:
//   char *name;
//   char *entry_point;
//
//   size_t num_args;
//   int8_t *arg_types; // (0 -> scalar, 1 -> binding)
//
//   size_t num_bindings;
//   uint32_t *binding_indices;
//
//   size_t scalars_binding;
//   size_t scalars_size; 
//   size_t *scalar_offsets;
struct wgpu_kernel_info;
size_t wgpu_num_kernel_infos;
wgpu_kernel_info wgpu_kernel_infos[];

struct wgpu_kernel_info *wgpu_get_kernel_info(const char *name) {
  for (int i = 0; i < wgpu_num_kernel_infos; i++) {
    if (strcmp(name, wgpu_kernel_infos[i].name) == 0) {
      return &wgpu_kernel_infos[i];
    }
  }

  return NULL;
}

// GPU ABSTRACTION LAYER

// Types.
struct wgpu_kernel {
  WGPUBindGroupLayout bind_group_layout;
  WGPUPipelineLayout pipeline_layout;
  WGPUComputePipeline pipeline;
  WGPUBuffer scalars_buffer;
};
typedef struct wgpu_kernel* gpu_kernel;
typedef WGPUBuffer gpu_mem;

static void gpu_create_kernel(struct futhark_context *ctx,
                              gpu_kernel *kernel_out,
                              const char *name) {
  if (ctx->debugging) {
    fprintf(ctx->log, "Creating kernel %s.\n", name);
  }

  struct wgpu_kernel_info *kernel_info = wgpu_get_kernel_info(name);
  struct wgpu_kernel *kernel = malloc(sizeof(struct wgpu_kernel));
  
  // Create bind group layout.
  WGPUBindGroupLayoutEntry *bgl_entries
    = calloc(1 + kernel_info->num_bindings, sizeof(WGPUBindGroupLayoutEntry));

  WGPUBindGroupEntry scalar_entry = bgl_entries;
  scalar_entry->binding = kernel_info->scalars_binding;
  scalar_entry->visibility = WGPUShaderStage_COMPUTE;
  scalar_entry->buffer = { .type = WGPUBufferBindingType_Uniform };

  for (int i = 0; i < kernel_info->num_bindings; i++) {
    WGPUBindGroupLayoutEntry *entry = &bgl_entries[1 + i];
    entry->binding = kernel_info->binding_indices[i];
    entry->visibility = WGPUShaderStage_COMPUTE;
    entry->buffer = { .type = WGPUBufferBindingType_Storage };
  }
  WGPUBindGroupLayoutDescriptor bgl_desc = {
    .entryCount = 1 + kernel_info->num_bindings,
    .entries = bgl_entries
  };
  kernel->bind_group_layout
    = wgpuDeviceCreateBindGroupLayout(ctx->device, &bgl_desc);
  free(bgl_entries);

  // Create pipeline layout.
  WGPUPipelineLayoutDescriptor pl = {
    .bindGroupLayoutCount = 1,
    .bindGroupLayouts = &kernel->bind_group_layout,
  };
  kernel->pipeline_layout = wgpuDeviceCreatePipelineLayout(ctx->device, &pl);

  // Create constants / overrides. 
  WGPUConstantEntry *const_entries = calloc(ctx->num_overrides,
                                            sizeof(WGPUConstantEntry));
  for (int i = 0; i < ctx->num_overrides; i++) {
    WGPUConstantEntry *entry = &const_entries[i];
    entry->key = ctx->override_names[i];
    entry->value = ctx->override_values[i];
  }

  // Create pipeline.
  WGPUComputePipelineDescriptor desc = {
    .layout = kernel->pipeline_layout,
    .compute = {
      .module = ctx->module,
      .entryPoint = kernel_info->entry_point,
      .constantCount = ctx->num_overrides,
      .constants = const_entries,
    }
  };
  kernel->pipeline = wgpuDeviceCreateComputePipeline(ctx->device, &desc);
  free(const_entries);

  WGPUBufferDescriptor scalars_desc = {
    .label = "kernel scalars",
    .size = kernel_info->scalars_size,
    .usage = WGPUBufferUsage_Uniform | WGPUBufferUsage_CopyDst
  };
  kernel->scalars_buffer = wgpuDeviceCreateBuffer(ctx->device, &scalars_desc);

  *kernel_out = kernel;
}

static void gpu_free_kernel(struct futhark_context *ctx,
                            gpu_kernel kernel) {
  (void)ctx;
  wgpuBufferDestroy(kernel->scalars_buffer);
  free(kernel);
}

static int gpu_scalar_to_device(struct futhark_context *ctx,
                                gpu_mem dst, size_t offset, size_t size,
                                void *src) {
  wgpuQueueWriteBuffer(ctx->queue, dst, offset, src, size);
}

static int gpu_scalar_from_device(struct futhark_context *ctx,
                                  void *dst,
                                  gpu_mem src, size_t offset, size_t size) {
  if (size > 8) {
    futhark_panic(-1, "gpu_scalar_from_device with size %zu > 8 is not allowed\n",
                  size);
  }

  WGPUCommandEncoder encoder = wgpuDeviceCreateCommandEncoder(ctx->device, NULL);
  wgpuCommandEncoderCopyBufferToBuffer(encoder,
    src, offset,
    ctx->scalar_readback_buffer, 0,
    size);

  WGPUCommandBuffer commandBuffer = wgpuCommandEncoderFinish(encoder, NULL);
  wgpuQueueSubmit(ctx->queue, 1, &commandBuffer);

  WGPUBufferMapAsyncStatus status = 
    wgpu_map_buffer_sync(ctx->scalar_readback_buffer, WGPUMapMode_Read, 0, size);
  if (status != WGPUBufferMapAsyncStatus_Success) {
    futhark_panic(-1, "Failed to read scalar from device memory with error %d\n",
                  status);
  }

  const void *mapped = wgpuBufferGetConstMappedRange(ctx->scalar_readback_buffer,
                                                     0, size);
  memcpy(dst, mapped, size);

  wgpuBufferUnmap(readback);
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

static int memcpy_host2gpu(struct futhark_context *ctx, bool sync,
                           gpu_mem dst, int64_t dst_offset,
                           const unsigned char *src, int64_t src_offset,
                           int64_t nbytes) {
  if (nbytes <= 0) { return FUTHARK_SUCCESS; }

  // There is no async copy to device memory at the moment (the spec for
  // `writeBuffer` specifies that a copy of the data is always made and there is
  // no other good option to use here), so we ignore the sync parameter.
  (void)sync;

  wgpuQueueWriteBuffer(ctx->queue, dst, dst_offset, src, src_offset, nbytes);
}

static int memcpy_gpu2host(struct futhark_context *ctx, bool sync,
                           unsigned char *dst, int64_t dst_offset,
                           gpu_mem src, int64_t src_offset,
                           int64_t nbytes) {
  if (nbytes <= 0) { return FUTHARK_SUCCESS; }

  WGPUBufferDescriptor desc = {
    .label = "tmp_readback",
    .size = nbytes,
    .usage = WGPUBufferUsage_MapRead | WGPUBufferUsage_CopyDst,
  };
  WGPUBuffer readback = wgpuDeviceCreateBuffer(ctx->device, &desc);
  
  WGPUCommandEncoder encoder = wgpuDeviceCreateCommandEncoder(ctx->device, NULL);
  wgpuCommandEncoderCopyBufferToBuffer(encoder,
    src, offset,
    readback, 0,
    nbytes);

  WGPUCommandBuffer commandBuffer = wgpuCommandEncoderFinish(encoder, NULL);
  wgpuQueueSubmit(ctx->queue, 1, &commandBuffer);

  // TODO: Could we do an actual async mapping here if `sync` is false?
  WGPUBufferMapAsyncStatus status = 
    wgpu_map_buffer_sync(readback, WGPUMapMode_Read, 0, nbytes);
  if (status != WGPUBufferMapAsyncStatus_Success) {
    futhark_panic(-1, "Failed to copy from device memory with error %d\n",
                  status);
  }

  const void *mapped = wgpuBufferGetConstMappedRange(readback, 0, nbytes);
  memcpy(dst, mapped, nbytes);

  wgpuBufferUnmap(readback);
  wgpuBufferDestroy(readback);
  return FUTHARK_SUCCESS;
}

static int gpu_launch_kernel(struct futhark_context* ctx,
                             gpu_kernel kernel, const char *name,
                             const int32_t grid[3],
                             const int32_t block[3],
                             unsigned int shared_mem_bytes,
                             int num_args,
                             void* args[num_args],
                             size_t args_sizes[num_args]) {
  // TODO: Deal with `block` not matching what's set in the pipeline constants
  // at creation time. Also, there, deal with no const block size being
  // available at all.

  struct wgpu_kernel_info *kernel_info = wgpu_get_kernel_info(name);

  if (num_args != kernel_info->num_args) {
    futhark_panic(-1, "Kernel %s called with num_args not maching its info\n",
                  name);
  }

  WGPUBindGroupEntry *bg_entries = calloc(1 + kernel_info->num_bindings,
                                          sizeof(WGPUBindGroupEntry));
  void *scalars = malloc(kernel_info->scalars_size);

  int scalar_index = 0;
  int binding_index = 0;
  for (int i = 0; i < num_args; i++) {
    if (kernel_info->arg_types[i] == 0) {
      // Scalar arg
      memcpy(scalars + kernel_info->scalar_offsets[scalar_index],
             args[i], args_sizes[i]);

      scalar_index++;
    }
    else {
      WGPUBindGroupEntry *entry = &bg_entries[1 + binding_index];
      entry->binding = kernel_info->binding_indices[binding_index];
      entry->buffer = (gpu_mem) *args[i];
      // In theory setting (offset, size) to (0, 0) should also work and mean
      // 'the entire buffer', but as of writing this, Firefox requires
      // specifying the size.
      entry->offset = 0;
      entry->size = wgpuBufferGetSize(entry->buffer);

      binding_index++;
    }
  }

  wgpuQueueWriteBuffer(ctx->queue, kernel->scalars_buffer, 0,
                       scalars, kernel_info->scalars_size);

  WGPUBindGroupEntry *scalar_entry = bg_entries;
  scalar_entry->binding = kernel_info->scalars_binding;
  scalar_entry->buffer = kernel->scalars_buffer;
  scalar_entry->offset = 0;
  scalar_entry->size = kernel_info->scalars_size;


  WGPUBindGroupDescriptor bg_desc = {
    .layout = kernel->bind_group_layout,
    .entryCount = 1 + kernel_info->num_bindings,
    .entries = bg_entries,
  };
  WGPUBindGroup bg = wgpuDeviceCreateBindGroup(ctx->device, &bg_desc);

  WGPUCommandEncoder encoder = wgpuDeviceCreateCommandEncoder(ctx->device, NULL);

  WGPUComputePassEncoder pass_encoder
    = wgpuCommandEncoderBeginComputePass(encoder, NULL);
  wgpuComputePassEncoderSetPipeline(pass_encoder, kernel->pipeline);
  wgpuComputePassEncoderSetBindGroup(pass_encoder, 0, bg, 0, NULL);
  wgpuComputePassEncoderDispatchWorkgroups(pass_encoder,
                                           grid[0], grid[1], grid[2]);
  wgpuComputePassEncoderEnd(pass_encoder);

  WGPUCommandBuffer cmd_buffer = wgpuCommandEncoderFinish(encoder, NULL);
  wgpuQueueSubmit(ctx->queue, 1, &cmd_buffer);

  free(scalars);

  return FUTHARK_SUCCESS;
}

static int gpu_alloc_actual(struct futhark_context *ctx,
                            size_t size, gpu_mem *mem_out) {
  WGPUBufferDescriptor desc = {
    .size = size,
    .usage = WGPUBufferUsage_CopySrc
           | WGPUBufferUsage_CopyDst
           | WGPUBufferUsage_Storage,
  };
  *mem_out = wgpuDeviceCreateBuffer(ctx->device, &desc);
  return FUTHARK_SUCCESS;
}

static int gpu_free_actual(struct futhark_context *ctx, gpu_mem mem) {
  (void)ctx;
  wgpuBufferDestroy(mem);
  return FUTHARK_SUCCESS;
}

// End of backends/webgpu.h.
