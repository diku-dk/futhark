// Start of backends/webgpu.h.

// Synchronous wrapper around asynchronous WebGPU APIs, based on looping with
// emscripten_sleep until the respective callback gets called.

typedef struct wgpu_wait_info {
  bool released;
  void *result;
} wgpu_wait_info;

void wgpu_map_sync_callback(WGPUBufferMapAsyncStatus status, void *info_v) {
  wgpu_wait_info *info = (wgpu_wait_info *)info_v;
  *((WGPUBufferMapAsyncStatus *) info->result) = status;
  info->released = true;
}

WGPUBufferMapAsyncStatus wgpu_map_buffer_sync(WGPUInstance instance,
                                              WGPUBuffer buffer,
                                              WGPUMapModeFlags mode,
                                              size_t offset, size_t size) {
  WGPUBufferMapAsyncStatus status;
  wgpu_wait_info info = {
    .released = false,
    .result = (void *)&status,
  };

#ifdef USE_DAWN
  WGPUBufferMapCallbackInfo cb_info = {
    .mode = WGPUCallbackMode_WaitAnyOnly,
    .callback = wgpu_map_sync_callback,
    .userdata = (void *) &info,
  };
  WGPUFuture f = wgpuBufferMapAsyncF(buffer, mode, offset, size, cb_info);
  WGPUFutureWaitInfo f_info = { .future = f };
  while (!info.released) {
    wgpuInstanceWaitAny(instance, 1, &f_info, 0);
  }
#else
  wgpuBufferMapAsync(buffer, mode, offset, size,
                     wgpu_map_sync_callback, (void *) &info);

  // TODO: Should this do some kind of volatile load?
  // (Same for other _sync wrappers below.)
  while (!info.released) {
    emscripten_sleep(0);
  }
#endif

  return status;
}

typedef struct wgpu_request_adapter_result {
  WGPURequestAdapterStatus status;
  WGPUAdapter adapter;
  const char *message;
} wgpu_request_adapter_result;

void wgpu_request_adapter_callback(WGPURequestAdapterStatus status,
                                   WGPUAdapter adapter,
                                   const char *message, void *userdata) {
  wgpu_wait_info *info = (wgpu_wait_info *)userdata;
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

#ifdef USE_DAWN
  WGPURequestAdapterCallbackInfo cb_info = {
    .mode = WGPUCallbackMode_WaitAnyOnly,
    .callback = wgpu_request_adapter_callback,
    .userdata = (void *) &info,
  };
  WGPUFuture f = wgpuInstanceRequestAdapterF(instance, options, cb_info);
  WGPUFutureWaitInfo f_info = { .future = f };
  while (!info.released) {
    wgpuInstanceWaitAny(instance, 1, &f_info, 0);
  }
#else
  wgpuInstanceRequestAdapter(instance, options, wgpu_request_adapter_callback,
                             (void *)&info);

  while (!info.released) {
    emscripten_sleep(0);
  }
#endif

  return result;
}

typedef struct wgpu_request_device_result {
  WGPURequestDeviceStatus status;
  WGPUDevice device;
  const char *message;
} wgpu_request_device_result;

void wgpu_request_device_callback(WGPURequestDeviceStatus status,
                                   WGPUDevice device,
                                   const char *message, void *userdata) {
  wgpu_wait_info *info = (wgpu_wait_info *)userdata;
  wgpu_request_device_result *result
    = (wgpu_request_device_result *)info->result;
  result->status = status;
  result->device = device;
  result->message = message;
  info->released = true;
}

wgpu_request_device_result wgpu_request_device_sync(
    WGPUInstance instance,
    WGPUAdapter adapter,
    WGPUDeviceDescriptor const * descriptor
) {
  wgpu_request_device_result result = {};
  wgpu_wait_info info = {
    .released = false,
    .result = (void *)&result,
  };

#ifdef USE_DAWN
  WGPURequestDeviceCallbackInfo cb_info = {
    .mode = WGPUCallbackMode_WaitAnyOnly,
    .callback = wgpu_request_device_callback,
    .userdata = (void *) &info,
  };
  WGPUFuture f = wgpuAdapterRequestDeviceF(adapter, descriptor, cb_info);
  WGPUFutureWaitInfo f_info = { .future = f };
  while (!info.released) {
    wgpuInstanceWaitAny(instance, 1, &f_info, 0);
  }
#else
  wgpuAdapterRequestDevice(adapter, descriptor, wgpu_request_device_callback,
                           (void *)&info);

  while (!info.released) {
    emscripten_sleep(0);
  }
#endif

  return result;
}

void wgpu_on_work_done_callback(WGPUQueueWorkDoneStatus status,
                                void *userdata) {
  wgpu_wait_info *info = (wgpu_wait_info *)userdata;
  *((WGPUQueueWorkDoneStatus *)info->result) = status;
  info->released = true;
}

WGPUQueueWorkDoneStatus wgpu_block_until_work_done(WGPUInstance instance,
                                                   WGPUQueue queue) {
  WGPUQueueWorkDoneStatus status;
  wgpu_wait_info info = {
    .released = false,
    .result = (void *)&status,
  };


#ifdef USE_DAWN
  WGPUQueueWorkDoneCallbackInfo cb_info = {
    .mode = WGPUCallbackMode_WaitAnyOnly,
    .callback = wgpu_on_work_done_callback,
    .userdata = (void *) &info,
  };
  WGPUFuture f = wgpuQueueOnSubmittedWorkDoneF(queue, cb_info);
  WGPUFutureWaitInfo f_info = { .future = f };
  while (!info.released) {
    wgpuInstanceWaitAny(instance, 1, &f_info, 0);
  }
#else
  wgpuQueueOnSubmittedWorkDone(queue, wgpu_on_work_done_callback, (void *)&info);

  while (!info.released) {
    emscripten_sleep(0);
  }
#endif

  return status;
}

void wgpu_on_uncaptured_error(WGPUErrorType error_type, const char *msg,
                              void *userdata) {
  futhark_panic(-1, "Uncaptured WebGPU error, type: %d\n%s\n", error_type, msg);
}

void wgpu_on_shader_compiled(WGPUCompilationInfoRequestStatus status,
                             struct WGPUCompilationInfo const * compilationInfo,
                             void * userdata) {
  // TODO: Check status, better printing
  for (int i = 0; i < compilationInfo->messageCount; i++) {
    WGPUCompilationMessage msg = compilationInfo->messages[i];
    printf("Shader compilation message: %s\n", msg.message);
  }
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

  struct gpu_config gpu;
};

static void backend_context_config_setup(struct futhark_context_config *cfg) {
  cfg->program = strconcat(gpu_program);

  cfg->gpu.default_block_size = 256;
  cfg->gpu.default_grid_size = 0; // Set properly later.
  cfg->gpu.default_tile_size = 32;
  cfg->gpu.default_reg_tile_size = 2;
  cfg->gpu.default_threshold = 32*1024;

  cfg->gpu.default_block_size_changed = 0;
  cfg->gpu.default_grid_size_changed = 0;
  cfg->gpu.default_tile_size_changed = 0;
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
  struct constants *constants;
  struct free_list free_list;
  struct event_list event_list;
  int64_t peak_mem_usage_default;
  int64_t cur_mem_usage_default;
  struct program* program;
  bool program_initialised;
  // Uniform fields above.

  // True if a potentially failing kernel has been enqueued.
  int32_t failure_is_an_option;
  int total_runs;
  long int total_runtime;
  int64_t peak_mem_usage_device;
  int64_t cur_mem_usage_device;

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
  size_t max_thread_block_size;
  size_t max_grid_size;
  size_t max_tile_size;
  size_t max_threshold;
  size_t max_shared_memory;
  size_t max_registers;
  size_t max_cache;

  struct builtin_kernels* kernels;
};

int futhark_context_sync(struct futhark_context *ctx) {
  // TODO: All the error handling stuff.
  WGPUQueueWorkDoneStatus status = wgpu_block_until_work_done(ctx->instance,
                                                              ctx->queue);
  if (status != WGPUQueueWorkDoneStatus_Success) {
    futhark_panic(-1, "Failed to wait for work to be done, status: %d\n",
                  status);
  }
  return FUTHARK_SUCCESS;
}

static void wgpu_size_setup(struct futhark_context *ctx) {
  struct futhark_context_config *cfg = ctx->cfg;
  // TODO: Deal with the device limits here, see cuda.h.

  // TODO: See if we can also do some proper heuristic for default_grid_size
  // here.
  if (!cfg->gpu.default_grid_size_changed) {
    cfg->gpu.default_grid_size = 16;
  }

  for (int i = 0; i < cfg->num_tuning_params; i++) {
    const char *size_class = cfg->tuning_param_classes[i];
    int64_t *size_value = &cfg->tuning_params[i];
    const char* size_name = cfg->tuning_param_names[i];
    //int64_t max_value = 0;
    int64_t default_value = 0;

    if (strstr(size_class, "thread_block_size") == size_class) {
      //max_value = ctx->max_thread_block_size;
      default_value = cfg->gpu.default_block_size;
    } else if (strstr(size_class, "grid_size") == size_class) {
      //max_value = ctx->max_grid_size;
      default_value = cfg->gpu.default_grid_size;
      // XXX: as a quick and dirty hack, use twice as many threads for
      // histograms by default.  We really should just be smarter
      // about sizes somehow.
      if (strstr(size_name, ".seghist_") != NULL) {
        default_value *= 2;
      }
    } else if (strstr(size_class, "tile_size") == size_class) {
      //max_value = ctx->max_tile_size;
      default_value = cfg->gpu.default_tile_size;
    } else if (strstr(size_class, "reg_tile_size") == size_class) {
      //max_value = 0; // No limit.
      default_value = cfg->gpu.default_reg_tile_size;
    } else if (strstr(size_class, "shared_memory") == size_class) {
      default_value = ctx->max_shared_memory;
    } else if (strstr(size_class, "cache") == size_class) {
      default_value = ctx->max_cache;
    } else if (strstr(size_class, "threshold") == size_class) {
      // Threshold can be as large as it takes.
      default_value = cfg->gpu.default_threshold;
    } else {
      // Bespoke sizes have no limit or default.
    }

    if (*size_value == 0) {
      *size_value = default_value;
    //} else if (max_value > 0 && *size_value > max_value) {
    //  fprintf(stderr, "Note: Device limits %s to %zu (down from %zu)\n",
    //          size_name, max_value, *size_value);
    //  *size_value = max_value;
    }
  }
}

void wgpu_module_setup(struct futhark_context *ctx, const char *program, WGPUShaderModule *module, const char* label) {
  WGPUShaderModuleWGSLDescriptor wgsl_desc = {
    .chain = {
      .sType = WGPUSType_ShaderModuleWGSLDescriptor
    },
    .code = program
  };
  WGPUShaderModuleDescriptor desc = {
    .label = label,
    .nextInChain = &wgsl_desc.chain
  };
  *module = wgpuDeviceCreateShaderModule(ctx->device, &desc);

  wgpuShaderModuleGetCompilationInfo(*module, wgpu_on_shader_compiled, NULL);
}

struct builtin_kernels* init_builtin_kernels(struct futhark_context* ctx);
void free_builtin_kernels(struct futhark_context* ctx, struct builtin_kernels* kernels);

int backend_context_setup(struct futhark_context *ctx) {
  ctx->failure_is_an_option = 0;
  ctx->total_runs = 0;
  ctx->total_runtime = 0;
  ctx->peak_mem_usage_device = 0;
  ctx->cur_mem_usage_device = 0;
  ctx->kernels = NULL;

  // These are the default limits from the spec, which will always be the actual
  // limit unless we explicitly request a larger one (which we do not currently
  // do).
  ctx->max_thread_block_size = 256;
  ctx->max_grid_size = 65536; // TODO: idk what these should be, just put a large enough value.
  ctx->max_tile_size = 65536; // TODO: idk what these should be, just put a large enough value.
  ctx->max_threshold = 65536; // TODO: idk what these should be, just put a large enough value.
  ctx->max_shared_memory = 16384;

  ctx->max_registers = 65536; // TODO: idk what these should be, just put a large enough value.

  // This is a number we picked semi-arbitrarily (2 MiB). There does not seem to
  // be a way to get L2 cache size from the WebGPU API.
  ctx->max_cache = 2097152;

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

  // We want to request the max limits possible.
  // Some limits, like maxStorageBuffersPerShaderStage has a huge impact
  // on what programs we can run, so we need to request the maximum possible.
  WGPUSupportedLimits supported;
  WGPUBool res = wgpuAdapterGetLimits(ctx->adapter, &supported);
  if (!res) {
    futhark_panic(-1, "Could not get WebGPU adapter limits\n", res);
  }
  WGPURequiredLimits required_limits;
  // If we just zero this memory, stuff crashes in the generated empscripten js.
  // For some reason, we have to set it to all 1s?
  memset((void*)&required_limits.limits, 0xff, sizeof(required_limits.limits));
  required_limits.limits.maxBindGroups = supported.limits.maxBindGroups;
  required_limits.limits.maxBindingsPerBindGroup = supported.limits.maxBindingsPerBindGroup;
  required_limits.limits.maxDynamicUniformBuffersPerPipelineLayout = supported.limits.maxDynamicUniformBuffersPerPipelineLayout;
  required_limits.limits.maxDynamicStorageBuffersPerPipelineLayout = supported.limits.maxDynamicStorageBuffersPerPipelineLayout;
  required_limits.limits.maxStorageBuffersPerShaderStage = supported.limits.maxStorageBuffersPerShaderStage;
  required_limits.limits.maxUniformBuffersPerShaderStage = supported.limits.maxUniformBuffersPerShaderStage;
  required_limits.limits.maxUniformBufferBindingSize = supported.limits.maxUniformBufferBindingSize;
  required_limits.limits.maxStorageBufferBindingSize = supported.limits.maxStorageBufferBindingSize;
  required_limits.limits.maxBufferSize = supported.limits.maxBufferSize;
  required_limits.limits.maxComputeWorkgroupStorageSize = supported.limits.maxComputeWorkgroupStorageSize;
  required_limits.limits.maxComputeInvocationsPerWorkgroup = supported.limits.maxComputeInvocationsPerWorkgroup;
  required_limits.limits.maxComputeWorkgroupSizeX = supported.limits.maxComputeWorkgroupSizeX;
  required_limits.limits.maxComputeWorkgroupSizeY = supported.limits.maxComputeWorkgroupSizeY;
  required_limits.limits.maxComputeWorkgroupSizeZ = supported.limits.maxComputeWorkgroupSizeZ;
  required_limits.limits.maxComputeWorkgroupsPerDimension = supported.limits.maxComputeWorkgroupsPerDimension;

  // Require support for 16-bit floats
  WGPUFeatureName required_features[] = { WGPUFeatureName_ShaderF16 };
  WGPUDeviceDescriptor device_desc = {
    .requiredFeatureCount = 1,
    .requiredFeatures = required_features,
    .requiredLimits = &required_limits
  };
  wgpu_request_device_result device_result
    = wgpu_request_device_sync(ctx->instance, ctx->adapter, &device_desc);
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

  wgpu_size_setup(ctx);

  WGPUBufferDescriptor desc = {
    .label = "scalar_readback",
    .size = 8,
    .usage = WGPUBufferUsage_MapRead | WGPUBufferUsage_CopyDst,
  };
  ctx->scalar_readback_buffer = wgpuDeviceCreateBuffer(ctx->device, &desc);
  free_list_init(&ctx->gpu_free_list);

  // We implement macros as override constants.
  int64_t *macro_vals;
  ctx->num_overrides = gpu_macros(ctx, &ctx->override_names,
                                  &macro_vals);
  ctx->override_values = malloc(ctx->num_overrides * sizeof(double));
  for (int i = 0; i < ctx->num_overrides; i++) {
    ctx->override_values[i] = (double) macro_vals[i];
  }
  free(macro_vals);

  wgpu_module_setup(ctx, ctx->cfg->program, &ctx->module, "Futhark program");

  if ((ctx->kernels = init_builtin_kernels(ctx)) == NULL) {
    printf("Failed to init builtin kernels\n");
    return 1;
  }

  return 0;
}

void backend_context_teardown(struct futhark_context *ctx) {
  if (ctx->kernels != NULL) {
    free_builtin_kernels(ctx, ctx->kernels);
  } // TODO
    free(ctx->override_names);
    free(ctx->override_values);

    if (gpu_free_all(ctx) != FUTHARK_SUCCESS) {
      futhark_panic(-1, "gpu_free_all failed");
    }
    wgpuBufferDestroy(ctx->scalar_readback_buffer);
    wgpuDeviceDestroy(ctx->device);
  //}
  free_list_destroy(&ctx->gpu_free_list);
}

// Definitions for these are included as part of code generation.
// wgpu_kernel_info contains:
//   char *name;
//
//   size_t num_scalars;
//   size_t scalars_binding;
//   size_t scalars_size;
//   size_t *scalar_offsets;
//
//   size_t num_bindings; // excluding the scalars binding
//   uint32_t *binding_indices;
//
//   size_t num_overrides;
//   char **used_overrides;

//   size_t num_dynamic_block_dims;
//   uint32_t *dynamic_block_dim_indices;
//   char **dynamic_block_dim_names;
//
//   size_t num_shared_mem_overrides;
//   char **shared_mem_overrides;
struct wgpu_kernel_info;
static size_t wgpu_num_kernel_infos;
static wgpu_kernel_info wgpu_kernel_infos[];

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
  struct wgpu_kernel_info *info;

  WGPUBuffer scalars_buffer;
  WGPUBindGroupLayout bind_group_layout;
  WGPUPipelineLayout pipeline_layout;

  // True if we can create a single pipeline in `gpu_create_kernel`. If false,
  // need to create a new pipeline for every kernel launch.
  bool static_pipeline;

  // ShaderModule for this kernel. Generated from the gpu_program of the kernel
  // info.
  WGPUShaderModule module;

  // Only set if static_pipeline.
  WGPUComputePipeline pipeline;

  // Only set if !static_pipeline.
  WGPUConstantEntry *const_entries;

  // How many entries are already set; there is enough space in the allocation
  // to additionally set the shared memory and dynamic block dimension entries.
  int const_entries_set;
};
typedef struct wgpu_kernel* gpu_kernel;
typedef WGPUBuffer gpu_mem;

static int gpu_alloc_actual(struct futhark_context *ctx,
  size_t size, gpu_mem *mem_out) {
  // Storage buffers bindings must have an effective size that is amultiple of
  // 4, so we round up all allocations.
  size = ((size + 4 - 1) / 4) * 4;
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

static void gpu_create_kernel(struct futhark_context *ctx,
                              gpu_kernel *kernel_out,
                              const char *name) {
  if (ctx->debugging) {
    fprintf(ctx->log, "Creating kernel %s.\n", name);
  }

  struct wgpu_kernel_info *kernel_info = wgpu_get_kernel_info(name);
  struct wgpu_kernel *kernel = malloc(sizeof(struct wgpu_kernel));
  kernel->info = kernel_info;

  // If this is a builtin kernel, generate the shader module here
  if (kernel_info->gpu_program[0]) {
    const char* wgsl = strconcat(kernel_info->gpu_program);
    wgpu_module_setup(ctx, wgsl, &kernel->module, name);
    free((void*)wgsl);
  }
  else {
    kernel->module = ctx->module;
  }

  WGPUBufferDescriptor scalars_desc = {
    .label = "kernel scalars",
    .size = kernel_info->scalars_size,
    .usage = WGPUBufferUsage_Uniform | WGPUBufferUsage_CopyDst
  };
  kernel->scalars_buffer = wgpuDeviceCreateBuffer(ctx->device, &scalars_desc);

  // Create bind group layout.
  WGPUBindGroupLayoutEntry *bgl_entries
    = calloc(1 + kernel_info->num_bindings, sizeof(WGPUBindGroupLayoutEntry));

  WGPUBindGroupLayoutEntry *scalar_entry = bgl_entries;
  scalar_entry->binding = kernel_info->scalars_binding;
  scalar_entry->visibility = WGPUShaderStage_Compute;
  WGPUBufferBindingLayout scalar_buffer_layout
    = { .type = WGPUBufferBindingType_Uniform };
  scalar_entry->buffer = scalar_buffer_layout;

  for (int i = 0; i < kernel_info->num_bindings; i++) {
    WGPUBindGroupLayoutEntry *entry = &bgl_entries[1 + i];
    entry->binding = kernel_info->binding_indices[i];
    entry->visibility = WGPUShaderStage_Compute;
    WGPUBufferBindingLayout buffer_layout
      = { .type = WGPUBufferBindingType_Storage };
    entry->buffer = buffer_layout;
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
  // TODO: We should be able to just set all overrides from the context and
  // remove the used_overrides from kernel_info. It only exists because
  // Chrome/Dawn currently complains if we set unused constants, see
  // https://issues.chromium.org/issues/338624452.
  WGPUConstantEntry *const_entries = calloc(kernel_info->num_overrides,
                                            sizeof(WGPUConstantEntry));
  int const_idx = 0;
  for (int i = 0; i < ctx->num_overrides; i++) {
    for (int j = 0; j < kernel_info->num_overrides; j++) {
      if (strcmp(kernel_info->used_overrides[j], ctx->override_names[i]) == 0) {
        WGPUConstantEntry *entry = &const_entries[const_idx];
        entry->key = ctx->override_names[i];
        entry->value = ctx->override_values[i];
        const_idx++;
      }
    }
  }

  kernel->static_pipeline =
    kernel_info->num_dynamic_block_dims == 0
    && kernel_info->num_shared_mem_overrides == 0;
  if (!kernel->static_pipeline) {
    kernel->const_entries = const_entries;
    kernel->const_entries_set = const_idx;
  }
  else {
    // Create pipeline.
    WGPUComputePipelineDescriptor desc = {
      .layout = kernel->pipeline_layout,
      .compute = {
        .module = kernel_info->gpu_program[0] ? kernel->module : ctx->module,
        .entryPoint = kernel_info->name,
        .constantCount = kernel_info->num_overrides,
        .constants = const_entries,
      }
    };

    kernel->pipeline = wgpuDeviceCreateComputePipeline(ctx->device, &desc);

    free(const_entries);
  }

  *kernel_out = kernel;
}

static void gpu_free_kernel(struct futhark_context *ctx,
                            gpu_kernel kernel) {
  (void)ctx;
  wgpuBufferDestroy(kernel->scalars_buffer);
  free(kernel);
}

static int gpu_scalar_to_device(struct futhark_context *ctx,
                                const char *provenance,
                                gpu_mem dst, size_t offset, size_t size,
                                void *src) {
  wgpuQueueWriteBuffer(ctx->queue, dst, offset, src, size);
  return FUTHARK_SUCCESS;
}

static int gpu_scalar_from_device(struct futhark_context *ctx,
                                  const char *provenance,
                                  void *dst,
                                  gpu_mem src, size_t offset, size_t size) {
  if (size > 8) {
    futhark_panic(-1, "gpu_scalar_from_device with size %zu > 8 is not allowed\n",
                  size);
  }

  size_t copy_size = ((size + 4 - 1) / 4) * 4;

  WGPUCommandEncoder encoder = wgpuDeviceCreateCommandEncoder(ctx->device, NULL);
  wgpuCommandEncoderCopyBufferToBuffer(encoder,
    src, offset,
    ctx->scalar_readback_buffer, 0,
    copy_size);

  WGPUCommandBuffer commandBuffer = wgpuCommandEncoderFinish(encoder, NULL);
  wgpuQueueSubmit(ctx->queue, 1, &commandBuffer);

  WGPUBufferMapAsyncStatus status =
    wgpu_map_buffer_sync(ctx->instance, ctx->scalar_readback_buffer,
                         WGPUMapMode_Read, 0, copy_size);
  if (status != WGPUBufferMapAsyncStatus_Success) {
    futhark_panic(-1, "gpu_scalar_from_device: Failed to read scalar from device memory with error %d\n",
                  status);
  }

  const void *mapped = wgpuBufferGetConstMappedRange(ctx->scalar_readback_buffer,
                                                     0, copy_size);
  memcpy(dst, mapped, size);

  wgpuBufferUnmap(ctx->scalar_readback_buffer);
  return FUTHARK_SUCCESS;
}

static int memcpy_host2gpu(struct futhark_context *ctx,
                           const char *provenance,
                           bool sync,
                           gpu_mem dst, int64_t dst_offset,
                           const unsigned char *src, int64_t src_offset,
                           int64_t nbytes) {
  if (nbytes <= 0) { return FUTHARK_SUCCESS; }

  // There is no async copy to device memory at the moment (the spec for
  // `writeBuffer` specifies that a copy of the data is always made and there is
  // no other good option to use here), so we ignore the sync parameter.
  (void)sync;

  // Bound storage buffers and copy operations must have sizes multiple of 4.
  // Note that writing more than `nbytes` is safe because we also pad all
  // buffers when allocating them, but we can't guarantee that the `src` here
  // has enough bytes.
  // If this is a copy somewhere into the middle of `dst`, it is also possible
  // we overwrite some data here, which would be bad.
  int64_t copy_size = ((nbytes + 4 - 1) / 4) * 4;
  if (copy_size > nbytes) {
    // Potential for an issue if we're not at the end of the destination buffer.
    // Find its size to make sure.
    uint64_t dst_size = wgpuBufferGetSize(dst);
    if (dst_offset + copy_size != dst_size) {
      printf("memcpy_host2gpu: Potentially could corrupt data due to padding!\n");
      //futhark_panic(-1, "memcpy_host2gpu: Would corrupt data due to padding!\n");
    }
  }

  const unsigned char *buf;
  int64_t offset;
  if (copy_size > nbytes) {
    buf = malloc(copy_size);
    offset = 0;
    memcpy((unsigned char*)buf, src + src_offset, copy_size);
  }
  else {
    buf = src;
    offset = src_offset;
  }

  wgpuQueueWriteBuffer(ctx->queue, dst, dst_offset, buf + offset, copy_size);

  if (buf != src) {
    free((void*)buf);
  }

  return FUTHARK_SUCCESS;
}

static int memcpy_gpu2host(struct futhark_context *ctx,
                           const char *provenance,
                           bool sync,
                           unsigned char *dst, int64_t dst_offset,
                           gpu_mem src, int64_t src_offset,
                           int64_t nbytes) {
  if (nbytes <= 0) { return FUTHARK_SUCCESS; }

  // Bound storage buffers and copy operations must have sizes multiple of 4.
  // Note that mapping more than `nbytes` is safe because we also pad all
  // buffers when allocating them.
  int64_t buf_size = ((nbytes + 4 - 1) / 4) * 4;

  WGPUBufferDescriptor desc = {
    .label = "tmp_readback",
    .size = buf_size,
    .usage = WGPUBufferUsage_MapRead | WGPUBufferUsage_CopyDst,
  };
  WGPUBuffer readback = wgpuDeviceCreateBuffer(ctx->device, &desc);

  WGPUCommandEncoder encoder = wgpuDeviceCreateCommandEncoder(ctx->device, NULL);
  wgpuCommandEncoderCopyBufferToBuffer(encoder,
    src, src_offset,
    readback, 0,
    buf_size);

  WGPUCommandBuffer commandBuffer = wgpuCommandEncoderFinish(encoder, NULL);
  wgpuQueueSubmit(ctx->queue, 1, &commandBuffer);

  // TODO: Could we do an actual async mapping here if `sync` is false?
  WGPUBufferMapAsyncStatus status =
    wgpu_map_buffer_sync(ctx->instance, readback, WGPUMapMode_Read, 0, buf_size);
  if (status != WGPUBufferMapAsyncStatus_Success) {
    futhark_panic(-1, "memcpy_gpu2host: Failed to copy from device memory with error %d\n",
                  status);
  }

  const void *mapped = wgpuBufferGetConstMappedRange(readback, 0, buf_size);
  memcpy(dst + dst_offset, mapped, nbytes);

  wgpuBufferUnmap(readback);
  wgpuBufferDestroy(readback);
  return FUTHARK_SUCCESS;
}

static int gpu_memcpy(struct futhark_context *ctx,
                      const char *provenance,
                      gpu_mem dst, int64_t dst_offset,
                      gpu_mem src, int64_t src_offset,
                      int64_t nbytes) {
  // Bound storage buffers and copy operations must have sizes multiple of 4.
  // Note that copying more than `nbytes` is memory-safe because we also pad all
  // buffers when allocating them.
  // It could however corrupt data if the copy is in the middle of the buffer,
  // like in host2gpu.
  int64_t copy_size = ((nbytes + 4 - 1) / 4) * 4;
  if (copy_size > nbytes) {
    // Potential for an issue if we're not at the end of the destination buffer.
    // Find its size to make sure.
    uint64_t dst_size = wgpuBufferGetSize(dst);
    if (dst_offset + copy_size != dst_size) {
      printf("gpu_memcpy: Potentially could corrupt data due to padding!\n");
      //futhark_panic(-1, "gpu_memcpy: Would corrupt data due to padding!\n");
    }
  }
  WGPUCommandEncoder encoder = wgpuDeviceCreateCommandEncoder(ctx->device, NULL);

  if (dst == src) {
    printf("gpu_memcpy: Cannot memcpy to/from the same buffer. Copying to temporary buffer first.\n");
    // Allocate temporary buffer.
    gpu_mem tmp;
    gpu_alloc_actual(ctx, copy_size, &tmp);

    // Copy data to temporary buffer and then to the destination.
    wgpuCommandEncoderCopyBufferToBuffer(encoder,
      src, src_offset, tmp, 0, copy_size);
    wgpuCommandEncoderCopyBufferToBuffer(encoder,
      tmp, 0, dst, dst_offset, copy_size);
    WGPUCommandBuffer commandBuffer = wgpuCommandEncoderFinish(encoder, NULL);
    wgpuQueueSubmit(ctx->queue, 1, &commandBuffer);

    // Free the temporary buffer once we have finished copying.
    futhark_context_sync(ctx);
    gpu_free_actual(ctx, tmp);
  }
  else {
    wgpuCommandEncoderCopyBufferToBuffer(encoder,
      src, src_offset, dst, dst_offset, copy_size);
    WGPUCommandBuffer commandBuffer = wgpuCommandEncoderFinish(encoder, NULL);
    wgpuQueueSubmit(ctx->queue, 1, &commandBuffer);
  }

  return FUTHARK_SUCCESS;
}

static int gpu_launch_kernel(struct futhark_context* ctx,
                             gpu_kernel kernel, const char *name,
                             const char *provenance,
                             const int32_t grid[3],
                             const int32_t block[3],
                             unsigned int shared_mem_bytes,
                             int num_args,
                             void* args[num_args],
                             size_t args_sizes[num_args]) {
  struct wgpu_kernel_info *kernel_info = kernel->info;

  if (num_args !=
      kernel_info->num_shared_mem_overrides
      + kernel_info->num_scalars
      + kernel_info->num_bindings
  ) {
    futhark_panic(-1, "Kernel %s called with num_args not maching its info\n",
                  name);
  }

  int shared_mem_start = 0;
  int scalars_start = shared_mem_start + kernel_info->num_shared_mem_overrides;
  int mem_start = scalars_start + kernel_info->num_scalars;

  void *scalars = malloc(kernel_info->scalars_size);
  for (int i = 0; i < kernel_info->num_scalars; i++) {
    memcpy(scalars + kernel_info->scalar_offsets[i],
        args[scalars_start + i], args_sizes[scalars_start + i]);
  }

  WGPUBindGroupEntry *bg_entries = calloc(1 + kernel_info->num_bindings,
                                          sizeof(WGPUBindGroupEntry));
  for (int i = 0; i < kernel_info->num_bindings; i++) {
    WGPUBindGroupEntry *entry = &bg_entries[1 + i];
    entry->binding = kernel_info->binding_indices[i];
    entry->buffer = (gpu_mem) *((gpu_mem *)args[mem_start + i]);
    // In theory setting (offset, size) to (0, 0) should also work and mean
    // 'the entire buffer', but as of writing this, Firefox requires
    // specifying the size.
    entry->offset = 0;
    entry->size = wgpuBufferGetSize(entry->buffer);
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

  WGPUComputePipeline pipeline;
  if (kernel->static_pipeline) { pipeline = kernel->pipeline; }
  else {
    int const_entry_idx = kernel->const_entries_set;
    for (int i = 0; i < kernel_info->num_dynamic_block_dims; i++) {
      WGPUConstantEntry *entry = &kernel->const_entries[const_entry_idx];
      const_entry_idx++;
      entry->key = kernel_info->dynamic_block_dim_names[i];
      entry->value = (double) block[kernel_info->dynamic_block_dim_indices[i]];
    }
    for (int i = 0; i < kernel_info->num_shared_mem_overrides; i++) {
      WGPUConstantEntry *entry = &kernel->const_entries[const_entry_idx];
      const_entry_idx++;
      entry->key = kernel_info->shared_mem_overrides[i];
      entry->value = (double) *((int32_t *) args[shared_mem_start + i]);
    }

    WGPUComputePipelineDescriptor desc = {
      .layout = kernel->pipeline_layout,
      .compute = {
        .module = kernel->module,
        .entryPoint = kernel_info->name,
        .constantCount = kernel_info->num_overrides,
        .constants = kernel->const_entries,
      }
    };
    pipeline = wgpuDeviceCreateComputePipeline(ctx->device, &desc);
  }

  WGPUCommandEncoder encoder = wgpuDeviceCreateCommandEncoder(ctx->device, NULL);

  WGPUComputePassEncoder pass_encoder
    = wgpuCommandEncoderBeginComputePass(encoder, NULL);
  wgpuComputePassEncoderSetPipeline(pass_encoder, pipeline);
  wgpuComputePassEncoderSetBindGroup(pass_encoder, 0, bg, 0, NULL);
  wgpuComputePassEncoderDispatchWorkgroups(pass_encoder,
                                           grid[0], grid[1], grid[2]);
  wgpuComputePassEncoderEnd(pass_encoder);

  WGPUCommandBuffer cmd_buffer = wgpuCommandEncoderFinish(encoder, NULL);
  wgpuQueueSubmit(ctx->queue, 1, &cmd_buffer);

  free(scalars);

  return FUTHARK_SUCCESS;
}

// End of backends/webgpu.h.
