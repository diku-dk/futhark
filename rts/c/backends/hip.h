// Start of backends/hip.h.

// Forward declarations.
// Invoked by setup_opencl() after the platform and device has been
// found, but before the program is loaded.  Its intended use is to
// tune constants based on the selected platform and device.
static void set_tuning_params(struct futhark_context* ctx);
static char* get_failure_msg(int failure_idx, int64_t args[]);

#define HIP_SUCCEED_FATAL(x) hip_api_succeed_fatal(x, #x, __FILE__, __LINE__)
#define HIP_SUCCEED_NONFATAL(x) hip_api_succeed_nonfatal(x, #x, __FILE__, __LINE__)
#define HIPRTC_SUCCEED_FATAL(x) hiprtc_api_succeed_fatal(x, #x, __FILE__, __LINE__)
#define HIPRTC_SUCCEED_NONFATAL(x) hiprtc_api_succeed_nonfatal(x, #x, __FILE__, __LINE__)
// Take care not to override an existing error.
#define HIP_SUCCEED_OR_RETURN(e) {             \
    char *serror = HIP_SUCCEED_NONFATAL(e);    \
    if (serror) {                               \
      if (!ctx->error) {                        \
        ctx->error = serror;                    \
        return bad;                             \
      } else {                                  \
        free(serror);                           \
      }                                         \
    }                                           \
  }

// HIP_SUCCEED_OR_RETURN returns the value of the variable 'bad' in
// scope.  By default, it will be this one.  Create a local variable
// of some other type if needed.  This is a bit of a hack, but it
// saves effort in the code generator.
static const int bad = 1;

static inline void hip_api_succeed_fatal(hipError_t res, const char *call,
                                         const char *file, int line) {
  if (res != hipSuccess) {
    const char *err_str = hipGetErrorString(res);
    if (err_str == NULL) { err_str = "Unknown"; }
    futhark_panic(-1, "%s:%d: HIP call\n  %s\nfailed with error code %d (%s)\n",
                  file, line, call, res, err_str);
  }
}

static char* hip_api_succeed_nonfatal(hipError_t res, const char *call,
                                      const char *file, int line) {
  if (res != hipSuccess) {
    const char *err_str = hipGetErrorString(res);
    if (err_str == NULL) { err_str = "Unknown"; }
    return msgprintf("%s:%d: HIP call\n  %s\nfailed with error code %d (%s)\n",
                     file, line, call, res, err_str);
  } else {
    return NULL;
  }
}

static inline void hiprtc_api_succeed_fatal(hiprtcResult res, const char *call,
                                           const char *file, int line) {
  if (res != HIPRTC_SUCCESS) {
    const char *err_str = hiprtcGetErrorString(res);
    futhark_panic(-1, "%s:%d: HIPRTC call\n  %s\nfailed with error code %d (%s)\n",
                  file, line, call, res, err_str);
  }
}

static char* hiprtc_api_succeed_nonfatal(hiprtcResult res, const char *call,
                                        const char *file, int line) {
  if (res != HIPRTC_SUCCESS) {
    const char *err_str = hiprtcGetErrorString(res);
    return msgprintf("%s:%d: HIPRTC call\n  %s\nfailed with error code %d (%s)\n",
                     file, line, call, res, err_str);
  } else {
    return NULL;
  }
}

struct futhark_context_config {
  int in_use;
  int debugging;
  int profiling;
  int logging;
  char* cache_fname;
  int num_tuning_params;
  int64_t *tuning_params;
  const char** tuning_param_names;
  const char** tuning_param_vars;
  const char** tuning_param_classes;
  // Uniform fields above.

  char* program;
  int num_build_opts;
  char* *build_opts;

  char* preferred_device;
  int preferred_device_num;

  size_t default_block_size;
  size_t default_grid_size;
  size_t default_tile_size;
  size_t default_reg_tile_size;
  size_t default_threshold;

  int default_block_size_changed;
  int default_grid_size_changed;
  int default_tile_size_changed;
};

static void backend_context_config_setup(struct futhark_context_config *cfg) {
  cfg->num_build_opts = 0;
  cfg->build_opts = (char**) malloc(sizeof(char*));
  cfg->build_opts[0] = NULL;
  cfg->preferred_device_num = 0;
  cfg->preferred_device = strdup("");
  cfg->program = strconcat(gpu_program);

  cfg->default_block_size = 256;
  cfg->default_grid_size = 0; // Set properly later.
  cfg->default_tile_size = 32;
  cfg->default_reg_tile_size = 2;
  cfg->default_threshold = 32*1024;

  cfg->default_block_size_changed = 0;
  cfg->default_grid_size_changed = 0;
  cfg->default_tile_size_changed = 0;
}

static void backend_context_config_teardown(struct futhark_context_config* cfg) {
  for (int i = 0; i < cfg->num_build_opts; i++) {
    free(cfg->build_opts[i]);
  }

  free(cfg->build_opts);
  free(cfg->preferred_device);
  free(cfg->program);
}

void futhark_context_config_add_build_option(struct futhark_context_config *cfg, const char *opt) {
  cfg->build_opts[cfg->num_build_opts] = strdup(opt);
  cfg->num_build_opts++;
  cfg->build_opts = (char **) realloc(cfg->build_opts, (cfg->num_build_opts + 1) * sizeof(char *));
  cfg->build_opts[cfg->num_build_opts] = NULL;
}

void futhark_context_config_set_device(struct futhark_context_config *cfg, const char *s) {
  int x = 0;
  if (*s == '#') {
    s++;
    while (isdigit(*s)) {
      x = x * 10 + (*s++)-'0';
    }
    // Skip trailing spaces.
    while (isspace(*s)) {
      s++;
    }
  }
  free(cfg->preferred_device);
  cfg->preferred_device = strdup(s);
  cfg->preferred_device_num = x;
}


const char* futhark_context_config_get_program(struct futhark_context_config *cfg) {
  return cfg->program;
}

void futhark_context_config_set_program(struct futhark_context_config *cfg, const char *s) {
  free(cfg->program);
  cfg->program = strdup(s);
}

void futhark_context_config_set_default_thread_block_size(struct futhark_context_config *cfg, int size) {
  cfg->default_block_size = size;
  cfg->default_block_size_changed = 1;
}

void futhark_context_config_set_default_grid_size(struct futhark_context_config *cfg, int num) {
  cfg->default_grid_size = num;
  cfg->default_grid_size_changed = 1;
}

void futhark_context_config_set_default_group_size(struct futhark_context_config *cfg, int num) {
  futhark_context_config_set_default_thread_block_size(cfg, num);
}


void futhark_context_config_set_default_num_groups(struct futhark_context_config *cfg, int num) {
  futhark_context_config_set_default_grid_size(cfg, num);
}

void futhark_context_config_set_default_tile_size(struct futhark_context_config *cfg, int size) {
  cfg->default_tile_size = size;
  cfg->default_tile_size_changed = 1;
}

void futhark_context_config_set_default_reg_tile_size(struct futhark_context_config *cfg, int size) {
  cfg->default_reg_tile_size = size;
}

void futhark_context_config_set_default_threshold(struct futhark_context_config *cfg, int size) {
  cfg->default_threshold = size;
}

int futhark_context_config_set_tuning_param(struct futhark_context_config *cfg,
                                            const char *param_name,
                                            size_t new_value) {
  for (int i = 0; i < cfg->num_tuning_params; i++) {
    if (strcmp(param_name, cfg->tuning_param_names[i]) == 0) {
      cfg->tuning_params[i] = new_value;
      return 0;
    }
  }
  if (strcmp(param_name, "default_thread_block_size") == 0 ||
      strcmp(param_name, "default_group_size") == 0) {
    cfg->default_block_size = new_value;
    return 0;
  }
  if (strcmp(param_name, "default_grid_size") == 0 ||
      strcmp(param_name, "default_num_groups") == 0) {
    cfg->default_grid_size = new_value;
    return 0;
  }
  if (strcmp(param_name, "default_threshold") == 0) {
    cfg->default_threshold = new_value;
    return 0;
  }
  if (strcmp(param_name, "default_tile_size") == 0) {
    cfg->default_tile_size = new_value;
    return 0;
  }
  if (strcmp(param_name, "default_reg_tile_size") == 0) {
    cfg->default_reg_tile_size = new_value;
    return 0;
  }
  return 1;
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
  bool program_initialised;
  // Uniform fields above.

  void* global_failure;
  void* global_failure_args;
  struct tuning_params tuning_params;
  // True if a potentially failing kernel has been enqueued.
  int32_t failure_is_an_option;
  int total_runs;
  long int total_runtime;
  int64_t peak_mem_usage_device;
  int64_t cur_mem_usage_device;
  struct program* program;

  hipDevice_t dev;
  int dev_id;
  hipModule_t module;
  hipStream_t stream;

  struct free_list gpu_free_list;

  size_t max_thread_block_size;
  size_t max_grid_size;
  size_t max_tile_size;
  size_t max_threshold;
  size_t max_shared_memory;
  size_t max_bespoke;
  size_t max_registers;
  size_t max_cache;

  size_t lockstep_width;

  struct builtin_kernels* kernels;
};

static int device_query(int dev_id, hipDeviceAttribute_t attr) {
  int val;
  HIP_SUCCEED_FATAL(hipDeviceGetAttribute(&val, attr, dev_id));
  return val;
}

static int function_query(hipFunction_t f, hipFunction_attribute attr) {
  int val;
  HIP_SUCCEED_FATAL(hipFuncGetAttribute(&val, attr, f));
  return val;
}

static int hip_device_setup(struct futhark_context *ctx) {
  struct futhark_context_config *cfg = ctx->cfg;
  int count, chosen = -1;
  hipDevice_t dev;

  HIP_SUCCEED_FATAL(hipGetDeviceCount(&count));
  if (count == 0) { return 1; }

  int num_device_matches = 0;

  for (int i = 0; i < count; i++) {
    hipDeviceProp_t prop;
    hipGetDeviceProperties(&prop, i);

    if (cfg->logging) {
      fprintf(ctx->log, "Device #%d: name=\"%s\"\n", i, prop.name);
    }

    if (strstr(prop.name, cfg->preferred_device) != NULL &&
        num_device_matches++ == cfg->preferred_device_num) {
      chosen = i;
      break;
    }
  }

  if (chosen == -1) { return 1; }

  if (cfg->logging) {
    fprintf(ctx->log, "Using device #%d\n", chosen);
  }

  ctx->dev_id = chosen;
  HIP_SUCCEED_FATAL(hipDeviceGet(&ctx->dev, ctx->dev_id));
  return 0;
}

static void hip_load_code_from_cache(struct futhark_context_config *cfg,
                                     const char *src,
                                     const char *opts[], size_t n_opts,
                                     struct cache_hash *h, const char *cache_fname,
                                     char **code, size_t *code_size) {
  if (cfg->logging) {
    fprintf(stderr, "Restoring cache from from %s...\n", cache_fname);
  }
  cache_hash_init(h);
  for (size_t i = 0; i < n_opts; i++) {
    cache_hash(h, opts[i], strlen(opts[i]));
  }
  cache_hash(h, src, strlen(src));
  errno = 0;
  if (cache_restore(cache_fname, h, (unsigned char**)code, code_size) != 0) {
    if (cfg->logging) {
      fprintf(stderr, "Failed to restore cache (errno: %s)\n", strerror(errno));
    }
  }
}

static void hip_size_setup(struct futhark_context *ctx) {
  struct futhark_context_config *cfg = ctx->cfg;
  if (cfg->default_block_size > ctx->max_thread_block_size) {
    if (cfg->default_block_size_changed) {
      fprintf(stderr,
              "Note: Device limits default block size to %zu (down from %zu).\n",
              ctx->max_thread_block_size, cfg->default_block_size);
    }
    cfg->default_block_size = ctx->max_thread_block_size;
  }
  if (cfg->default_grid_size > ctx->max_grid_size) {
    if (cfg->default_grid_size_changed) {
      fprintf(stderr,
              "Note: Device limits default grid size to %zu (down from %zu).\n",
              ctx->max_grid_size, cfg->default_grid_size);
    }
    cfg->default_grid_size = ctx->max_grid_size;
  }
  if (cfg->default_tile_size > ctx->max_tile_size) {
    if (cfg->default_tile_size_changed) {
      fprintf(stderr,
              "Note: Device limits default tile size to %zu (down from %zu).\n",
              ctx->max_tile_size, cfg->default_tile_size);
    }
    cfg->default_tile_size = ctx->max_tile_size;
  }

  if (!cfg->default_grid_size_changed) {
    cfg->default_grid_size =
      (device_query(ctx->dev, hipDeviceAttributePhysicalMultiProcessorCount) *
       device_query(ctx->dev, hipDeviceAttributeMaxThreadsPerMultiProcessor))
      / cfg->default_block_size;
  }

  for (int i = 0; i < cfg->num_tuning_params; i++) {
    const char *size_class = cfg->tuning_param_classes[i];
    int64_t *size_value = &cfg->tuning_params[i];
    const char* size_name = cfg->tuning_param_names[i];
    int64_t max_value = 0, default_value = 0;

    if (strstr(size_class, "thread_block_size") == size_class) {
      max_value = ctx->max_thread_block_size;
      default_value = cfg->default_block_size;
    } else if (strstr(size_class, "grid_size") == size_class) {
      max_value = ctx->max_grid_size;
      default_value = cfg->default_grid_size;
      // XXX: as a quick and dirty hack, use twice as many threads for
      // histograms by default.  We really should just be smarter
      // about sizes somehow.
      if (strstr(size_name, ".seghist_") != NULL) {
        default_value *= 2;
      }
    } else if (strstr(size_class, "tile_size") == size_class) {
      max_value = ctx->max_tile_size;
      default_value = cfg->default_tile_size;
    } else if (strstr(size_class, "reg_tile_size") == size_class) {
      max_value = 0; // No limit.
      default_value = cfg->default_reg_tile_size;
    } else if (strstr(size_class, "threshold") == size_class) {
      // Threshold can be as large as it takes.
      default_value = cfg->default_threshold;
    } else {
      // Bespoke sizes have no limit or default.
    }

    if (*size_value == 0) {
      *size_value = default_value;
    } else if (max_value > 0 && *size_value > max_value) {
      fprintf(stderr, "Note: Device limits %s to %zu (down from %zu)\n",
              size_name, max_value, *size_value);
      *size_value = max_value;
    }
  }
}

static char* hiprtc_build(const char *src, const char *opts[], size_t n_opts,
                          char **code, size_t *code_size) {
  hiprtcProgram prog;
  char *problem = NULL;

  problem = HIPRTC_SUCCEED_NONFATAL(hiprtcCreateProgram(&prog, src, "futhark-hip", 0, NULL, NULL));

  if (problem) {
    return problem;
  }

  hiprtcResult res = hiprtcCompileProgram(prog, n_opts, opts);
  if (res != HIPRTC_SUCCESS) {
    size_t log_size;
    if (hiprtcGetProgramLogSize(prog, &log_size) == HIPRTC_SUCCESS) {
      char *log = (char*) malloc(log_size+1);
      log[log_size] = 0; // HIPRTC does not zero-terminate.
      if (hiprtcGetProgramLog(prog, log) == HIPRTC_SUCCESS) {
        problem = msgprintf("HIPRTC compilation failed.\n\n%s\n", log);
      } else {
        problem = msgprintf("Could not retrieve compilation log\n");
      }
      free(log);
    }
    return problem;
  }

  HIPRTC_SUCCEED_FATAL(hiprtcGetCodeSize(prog, code_size));
  *code = (char*) malloc(*code_size);
  HIPRTC_SUCCEED_FATAL(hiprtcGetCode(prog, *code));
  HIPRTC_SUCCEED_FATAL(hiprtcDestroyProgram(&prog));
  return NULL;
}

static void hiprtc_mk_build_options(struct futhark_context *ctx, const char *extra_opts[],
                                    char*** opts_out, size_t *n_opts) {
  int arch_set = 0, num_extra_opts;
  struct futhark_context_config *cfg = ctx->cfg;

  char** macro_names;
  int64_t* macro_vals;
  int num_macros = gpu_macros(ctx, &macro_names, &macro_vals);

  for (num_extra_opts = 0; extra_opts[num_extra_opts] != NULL; num_extra_opts++) {
    if (strstr(extra_opts[num_extra_opts], "--gpu-architecture")
        == extra_opts[num_extra_opts]) {
      arch_set = 1;
    }
  }

  size_t i = 0, n_opts_alloc = 20 + num_macros + num_extra_opts + cfg->num_tuning_params;
  char **opts = (char**) malloc(n_opts_alloc * sizeof(char *));
  if (!arch_set) {
    hipDeviceProp_t props;
    HIP_SUCCEED_FATAL(hipGetDeviceProperties(&props, ctx->dev_id));
    opts[i++] = msgprintf("--gpu-architecture=%s", props.gcnArchName);
  }
  if (cfg->debugging) {
    opts[i++] = strdup("-G");
    opts[i++] = strdup("-lineinfo");
  }
  opts[i++] = msgprintf("-D%s=%d",
                        "max_thread_block_size",
                        (int)ctx->max_thread_block_size);
  opts[i++] = msgprintf("-D%s=%d",
                        "max_shared_memory",
                        (int)ctx->max_shared_memory);
  opts[i++] = msgprintf("-D%s=%d",
                        "max_registers",
                        (int)ctx->max_registers);

  for (int j = 0; j < num_macros; j++) {
    opts[i++] = msgprintf("-D%s=%zu", macro_names[j], macro_vals[j]);
  }

  for (int j = 0; j < cfg->num_tuning_params; j++) {
    opts[i++] = msgprintf("-D%s=%zu", cfg->tuning_param_vars[j],
                          cfg->tuning_params[j]);
  }
  opts[i++] = msgprintf("-DLOCKSTEP_WIDTH=%zu", ctx->lockstep_width);
  opts[i++] = msgprintf("-DMAX_THREADS_PER_BLOCK=%zu", ctx->max_thread_block_size);

  for (int j = 0; extra_opts[j] != NULL; j++) {
    opts[i++] = strdup(extra_opts[j]);
  }

  opts[i++] = msgprintf("-DTR_BLOCK_DIM=%d", TR_BLOCK_DIM);
  opts[i++] = msgprintf("-DTR_TILE_DIM=%d", TR_TILE_DIM);
  opts[i++] = msgprintf("-DTR_ELEMS_PER_THREAD=%d", TR_ELEMS_PER_THREAD);

  free(macro_names);
  free(macro_vals);

  *n_opts = i;
  *opts_out = opts;
}

static char* hip_module_setup(struct futhark_context *ctx,
                              const char *src,
                              const char *extra_opts[],
                              const char* cache_fname) {
  char *code = NULL;
  size_t code_size = 0;
  struct futhark_context_config *cfg = ctx->cfg;

  char **opts;
  size_t n_opts;
  hiprtc_mk_build_options(ctx, extra_opts, &opts, &n_opts);

  if (cfg->logging) {
    fprintf(stderr, "HIPRTC build options:\n");
    for (size_t j = 0; j < n_opts; j++) {
      fprintf(stderr, "\t%s\n", opts[j]);
    }
    fprintf(stderr, "\n");
  }

  struct cache_hash h;
  int loaded_code_from_cache = 0;
  if (cache_fname != NULL) {
    hip_load_code_from_cache(cfg, src, (const char**)opts, n_opts, &h, cache_fname, &code, &code_size);

    if (code != NULL) {
      if (cfg->logging) {
        fprintf(stderr, "Restored compiled code from cache; now loading module...\n");
      }
      if (hipModuleLoadData(&ctx->module, code) == hipSuccess) {
        if (cfg->logging) {
          fprintf(stderr, "Success!\n");
        }
        loaded_code_from_cache = 1;
      } else {
        if (cfg->logging) {
          fprintf(stderr, "Failed!\n");
        }
        free(code);
        code = NULL;
      }
    }
  }

  if (code == NULL) {
    char* problem = hiprtc_build(src, (const char**)opts, n_opts, &code, &code_size);
    if (problem != NULL) {
      return problem;
    }
  }

  if (!loaded_code_from_cache) {
    HIP_SUCCEED_FATAL(hipModuleLoadData(&ctx->module, code));
  }

  if (cache_fname != NULL && !loaded_code_from_cache) {
    if (cfg->logging) {
      fprintf(stderr, "Caching compiled code in %s...\n", cache_fname);
    }
    errno = 0;
    if (cache_store(cache_fname, &h, (const unsigned char*)code, code_size) != 0) {
      fprintf(stderr, "Failed to cache compiled code: %s\n", strerror(errno));
    }
  }

  for (size_t i = 0; i < n_opts; i++) {
    free((char *)opts[i]);
  }
  free(opts);
  free(code);

  return NULL;
}

struct hip_event {
  hipEvent_t start;
  hipEvent_t end;
};

static struct hip_event* hip_event_new(struct futhark_context* ctx) {
  if (ctx->profiling && !ctx->profiling_paused) {
    struct hip_event* e = malloc(sizeof(struct hip_event));
    hipEventCreate(&e->start);
    hipEventCreate(&e->end);
    return e;
  } else {
    return NULL;
  }
}

static int hip_event_report(struct str_builder* sb, struct hip_event* e) {
  float ms;
  hipError_t err;
  if ((err = hipEventElapsedTime(&ms, e->start, e->end)) != hipSuccess) {
    return err;
  }

  // HIP provides milisecond resolution, but we want microseconds.
  str_builder(sb, ",\"duration\":%f", ms*1000);

  if ((err = hipEventDestroy(e->start)) != hipSuccess) {
    return 1;
  }

  if ((err = hipEventDestroy(e->end)) != hipSuccess) {
    return 1;
  }

  free(e);

  return 0;
}

int futhark_context_sync(struct futhark_context* ctx) {
  HIP_SUCCEED_OR_RETURN(hipStreamSynchronize(ctx->stream));
  if (ctx->failure_is_an_option) {
    // Check for any delayed error.
    int32_t failure_idx;
    HIP_SUCCEED_OR_RETURN(hipMemcpyDtoH(&failure_idx,
                                        ctx->global_failure,
                                        sizeof(int32_t)));
    ctx->failure_is_an_option = 0;

    if (failure_idx >= 0) {
      // We have to clear global_failure so that the next entry point
      // is not considered a failure from the start.
      int32_t no_failure = -1;
      HIP_SUCCEED_OR_RETURN(hipMemcpyHtoD(ctx->global_failure,
                                          &no_failure,
                                          sizeof(int32_t)));

      int64_t args[max_failure_args+1];
      HIP_SUCCEED_OR_RETURN(hipMemcpyDtoH(&args,
                                          ctx->global_failure_args,
                                          sizeof(args)));

      ctx->error = get_failure_msg(failure_idx, args);

      return FUTHARK_PROGRAM_ERROR;
    }
  }
  return 0;
}

struct builtin_kernels* init_builtin_kernels(struct futhark_context* ctx);
void free_builtin_kernels(struct futhark_context* ctx, struct builtin_kernels* kernels);

int backend_context_setup(struct futhark_context* ctx) {
  ctx->failure_is_an_option = 0;
  ctx->total_runs = 0;
  ctx->total_runtime = 0;
  ctx->peak_mem_usage_device = 0;
  ctx->cur_mem_usage_device = 0;
  ctx->kernels = NULL;

  HIP_SUCCEED_FATAL(hipInit(0));
  if (hip_device_setup(ctx) != 0) {
    futhark_panic(-1, "No suitable HIP device found.\n");
  }

  free_list_init(&ctx->gpu_free_list);

  ctx->max_shared_memory = device_query(ctx->dev, hipDeviceAttributeMaxSharedMemoryPerBlock);
  ctx->max_thread_block_size = device_query(ctx->dev, hipDeviceAttributeMaxThreadsPerBlock);
  ctx->max_grid_size = device_query(ctx->dev, hipDeviceAttributeMaxGridDimX);
  ctx->max_tile_size = sqrt(ctx->max_thread_block_size);
  ctx->max_threshold = 0;
  ctx->max_bespoke = 0;
  ctx->max_registers = device_query(ctx->dev, hipDeviceAttributeMaxRegistersPerBlock);
  ctx->max_cache = device_query(ctx->dev, hipDeviceAttributeL2CacheSize);
  // FIXME: in principle we should query hipDeviceAttributeWarpSize
  // from the device, which will provide 64 on AMD GPUs.
  // Unfortunately, we currently do nasty implicit intra-warp
  // synchronisation in codegen, which does not work when this is 64.
  // Once our codegen properly synchronises intra-warp operations, we
  // can use the actual hardware lockstep width instead.
  ctx->lockstep_width = 32;
  HIP_SUCCEED_FATAL(hipStreamCreate(&ctx->stream));
  hip_size_setup(ctx);
  ctx->error = hip_module_setup(ctx,
                                ctx->cfg->program,
                                (const char**)ctx->cfg->build_opts,
                                ctx->cfg->cache_fname);

  if (ctx->error != NULL) {
    futhark_panic(1, "During HIP initialisation:\n%s\n", ctx->error);
  }

  int32_t no_error = -1;
  HIP_SUCCEED_FATAL(hipMalloc(&ctx->global_failure, sizeof(no_error)));
  HIP_SUCCEED_FATAL(hipMemcpyHtoD(ctx->global_failure, &no_error, sizeof(no_error)));
  // The +1 is to avoid zero-byte allocations.
  HIP_SUCCEED_FATAL(hipMalloc(&ctx->global_failure_args, sizeof(int64_t)*(max_failure_args+1)));

  if ((ctx->kernels = init_builtin_kernels(ctx)) == NULL) {
    return 1;
  }

  return 0;
}

void backend_context_teardown(struct futhark_context* ctx) {
  if (ctx->kernels != NULL) {
    free_builtin_kernels(ctx, ctx->kernels);
    hipFree(ctx->global_failure);
    hipFree(ctx->global_failure_args);
    HIP_SUCCEED_FATAL(gpu_free_all(ctx));
    HIP_SUCCEED_FATAL(hipStreamDestroy(ctx->stream));
    HIP_SUCCEED_FATAL(hipModuleUnload(ctx->module));
  }
  free_list_destroy(&ctx->gpu_free_list);
}

// GPU ABSTRACTION LAYER

typedef hipFunction_t gpu_kernel;
typedef hipDeviceptr_t gpu_mem;

static void gpu_create_kernel(struct futhark_context *ctx,
                              gpu_kernel* kernel,
                              const char* name) {
  if (ctx->debugging) {
    fprintf(ctx->log, "Creating kernel %s.\n", name);
  }
  HIP_SUCCEED_FATAL(hipModuleGetFunction(kernel, ctx->module, name));
}

static void gpu_free_kernel(struct futhark_context *ctx,
                            gpu_kernel kernel) {
  (void)ctx;
  (void)kernel;
}

static int gpu_scalar_to_device(struct futhark_context* ctx,
                                gpu_mem dst, size_t offset, size_t size,
                                void *src) {
  struct hip_event *event = hip_event_new(ctx);
  if (event != NULL) {
    add_event(ctx,
              "copy_scalar_to_dev",
              strdup(""),
              event,
              (event_report_fn)hip_event_report);
    HIP_SUCCEED_FATAL(hipEventRecord(event->start, ctx->stream));
  }
  HIP_SUCCEED_OR_RETURN(hipMemcpyHtoD((unsigned char*)dst + offset, src, size));
  if (event != NULL) {
    HIP_SUCCEED_FATAL(hipEventRecord(event->end, ctx->stream));
  }
  return FUTHARK_SUCCESS;
}

static int gpu_scalar_from_device(struct futhark_context* ctx,
                                  void *dst,
                                  gpu_mem src, size_t offset, size_t size) {
  struct hip_event *event = hip_event_new(ctx);
  if (event != NULL) {
    add_event(ctx,
              "copy_scalar_from_dev",
              strdup(""),
              event,
              (event_report_fn)hip_event_report);
    HIP_SUCCEED_FATAL(hipEventRecord(event->start, ctx->stream));
  }
  HIP_SUCCEED_OR_RETURN(hipMemcpyDtoH(dst, (unsigned char*)src + offset, size));
  if (event != NULL) {
    HIP_SUCCEED_FATAL(hipEventRecord(event->end, ctx->stream));
  }
  return FUTHARK_SUCCESS;
}

static int gpu_memcpy(struct futhark_context* ctx,
                      gpu_mem dst, int64_t dst_offset,
                      gpu_mem src, int64_t src_offset,
                      int64_t nbytes) {
  struct hip_event *event = hip_event_new(ctx);
  if (event != NULL) {
    add_event(ctx,
              "copy_dev_to_dev",
              strdup(""),
              event,
              (event_report_fn)hip_event_report);
    HIP_SUCCEED_FATAL(hipEventRecord(event->start, ctx->stream));
  }
  HIP_SUCCEED_OR_RETURN(hipMemcpyWithStream((unsigned char*)dst+dst_offset, (unsigned char*)src+src_offset,
                                            nbytes, hipMemcpyDeviceToDevice ,ctx->stream));
  if (event != NULL) {
    HIP_SUCCEED_FATAL(hipEventRecord(event->end, ctx->stream));
  }
  return FUTHARK_SUCCESS;
}

static int memcpy_host2gpu(struct futhark_context* ctx, bool sync,
                           gpu_mem dst, int64_t dst_offset,
                           const unsigned char* src, int64_t src_offset,
                           int64_t nbytes) {
  if (nbytes > 0) {
    struct hip_event *event = hip_event_new(ctx);
    if (event != NULL) {
      add_event(ctx,
                "copy_host_to_dev",
                strdup(""),
                event,
                (event_report_fn)hip_event_report);
      HIP_SUCCEED_FATAL(hipEventRecord(event->start, ctx->stream));
    }
    if (sync) {
      HIP_SUCCEED_OR_RETURN
        (hipMemcpyHtoD((unsigned char*)dst + dst_offset,
                       (unsigned char*)src + src_offset, nbytes));
    } else {
      HIP_SUCCEED_OR_RETURN
        (hipMemcpyHtoDAsync((unsigned char*)dst + dst_offset,
                            (unsigned char*)src + src_offset,
                            nbytes, ctx->stream));
    }
    if (event != NULL) {
      HIP_SUCCEED_FATAL(hipEventRecord(event->end, ctx->stream));
    }
  }
  return FUTHARK_SUCCESS;
}

static int memcpy_gpu2host(struct futhark_context* ctx, bool sync,
                           unsigned char* dst, int64_t dst_offset,
                           gpu_mem src, int64_t src_offset,
                           int64_t nbytes) {
  if (nbytes > 0) {
    struct hip_event *event = hip_event_new(ctx);
    if (event != NULL) {
      add_event(ctx,
                "copy_dev_to_host",
                strdup(""),
                event,
                (event_report_fn)hip_event_report);
      HIP_SUCCEED_FATAL(hipEventRecord(event->start, ctx->stream));
    }
    if (sync) {
      HIP_SUCCEED_OR_RETURN
        (hipMemcpyDtoH(dst + dst_offset,
                       (unsigned char*)src + src_offset,
                       nbytes));
    } else {
      HIP_SUCCEED_OR_RETURN
        (hipMemcpyDtoHAsync(dst + dst_offset,
                            (unsigned char*)src + src_offset,
                            nbytes, ctx->stream));
    }
    if (event != NULL) {
      HIP_SUCCEED_FATAL(hipEventRecord(event->end, ctx->stream));
    }
    if (sync &&
        ctx->failure_is_an_option &&
        futhark_context_sync(ctx) != 0) {
      return 1;
    }
  }
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
  (void) args_sizes;
  int64_t time_start = 0, time_end = 0;
  if (ctx->debugging) {
    time_start = get_wall_time();
  }

  struct hip_event *event = hip_event_new(ctx);

  if (event != NULL) {
    HIP_SUCCEED_FATAL(hipEventRecord(event->start, ctx->stream));
    add_event(ctx,
              name,
              msgprintf("Kernel %s with\n"
                        "  grid=(%d,%d,%d)\n"
                        "  block=(%d,%d,%d)\n"
                        "  shared memory=%d",
                        name,
                        grid[0], grid[1], grid[2],
                        block[0], block[1], block[2],
                        shared_mem_bytes),
              event,
              (event_report_fn)hip_event_report);
  }

  HIP_SUCCEED_OR_RETURN
    (hipModuleLaunchKernel(kernel,
                           grid[0], grid[1], grid[2],
                           block[0], block[1], block[2],
                           shared_mem_bytes, ctx->stream,
                           args, NULL));

  if (event != NULL) {
    HIP_SUCCEED_FATAL(hipEventRecord(event->end, ctx->stream));
  }

  if (ctx->debugging) {
    HIP_SUCCEED_FATAL(hipStreamSynchronize(ctx->stream));
    time_end = get_wall_time();
    long int time_diff = time_end - time_start;
    fprintf(ctx->log, "  runtime: %ldus\n\n", time_diff);
  }

  return FUTHARK_SUCCESS;
}

static int gpu_alloc_actual(struct futhark_context *ctx, size_t size, gpu_mem *mem_out) {
  hipError_t res = hipMalloc(mem_out, size);
  if (res == hipErrorOutOfMemory) {
    return FUTHARK_OUT_OF_MEMORY;
  }
  HIP_SUCCEED_OR_RETURN(res);
  return FUTHARK_SUCCESS;
}

static int gpu_free_actual(struct futhark_context *ctx, gpu_mem mem) {
  (void)ctx;
  HIP_SUCCEED_OR_RETURN(hipFree(mem));
  return FUTHARK_SUCCESS;
}

// End of backends/hip.h.
