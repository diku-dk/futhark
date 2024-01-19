// Start of backends/cuda.h.

// Forward declarations.
// Invoked by setup_opencl() after the platform and device has been
// found, but before the program is loaded.  Its intended use is to
// tune constants based on the selected platform and device.
static void set_tuning_params(struct futhark_context* ctx);
static char* get_failure_msg(int failure_idx, int64_t args[]);

#define CUDA_SUCCEED_FATAL(x) cuda_api_succeed_fatal(x, #x, __FILE__, __LINE__)
#define CUDA_SUCCEED_NONFATAL(x) cuda_api_succeed_nonfatal(x, #x, __FILE__, __LINE__)
#define NVRTC_SUCCEED_FATAL(x) nvrtc_api_succeed_fatal(x, #x, __FILE__, __LINE__)
#define NVRTC_SUCCEED_NONFATAL(x) nvrtc_api_succeed_nonfatal(x, #x, __FILE__, __LINE__)
// Take care not to override an existing error.
#define CUDA_SUCCEED_OR_RETURN(e) {             \
    char *serror = CUDA_SUCCEED_NONFATAL(e);    \
    if (serror) {                               \
      if (!ctx->error) {                        \
        ctx->error = serror;                    \
        return bad;                             \
      } else {                                  \
        free(serror);                           \
      }                                         \
    }                                           \
  }

// CUDA_SUCCEED_OR_RETURN returns the value of the variable 'bad' in
// scope.  By default, it will be this one.  Create a local variable
// of some other type if needed.  This is a bit of a hack, but it
// saves effort in the code generator.
static const int bad = 1;

static inline void cuda_api_succeed_fatal(CUresult res, const char *call,
                                          const char *file, int line) {
  if (res != CUDA_SUCCESS) {
    const char *err_str;
    cuGetErrorString(res, &err_str);
    if (err_str == NULL) { err_str = "Unknown"; }
    futhark_panic(-1, "%s:%d: CUDA call\n  %s\nfailed with error code %d (%s)\n",
                  file, line, call, res, err_str);
  }
}

static char* cuda_api_succeed_nonfatal(CUresult res, const char *call,
                                       const char *file, int line) {
  if (res != CUDA_SUCCESS) {
    const char *err_str;
    cuGetErrorString(res, &err_str);
    if (err_str == NULL) { err_str = "Unknown"; }
    return msgprintf("%s:%d: CUDA call\n  %s\nfailed with error code %d (%s)\n",
                     file, line, call, res, err_str);
  } else {
    return NULL;
  }
}

static inline void nvrtc_api_succeed_fatal(nvrtcResult res, const char *call,
                                           const char *file, int line) {
  if (res != NVRTC_SUCCESS) {
    const char *err_str = nvrtcGetErrorString(res);
    futhark_panic(-1, "%s:%d: NVRTC call\n  %s\nfailed with error code %d (%s)\n",
                  file, line, call, res, err_str);
  }
}

static char* nvrtc_api_succeed_nonfatal(nvrtcResult res, const char *call,
                                        const char *file, int line) {
  if (res != NVRTC_SUCCESS) {
    const char *err_str = nvrtcGetErrorString(res);
    return msgprintf("%s:%d: NVRTC call\n  %s\nfailed with error code %d (%s)\n",
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
  int num_nvrtc_opts;
  char* *nvrtc_opts;

  char* preferred_device;
  int preferred_device_num;

  char* dump_ptx_to;
  char* load_ptx_from;

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
  cfg->num_nvrtc_opts = 0;
  cfg->nvrtc_opts = (char**) malloc(sizeof(char*));
  cfg->nvrtc_opts[0] = NULL;

  cfg->program = strconcat(gpu_program);

  cfg->preferred_device_num = 0;
  cfg->preferred_device = strdup("");

  cfg->dump_ptx_to = NULL;
  cfg->load_ptx_from = NULL;

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
  for (int i = 0; i < cfg->num_nvrtc_opts; i++) {
    free(cfg->nvrtc_opts[i]);
  }
  free(cfg->nvrtc_opts);
  free(cfg->dump_ptx_to);
  free(cfg->load_ptx_from);
  free(cfg->preferred_device);
  free(cfg->program);
}

void futhark_context_config_add_nvrtc_option(struct futhark_context_config *cfg, const char *opt) {
  cfg->nvrtc_opts[cfg->num_nvrtc_opts] = strdup(opt);
  cfg->num_nvrtc_opts++;
  cfg->nvrtc_opts = (char **) realloc(cfg->nvrtc_opts, (cfg->num_nvrtc_opts + 1) * sizeof(char *));
  cfg->nvrtc_opts[cfg->num_nvrtc_opts] = NULL;
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

void futhark_context_config_dump_ptx_to(struct futhark_context_config *cfg, const char *path) {
  free(cfg->dump_ptx_to);
  cfg->dump_ptx_to = strdup(path);
}

void futhark_context_config_load_ptx_from(struct futhark_context_config *cfg, const char *path) {
  free(cfg->load_ptx_from);
  cfg->load_ptx_from = strdup(path);
}

void futhark_context_config_set_default_thread_block_size(struct futhark_context_config *cfg, int size) {
  cfg->default_block_size = size;
  cfg->default_block_size_changed = 1;
}

void futhark_context_config_set_default_grid_size(struct futhark_context_config *cfg, int num) {
  cfg->default_grid_size = num;
  cfg->default_grid_size_changed = 1;
}

void futhark_context_config_set_default_group_size(struct futhark_context_config *cfg, int size) {
  futhark_context_config_set_default_thread_block_size(cfg, size);
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
  if (strcmp(param_name, "default_thread_block_size") == 0
      || strcmp(param_name, "default_group_size") == 0) {
    cfg->default_block_size = new_value;
    return 0;
  }
  if (strcmp(param_name, "default_num_groups") == 0 ||
      strcmp(param_name, "default_grid_size") == 0) {
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

// A record of something that happened.
struct profiling_record {
  cudaEvent_t *events; // Points to two events.
  const char *name;
};

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

  CUdeviceptr global_failure;
  CUdeviceptr global_failure_args;
  struct tuning_params tuning_params;
  // True if a potentially failing kernel has been enqueued.
  int32_t failure_is_an_option;
  int total_runs;
  long int total_runtime;
  int64_t peak_mem_usage_device;
  int64_t cur_mem_usage_device;

  CUdevice dev;
  CUcontext cu_ctx;
  CUmodule module;
  CUstream stream;

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

#define CU_DEV_ATTR(x) (CU_DEVICE_ATTRIBUTE_##x)
#define device_query(dev,attrib) _device_query(dev, CU_DEV_ATTR(attrib))
static int _device_query(CUdevice dev, CUdevice_attribute attrib) {
  int val;
  CUDA_SUCCEED_FATAL(cuDeviceGetAttribute(&val, attrib, dev));
  return val;
}

#define CU_FUN_ATTR(x) (CU_FUNC_ATTRIBUTE_##x)
#define function_query(fn,attrib) _function_query(dev, CU_FUN_ATTR(attrib))
static int _function_query(CUfunction dev, CUfunction_attribute attrib) {
  int val;
  CUDA_SUCCEED_FATAL(cuFuncGetAttribute(&val, attrib, dev));
  return val;
}

static int cuda_device_setup(struct futhark_context *ctx) {
  struct futhark_context_config *cfg = ctx->cfg;
  char name[256];
  int count, chosen = -1, best_cc = -1;
  int cc_major_best = 0, cc_minor_best = 0;
  int cc_major = 0, cc_minor = 0;
  CUdevice dev;

  CUDA_SUCCEED_FATAL(cuDeviceGetCount(&count));
  if (count == 0) { return 1; }

  int num_device_matches = 0;

  // XXX: Current device selection policy is to choose the device with the
  // highest compute capability (if no preferred device is set).
  // This should maybe be changed, since greater compute capability is not
  // necessarily an indicator of better performance.
  for (int i = 0; i < count; i++) {
    CUDA_SUCCEED_FATAL(cuDeviceGet(&dev, i));

    cc_major = device_query(dev, COMPUTE_CAPABILITY_MAJOR);
    cc_minor = device_query(dev, COMPUTE_CAPABILITY_MINOR);

    CUDA_SUCCEED_FATAL(cuDeviceGetName(name, sizeof(name) - 1, dev));
    name[sizeof(name) - 1] = 0;

    if (cfg->logging) {
      fprintf(ctx->log, "Device #%d: name=\"%s\", compute capability=%d.%d\n",
              i, name, cc_major, cc_minor);
    }

    if (device_query(dev, COMPUTE_MODE) == CU_COMPUTEMODE_PROHIBITED) {
      if (cfg->logging) {
        fprintf(ctx->log, "Device #%d is compute-prohibited, ignoring\n", i);
      }
      continue;
    }

    if (best_cc == -1 || cc_major > cc_major_best ||
        (cc_major == cc_major_best && cc_minor > cc_minor_best)) {
      best_cc = i;
      cc_major_best = cc_major;
      cc_minor_best = cc_minor;
    }

    if (strstr(name, cfg->preferred_device) != NULL &&
        num_device_matches++ == cfg->preferred_device_num) {
      chosen = i;
      break;
    }
  }

  if (chosen == -1) { chosen = best_cc; }
  if (chosen == -1) { return 1; }

  if (cfg->logging) {
    fprintf(ctx->log, "Using device #%d\n", chosen);
  }

  CUDA_SUCCEED_FATAL(cuDeviceGet(&ctx->dev, chosen));
  return 0;
}

static const char *cuda_nvrtc_get_arch(CUdevice dev) {
  static struct {
    int major;
    int minor;
    const char *arch_str;
  } const x[] = {
    { 3, 0, "compute_30" },
    { 3, 2, "compute_32" },
    { 3, 5, "compute_35" },
    { 3, 7, "compute_37" },
    { 5, 0, "compute_50" },
    { 5, 2, "compute_52" },
    { 5, 3, "compute_53" },
    { 6, 0, "compute_60" },
    { 6, 1, "compute_61" },
    { 6, 2, "compute_62" },
    { 7, 0, "compute_70" },
    { 7, 2, "compute_72" },
    { 7, 5, "compute_75" },
    { 8, 0, "compute_80" },
    { 8, 6, "compute_80" },
    { 8, 7, "compute_80" }
  };

  int major = device_query(dev, COMPUTE_CAPABILITY_MAJOR);
  int minor = device_query(dev, COMPUTE_CAPABILITY_MINOR);

  int chosen = -1;
  int num_archs = sizeof(x)/sizeof(x[0]);
  for (int i = 0; i < num_archs; i++) {
    if (x[i].major < major || (x[i].major == major && x[i].minor <= minor)) {
      chosen = i;
    } else {
      break;
    }
  }

  if (chosen == -1) {
    futhark_panic(-1, "Unsupported compute capability %d.%d\n", major, minor);
  }

  if (x[chosen].major != major || x[chosen].minor != minor) {
    fprintf(stderr,
            "Warning: device compute capability is %d.%d, but newest supported by Futhark is %d.%d.\n",
            major, minor, x[chosen].major, x[chosen].minor);
  }

  return x[chosen].arch_str;
}

static void cuda_nvrtc_mk_build_options(struct futhark_context *ctx, const char *extra_opts[],
                                        char*** opts_out, size_t *n_opts) {
  int arch_set = 0, num_extra_opts;
  struct futhark_context_config *cfg = ctx->cfg;

  char** macro_names;
  int64_t* macro_vals;
  int num_macros = gpu_macros(ctx, &macro_names, &macro_vals);

  // nvrtc cannot handle multiple -arch options.  Hence, if one of the
  // extra_opts is -arch, we have to be careful not to do our usual
  // automatic generation.
  for (num_extra_opts = 0; extra_opts[num_extra_opts] != NULL; num_extra_opts++) {
    if (strstr(extra_opts[num_extra_opts], "-arch")
        == extra_opts[num_extra_opts] ||
        strstr(extra_opts[num_extra_opts], "--gpu-architecture")
        == extra_opts[num_extra_opts]) {
      arch_set = 1;
    }
  }

  size_t i = 0, n_opts_alloc = 20 + num_macros + num_extra_opts + cfg->num_tuning_params;
  char **opts = (char**) malloc(n_opts_alloc * sizeof(char *));
  if (!arch_set) {
    opts[i++] = strdup("-arch");
    opts[i++] = strdup(cuda_nvrtc_get_arch(ctx->dev));
  }
  opts[i++] = strdup("-default-device");
  if (cfg->debugging) {
    opts[i++] = strdup("-G");
    opts[i++] = strdup("-lineinfo");
  } else {
    opts[i++] = strdup("--disable-warnings");
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

  // Time for the best lines of the code in the entire compiler.
  if (getenv("CUDA_HOME") != NULL) {
    opts[i++] = msgprintf("-I%s/include", getenv("CUDA_HOME"));
  }
  if (getenv("CUDA_ROOT") != NULL) {
    opts[i++] = msgprintf("-I%s/include", getenv("CUDA_ROOT"));
  }
  if (getenv("CUDA_PATH") != NULL) {
    opts[i++] = msgprintf("-I%s/include", getenv("CUDA_PATH"));
  }
  opts[i++] = msgprintf("-I/usr/local/cuda/include");
  opts[i++] = msgprintf("-I/usr/include");

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

static char* cuda_nvrtc_build(const char *src, const char *opts[], size_t n_opts,
                              char **ptx) {
  nvrtcProgram prog;
  char *problem = NULL;

  problem = NVRTC_SUCCEED_NONFATAL(nvrtcCreateProgram(&prog, src, "futhark-cuda", 0, NULL, NULL));

  if (problem) {
    return problem;
  }

  nvrtcResult res = nvrtcCompileProgram(prog, n_opts, opts);
  if (res != NVRTC_SUCCESS) {
    size_t log_size;
    if (nvrtcGetProgramLogSize(prog, &log_size) == NVRTC_SUCCESS) {
      char *log = (char*) malloc(log_size);
      if (nvrtcGetProgramLog(prog, log) == NVRTC_SUCCESS) {
        problem = msgprintf("NVRTC compilation failed.\n\n%s\n", log);
      } else {
        problem = msgprintf("Could not retrieve compilation log\n");
      }
      free(log);
    }
    return problem;
  }

  size_t ptx_size;
  NVRTC_SUCCEED_FATAL(nvrtcGetPTXSize(prog, &ptx_size));
  *ptx = (char*) malloc(ptx_size);
  NVRTC_SUCCEED_FATAL(nvrtcGetPTX(prog, *ptx));

  NVRTC_SUCCEED_FATAL(nvrtcDestroyProgram(&prog));

  return NULL;
}

static void cuda_load_ptx_from_cache(struct futhark_context_config *cfg,
                                     const char *src,
                                     const char *opts[], size_t n_opts,
                                     struct cache_hash *h, const char *cache_fname,
                                     char **ptx) {
  if (cfg->logging) {
    fprintf(stderr, "Restoring cache from from %s...\n", cache_fname);
  }
  cache_hash_init(h);
  for (size_t i = 0; i < n_opts; i++) {
    cache_hash(h, opts[i], strlen(opts[i]));
  }
  cache_hash(h, src, strlen(src));
  size_t ptxsize;
  errno = 0;
  if (cache_restore(cache_fname, h, (unsigned char**)ptx, &ptxsize) != 0) {
    if (cfg->logging) {
      fprintf(stderr, "Failed to restore cache (errno: %s)\n", strerror(errno));
    }
  }
}

static void cuda_size_setup(struct futhark_context *ctx)
{
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
      (device_query(ctx->dev, MULTIPROCESSOR_COUNT) *
       device_query(ctx->dev, MAX_THREADS_PER_MULTIPROCESSOR))
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

static char* cuda_module_setup(struct futhark_context *ctx,
                               const char *src,
                               const char *extra_opts[],
                               const char* cache_fname) {
  char *ptx = NULL;
  struct futhark_context_config *cfg = ctx->cfg;

  if (cfg->load_ptx_from) {
    ptx = slurp_file(cfg->load_ptx_from, NULL);
  }

  char **opts;
  size_t n_opts;
  cuda_nvrtc_mk_build_options(ctx, extra_opts, &opts, &n_opts);

  if (cfg->logging) {
    fprintf(stderr, "NVRTC compile options:\n");
    for (size_t j = 0; j < n_opts; j++) {
      fprintf(stderr, "\t%s\n", opts[j]);
    }
    fprintf(stderr, "\n");
  }

  struct cache_hash h;
  int loaded_ptx_from_cache = 0;
  if (cache_fname != NULL) {
    cuda_load_ptx_from_cache(cfg, src, (const char**)opts, n_opts, &h, cache_fname, &ptx);

    if (ptx != NULL) {
      if (cfg->logging) {
        fprintf(stderr, "Restored PTX from cache; now loading module...\n");
      }
      if (cuModuleLoadData(&ctx->module, ptx) == CUDA_SUCCESS) {
        if (cfg->logging) {
          fprintf(stderr, "Success!\n");
        }
        loaded_ptx_from_cache = 1;
      } else {
        if (cfg->logging) {
          fprintf(stderr, "Failed!\n");
        }
        free(ptx);
        ptx = NULL;
      }
    }
  }

  if (ptx == NULL) {
    char* problem = cuda_nvrtc_build(src, (const char**)opts, n_opts, &ptx);
    if (problem != NULL) {
      return problem;
    }
  }

  if (cfg->dump_ptx_to != NULL) {
    dump_file(cfg->dump_ptx_to, ptx, strlen(ptx));
  }

  if (!loaded_ptx_from_cache) {
    CUDA_SUCCEED_FATAL(cuModuleLoadData(&ctx->module, ptx));
  }

  if (cache_fname != NULL && !loaded_ptx_from_cache) {
    if (cfg->logging) {
      fprintf(stderr, "Caching PTX in %s...\n", cache_fname);
    }
    errno = 0;
    if (cache_store(cache_fname, &h, (const unsigned char*)ptx, strlen(ptx)) != 0) {
      fprintf(stderr, "Failed to cache PTX: %s\n", strerror(errno));
    }
  }

  for (size_t i = 0; i < n_opts; i++) {
    free((char *)opts[i]);
  }
  free(opts);
  free(ptx);

  return NULL;
}

struct cuda_event {
  cudaEvent_t start;
  cudaEvent_t end;
};

static struct cuda_event* cuda_event_new(struct futhark_context* ctx) {
  if (ctx->profiling && !ctx->profiling_paused) {
    struct cuda_event* e = malloc(sizeof(struct cuda_event));
    cudaEventCreate(&e->start);
    cudaEventCreate(&e->end);
    return e;
  } else {
    return NULL;
  }
}

static int cuda_event_report(struct str_builder* sb, struct cuda_event* e) {
  float ms;
  CUresult err;
  if ((err = cuEventElapsedTime(&ms, e->start, e->end)) != CUDA_SUCCESS) {
    return err;
  }

  // CUDA provides milisecond resolution, but we want microseconds.
  str_builder(sb, ",\"duration\":%f", ms*1000);

  if ((err = cuEventDestroy(e->start)) != CUDA_SUCCESS) {
    return 1;
  }

  if ((err = cuEventDestroy(e->end)) != CUDA_SUCCESS) {
    return 1;
  }

  free(e);

  return 0;
}

int futhark_context_sync(struct futhark_context* ctx) {
  CUDA_SUCCEED_OR_RETURN(cuCtxPushCurrent(ctx->cu_ctx));
  CUDA_SUCCEED_OR_RETURN(cuCtxSynchronize());
  if (ctx->failure_is_an_option) {
    // Check for any delayed error.
    int32_t failure_idx;
    CUDA_SUCCEED_OR_RETURN(
                           cuMemcpyDtoH(&failure_idx,
                                        ctx->global_failure,
                                        sizeof(int32_t)));
    ctx->failure_is_an_option = 0;

    if (failure_idx >= 0) {
      // We have to clear global_failure so that the next entry point
      // is not considered a failure from the start.
      int32_t no_failure = -1;
      CUDA_SUCCEED_OR_RETURN(
                             cuMemcpyHtoD(ctx->global_failure,
                                          &no_failure,
                                          sizeof(int32_t)));

      int64_t args[max_failure_args+1];
      CUDA_SUCCEED_OR_RETURN(
                             cuMemcpyDtoH(&args,
                                          ctx->global_failure_args,
                                          sizeof(args)));

      ctx->error = get_failure_msg(failure_idx, args);

      return FUTHARK_PROGRAM_ERROR;
    }
  }

  CUDA_SUCCEED_OR_RETURN(cuCtxPopCurrent(&ctx->cu_ctx));
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

  CUDA_SUCCEED_FATAL(cuInit(0));
  if (cuda_device_setup(ctx) != 0) {
    futhark_panic(-1, "No suitable CUDA device found.\n");
  }
  CUDA_SUCCEED_FATAL(cuCtxCreate(&ctx->cu_ctx, 0, ctx->dev));

  free_list_init(&ctx->gpu_free_list);

  // MAX_SHARED_MEMORY_PER_BLOCK gives bogus numbers (48KiB); probably
  // for backwards compatibility.  Add _OPTIN and you seem to get the
  // right number.
  ctx->max_shared_memory = device_query(ctx->dev, MAX_SHARED_MEMORY_PER_BLOCK_OPTIN);
#if CUDART_VERSION >= 12000
  ctx->max_shared_memory -= device_query(ctx->dev, RESERVED_SHARED_MEMORY_PER_BLOCK);
#endif
  ctx->max_thread_block_size = device_query(ctx->dev, MAX_THREADS_PER_BLOCK);
  ctx->max_grid_size = device_query(ctx->dev, MAX_GRID_DIM_X);
  ctx->max_tile_size = sqrt(ctx->max_thread_block_size);
  ctx->max_threshold = 0;
  ctx->max_bespoke = 0;
  ctx->max_registers = device_query(ctx->dev, MAX_REGISTERS_PER_BLOCK);
  ctx->max_cache = device_query(ctx->dev, L2_CACHE_SIZE);
  ctx->lockstep_width = device_query(ctx->dev, WARP_SIZE);
  CUDA_SUCCEED_FATAL(cuStreamCreate(&ctx->stream, CU_STREAM_DEFAULT));
  cuda_size_setup(ctx);
  ctx->error = cuda_module_setup(ctx,
                                 ctx->cfg->program,
                                 (const char**)ctx->cfg->nvrtc_opts,
                                 ctx->cfg->cache_fname);

  if (ctx->error != NULL) {
    futhark_panic(1, "During CUDA initialisation:\n%s\n", ctx->error);
  }

  int32_t no_error = -1;
  CUDA_SUCCEED_FATAL(cuMemAlloc(&ctx->global_failure, sizeof(no_error)));
  CUDA_SUCCEED_FATAL(cuMemcpyHtoD(ctx->global_failure, &no_error, sizeof(no_error)));
  // The +1 is to avoid zero-byte allocations.
  CUDA_SUCCEED_FATAL(cuMemAlloc(&ctx->global_failure_args, sizeof(int64_t)*(max_failure_args+1)));

  if ((ctx->kernels = init_builtin_kernels(ctx)) == NULL) {
    return 1;
  }

  return 0;
}

void backend_context_teardown(struct futhark_context* ctx) {
  if (ctx->kernels != NULL) {
    free_builtin_kernels(ctx, ctx->kernels);
    cuMemFree(ctx->global_failure);
    cuMemFree(ctx->global_failure_args);
    CUDA_SUCCEED_FATAL(gpu_free_all(ctx));
    CUDA_SUCCEED_FATAL(cuStreamDestroy(ctx->stream));
    CUDA_SUCCEED_FATAL(cuModuleUnload(ctx->module));
    CUDA_SUCCEED_FATAL(cuCtxDestroy(ctx->cu_ctx));
  }
  free_list_destroy(&ctx->gpu_free_list);
}

// GPU ABSTRACTION LAYER

// Types.

typedef CUfunction gpu_kernel;
typedef CUdeviceptr gpu_mem;

static void gpu_create_kernel(struct futhark_context *ctx,
                              gpu_kernel* kernel,
                              const char* name) {
  if (ctx->debugging) {
    fprintf(ctx->log, "Creating kernel %s.\n", name);
  }
  CUDA_SUCCEED_FATAL(cuModuleGetFunction(kernel, ctx->module, name));
  // Unless the below is set, the kernel is limited to 48KiB of memory.
  CUDA_SUCCEED_FATAL(cuFuncSetAttribute(*kernel,
                                        cudaFuncAttributeMaxDynamicSharedMemorySize,
                                        ctx->max_shared_memory));
}

static void gpu_free_kernel(struct futhark_context *ctx,
                            gpu_kernel kernel) {
  (void)ctx;
  (void)kernel;
}

static int gpu_scalar_to_device(struct futhark_context* ctx,
                                gpu_mem dst, size_t offset, size_t size,
                                void *src) {
  struct cuda_event *event = cuda_event_new(ctx);
  if (event != NULL) {
    add_event(ctx,
              "copy_scalar_to_dev",
              strdup(""),
              event,
              (event_report_fn)cuda_event_report);
    CUDA_SUCCEED_FATAL(cuEventRecord(event->start, ctx->stream));
  }
  CUDA_SUCCEED_OR_RETURN(cuMemcpyHtoD(dst + offset, src, size));
  if (event != NULL) {
    CUDA_SUCCEED_FATAL(cuEventRecord(event->start, ctx->stream));
  }
  return FUTHARK_SUCCESS;
}

static int gpu_scalar_from_device(struct futhark_context* ctx,
                                  void *dst,
                                  gpu_mem src, size_t offset, size_t size) {
  struct cuda_event *event = cuda_event_new(ctx);
  if (event != NULL) {
    add_event(ctx,
              "copy_scalar_from_dev",
              strdup(""),
              event,
              (event_report_fn)cuda_event_report);
    CUDA_SUCCEED_FATAL(cuEventRecord(event->start, ctx->stream));
  }
  CUDA_SUCCEED_OR_RETURN(cuMemcpyDtoH(dst, src + offset, size));
  if (event != NULL) {
    CUDA_SUCCEED_FATAL(cuEventRecord(event->end, ctx->stream));
  }
  return FUTHARK_SUCCESS;
}

static int gpu_memcpy(struct futhark_context* ctx,
                      gpu_mem dst, int64_t dst_offset,
                      gpu_mem src, int64_t src_offset,
                      int64_t nbytes) {
  struct cuda_event *event = cuda_event_new(ctx);
  if (event != NULL) {
    add_event(ctx,
              "copy_dev_to_dev",
              strdup(""),
              event,
              (event_report_fn)cuda_event_report);
    CUDA_SUCCEED_FATAL(cuEventRecord(event->start, ctx->stream));
  }
  CUDA_SUCCEED_OR_RETURN(cuMemcpy(dst+dst_offset, src+src_offset, nbytes));
  if (event != NULL) {
    CUDA_SUCCEED_FATAL(cuEventRecord(event->end, ctx->stream));
  }
  return FUTHARK_SUCCESS;
}

static int memcpy_host2gpu(struct futhark_context* ctx, bool sync,
                           gpu_mem dst, int64_t dst_offset,
                           const unsigned char* src, int64_t src_offset,
                           int64_t nbytes) {
  if (nbytes > 0) {
    struct cuda_event *event = cuda_event_new(ctx);
    if (event != NULL) {
      add_event(ctx,
                "copy_host_to_dev",
                strdup(""),
                event,
                (event_report_fn)cuda_event_report);
      CUDA_SUCCEED_FATAL(cuEventRecord(event->start, ctx->stream));
    }
    if (sync) {
      CUDA_SUCCEED_OR_RETURN
        (cuMemcpyHtoD(dst + dst_offset, src + src_offset, nbytes));
    } else {
      CUDA_SUCCEED_OR_RETURN
        (cuMemcpyHtoDAsync(dst + dst_offset, src + src_offset, nbytes, ctx->stream));
    }
    if (event != NULL) {
      CUDA_SUCCEED_FATAL(cuEventRecord(event->end, ctx->stream));
    }
  }
  return FUTHARK_SUCCESS;
}

static int memcpy_gpu2host(struct futhark_context* ctx, bool sync,
                           unsigned char* dst, int64_t dst_offset,
                           gpu_mem src, int64_t src_offset,
                           int64_t nbytes) {
  if (nbytes > 0) {
    struct cuda_event *event = cuda_event_new(ctx);
    if (event != NULL) {
      add_event(ctx,
                "copy_dev_to_host",
                strdup(""),
                event,
                (event_report_fn)cuda_event_report);
      CUDA_SUCCEED_FATAL(cuEventRecord(event->start, ctx->stream));
    }
    if (sync) {
      CUDA_SUCCEED_OR_RETURN
        (cuMemcpyDtoH(dst + dst_offset, src + src_offset, nbytes));
    } else {
      CUDA_SUCCEED_OR_RETURN
        (cuMemcpyDtoHAsync(dst + dst_offset, src + src_offset, nbytes, ctx->stream));
    }
    if (event != NULL) {
      CUDA_SUCCEED_FATAL(cuEventRecord(event->end, ctx->stream));
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

  struct cuda_event *event = cuda_event_new(ctx);

  if (event != NULL) {
    CUDA_SUCCEED_FATAL(cuEventRecord(event->start, ctx->stream));
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
              (event_report_fn)cuda_event_report);
  }

  CUDA_SUCCEED_OR_RETURN
    (cuLaunchKernel(kernel,
                    grid[0], grid[1], grid[2],
                    block[0], block[1], block[2],
                    shared_mem_bytes, ctx->stream,
                    args, NULL));

  if (event != NULL) {
    CUDA_SUCCEED_FATAL(cuEventRecord(event->end, ctx->stream));
  }

  if (ctx->debugging) {
    CUDA_SUCCEED_FATAL(cuCtxSynchronize());
    time_end = get_wall_time();
    long int time_diff = time_end - time_start;
    fprintf(ctx->log, "  runtime: %ldus\n\n", time_diff);
  }

  return FUTHARK_SUCCESS;
}

static int gpu_alloc_actual(struct futhark_context *ctx, size_t size, gpu_mem *mem_out) {
  CUresult res = cuMemAlloc(mem_out, size);
  if (res == CUDA_ERROR_OUT_OF_MEMORY) {
    return FUTHARK_OUT_OF_MEMORY;
  }
  CUDA_SUCCEED_OR_RETURN(res);
  return FUTHARK_SUCCESS;
}

static int gpu_free_actual(struct futhark_context *ctx, gpu_mem mem) {
  (void)ctx;
  CUDA_SUCCEED_OR_RETURN(cuMemFree(mem));
  return FUTHARK_SUCCESS;
}

// End of backends/cuda.h.
