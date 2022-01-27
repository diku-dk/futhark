// Start of cuda.h.

#define CUDA_SUCCEED_FATAL(x) cuda_api_succeed_fatal(x, #x, __FILE__, __LINE__)
#define CUDA_SUCCEED_NONFATAL(x) cuda_api_succeed_nonfatal(x, #x, __FILE__, __LINE__)
#define NVRTC_SUCCEED_FATAL(x) nvrtc_api_succeed_fatal(x, #x, __FILE__, __LINE__)
#define NVRTC_SUCCEED_NONFATAL(x) nvrtc_api_succeed_nonfatal(x, #x, __FILE__, __LINE__)
// Take care not to override an existing error.
#define CUDA_SUCCEED_OR_RETURN(e) {               \
    char *serror = CUDA_SUCCEED_NONFATAL(e);      \
    if (serror) {                                 \
      if (!ctx->error) {                          \
        ctx->error = serror;                      \
        return bad;                               \
      } else {                                    \
        free(serror);                             \
      }                                           \
    }                                             \
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

struct cuda_config {
  int debugging;
  int logging;
  const char *preferred_device;
  int preferred_device_num;

  const char *dump_program_to;
  const char *load_program_from;

  const char *dump_ptx_to;
  const char *load_ptx_from;

  size_t default_block_size;
  size_t default_grid_size;
  size_t default_tile_size;
  size_t default_reg_tile_size;
  size_t default_threshold;

  int default_block_size_changed;
  int default_grid_size_changed;
  int default_tile_size_changed;

  int num_sizes;
  const char **size_names;
  const char **size_vars;
  int64_t *size_values;
  const char **size_classes;
};

static void cuda_config_init(struct cuda_config *cfg,
                             int num_sizes,
                             const char *size_names[],
                             const char *size_vars[],
                             int64_t *size_values,
                             const char *size_classes[]) {
  cfg->debugging = 0;
  cfg->logging = 0;
  cfg->preferred_device_num = 0;
  cfg->preferred_device = "";
  cfg->dump_program_to = NULL;
  cfg->load_program_from = NULL;

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

  cfg->num_sizes = num_sizes;
  cfg->size_names = size_names;
  cfg->size_vars = size_vars;
  cfg->size_values = size_values;
  cfg->size_classes = size_classes;
}

// A record of something that happened.
struct profiling_record {
  cudaEvent_t *events; // Points to two events.
  int *runs;
  int64_t *runtime;
};

struct cuda_context {
  CUdevice dev;
  CUcontext cu_ctx;
  CUmodule module;

  struct cuda_config cfg;

  struct free_list free_list;

  size_t max_block_size;
  size_t max_grid_size;
  size_t max_tile_size;
  size_t max_threshold;
  size_t max_shared_memory;
  size_t max_bespoke;

  size_t lockstep_width;

  struct profiling_record *profiling_records;
  int profiling_records_capacity;
  int profiling_records_used;
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

static void set_preferred_device(struct cuda_config *cfg, const char *s) {
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
  cfg->preferred_device = s;
  cfg->preferred_device_num = x;
}

static int cuda_device_setup(struct cuda_context *ctx) {
  char name[256];
  int count, chosen = -1, best_cc = -1;
  int cc_major_best, cc_minor_best;
  int cc_major, cc_minor;
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

    if (ctx->cfg.debugging) {
      fprintf(stderr, "Device #%d: name=\"%s\", compute capability=%d.%d\n",
          i, name, cc_major, cc_minor);
    }

    if (device_query(dev, COMPUTE_MODE) == CU_COMPUTEMODE_PROHIBITED) {
      if (ctx->cfg.debugging) {
        fprintf(stderr, "Device #%d is compute-prohibited, ignoring\n", i);
      }
      continue;
    }

    if (best_cc == -1 || cc_major > cc_major_best ||
        (cc_major == cc_major_best && cc_minor > cc_minor_best)) {
      best_cc = i;
      cc_major_best = cc_major;
      cc_minor_best = cc_minor;
    }

    if (strstr(name, ctx->cfg.preferred_device) != NULL &&
        num_device_matches++ == ctx->cfg.preferred_device_num) {
      chosen = i;
      break;
    }
  }

  if (chosen == -1) { chosen = best_cc; }
  if (chosen == -1) { return 1; }

  if (ctx->cfg.debugging) {
    fprintf(stderr, "Using device #%d\n", chosen);
  }

  CUDA_SUCCEED_FATAL(cuDeviceGet(&ctx->dev, chosen));
  return 0;
}

static char *concat_fragments(const char *src_fragments[]) {
  size_t src_len = 0;
  const char **p;

  for (p = src_fragments; *p; p++) {
    src_len += strlen(*p);
  }

  char *src = (char*) malloc(src_len + 1);
  size_t n = 0;
  for (p = src_fragments; *p; p++) {
    strcpy(src + n, *p);
    n += strlen(*p);
  }

  return src;
}

static const char *cuda_nvrtc_get_arch(CUdevice dev) {
  struct {
    int major;
    int minor;
    const char *arch_str;
  } static const x[] = {
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
    { 8, 0, "compute_80" }
  };

  int major = device_query(dev, COMPUTE_CAPABILITY_MAJOR);
  int minor = device_query(dev, COMPUTE_CAPABILITY_MINOR);

  int chosen = -1;
  for (int i = 0; i < sizeof(x)/sizeof(x[0]); i++) {
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

static char* cuda_nvrtc_build(struct cuda_context *ctx, const char *src,
                              const char *extra_opts[], char **ptx) {
  nvrtcProgram prog;
  char *problem = NULL;

  problem = NVRTC_SUCCEED_NONFATAL(nvrtcCreateProgram(&prog, src, "futhark-cuda", 0, NULL, NULL));

  if (problem) {
    return problem;
  }

  int arch_set = 0, num_extra_opts;

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

  size_t n_opts, i = 0, i_dyn, n_opts_alloc = 20 + num_extra_opts + ctx->cfg.num_sizes;
  const char **opts = (const char**) malloc(n_opts_alloc * sizeof(const char *));
  if (!arch_set) {
    opts[i++] = "-arch";
    opts[i++] = cuda_nvrtc_get_arch(ctx->dev);
  }
  opts[i++] = "-default-device";
  if (ctx->cfg.debugging) {
    opts[i++] = "-G";
    opts[i++] = "-lineinfo";
  } else {
    opts[i++] = "--disable-warnings";
  }
  i_dyn = i;
  for (size_t j = 0; j < ctx->cfg.num_sizes; j++) {
    opts[i++] = msgprintf("-D%s=%zu", ctx->cfg.size_vars[j],
        ctx->cfg.size_values[j]);
  }
  opts[i++] = msgprintf("-DLOCKSTEP_WIDTH=%zu", ctx->lockstep_width);
  opts[i++] = msgprintf("-DMAX_THREADS_PER_BLOCK=%zu", ctx->max_block_size);

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

  // It is crucial that the extra_opts are last, so that the free()
  // logic below does not cause problems.
  for (int j = 0; extra_opts[j] != NULL; j++) {
    opts[i++] = extra_opts[j];
  }

  n_opts = i;

  if (ctx->cfg.debugging) {
    fprintf(stderr, "NVRTC compile options:\n");
    for (size_t j = 0; j < n_opts; j++) {
      fprintf(stderr, "\t%s\n", opts[j]);
    }
    fprintf(stderr, "\n");
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

  for (i = i_dyn; i < n_opts-num_extra_opts; i++) { free((char *)opts[i]); }
  free(opts);

  size_t ptx_size;
  NVRTC_SUCCEED_FATAL(nvrtcGetPTXSize(prog, &ptx_size));
  *ptx = (char*) malloc(ptx_size);
  NVRTC_SUCCEED_FATAL(nvrtcGetPTX(prog, *ptx));

  NVRTC_SUCCEED_FATAL(nvrtcDestroyProgram(&prog));

  return NULL;
}

static void cuda_size_setup(struct cuda_context *ctx)
{
  if (ctx->cfg.default_block_size > ctx->max_block_size) {
    if (ctx->cfg.default_block_size_changed) {
      fprintf(stderr,
          "Note: Device limits default block size to %zu (down from %zu).\n",
          ctx->max_block_size, ctx->cfg.default_block_size);
    }
    ctx->cfg.default_block_size = ctx->max_block_size;
  }
  if (ctx->cfg.default_grid_size > ctx->max_grid_size) {
    if (ctx->cfg.default_grid_size_changed) {
      fprintf(stderr,
          "Note: Device limits default grid size to %zu (down from %zu).\n",
          ctx->max_grid_size, ctx->cfg.default_grid_size);
    }
    ctx->cfg.default_grid_size = ctx->max_grid_size;
  }
  if (ctx->cfg.default_tile_size > ctx->max_tile_size) {
    if (ctx->cfg.default_tile_size_changed) {
      fprintf(stderr,
          "Note: Device limits default tile size to %zu (down from %zu).\n",
          ctx->max_tile_size, ctx->cfg.default_tile_size);
    }
    ctx->cfg.default_tile_size = ctx->max_tile_size;
  }

  if (!ctx->cfg.default_grid_size_changed) {
    ctx->cfg.default_grid_size =
      (device_query(ctx->dev, MULTIPROCESSOR_COUNT) *
       device_query(ctx->dev, MAX_THREADS_PER_MULTIPROCESSOR))
      / ctx->cfg.default_block_size;
  }

  for (int i = 0; i < ctx->cfg.num_sizes; i++) {
    const char *size_class = ctx->cfg.size_classes[i];
    int64_t *size_value = &ctx->cfg.size_values[i];
    const char* size_name = ctx->cfg.size_names[i];
    int64_t max_value = 0, default_value = 0;

    if (strstr(size_class, "group_size") == size_class) {
      max_value = ctx->max_block_size;
      default_value = ctx->cfg.default_block_size;
    } else if (strstr(size_class, "num_groups") == size_class) {
      max_value = ctx->max_grid_size;
      default_value = ctx->cfg.default_grid_size;
      // XXX: as a quick and dirty hack, use twice as many threads for
      // histograms by default.  We really should just be smarter
      // about sizes somehow.
      if (strstr(size_name, ".seghist_") != NULL) {
        default_value *= 2;
      }
    } else if (strstr(size_class, "tile_size") == size_class) {
      max_value = ctx->max_tile_size;
      default_value = ctx->cfg.default_tile_size;
    } else if (strstr(size_class, "reg_tile_size") == size_class) {
      max_value = 0; // No limit.
      default_value = ctx->cfg.default_reg_tile_size;
    } else if (strstr(size_class, "threshold") == size_class) {
      // Threshold can be as large as it takes.
      default_value = ctx->cfg.default_threshold;
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

static char* cuda_module_setup(struct cuda_context *ctx,
                               const char *src_fragments[],
                               const char *extra_opts[]) {
  char *ptx = NULL, *src = NULL;

  if (ctx->cfg.load_program_from == NULL) {
    src = concat_fragments(src_fragments);
  } else {
    src = slurp_file(ctx->cfg.load_program_from, NULL);
  }

  if (ctx->cfg.load_ptx_from) {
    if (ctx->cfg.load_program_from != NULL) {
      fprintf(stderr,
              "WARNING: Using PTX from %s instead of C code from %s\n",
              ctx->cfg.load_ptx_from, ctx->cfg.load_program_from);
    }
    ptx = slurp_file(ctx->cfg.load_ptx_from, NULL);
  }

  if (ctx->cfg.dump_program_to != NULL) {
    dump_file(ctx->cfg.dump_program_to, src, strlen(src));
  }

  if (ptx == NULL) {
    char* problem = cuda_nvrtc_build(ctx, src, extra_opts, &ptx);
    if (problem != NULL) {
      free(src);
      return problem;
    }
  }

  if (ctx->cfg.dump_ptx_to != NULL) {
    dump_file(ctx->cfg.dump_ptx_to, ptx, strlen(ptx));
  }

  CUDA_SUCCEED_FATAL(cuModuleLoadData(&ctx->module, ptx));

  free(ptx);
  if (src != NULL) {
    free(src);
  }

  return NULL;
}

static char* cuda_setup(struct cuda_context *ctx, const char *src_fragments[], const char *extra_opts[]) {
  CUDA_SUCCEED_FATAL(cuInit(0));

  if (cuda_device_setup(ctx) != 0) {
    futhark_panic(-1, "No suitable CUDA device found.\n");
  }
  CUDA_SUCCEED_FATAL(cuCtxCreate(&ctx->cu_ctx, 0, ctx->dev));

  free_list_init(&ctx->free_list);

  ctx->max_shared_memory = device_query(ctx->dev, MAX_SHARED_MEMORY_PER_BLOCK);
  ctx->max_block_size = device_query(ctx->dev, MAX_THREADS_PER_BLOCK);
  ctx->max_grid_size = device_query(ctx->dev, MAX_GRID_DIM_X);
  ctx->max_tile_size = sqrt(ctx->max_block_size);
  ctx->max_threshold = 0;
  ctx->max_bespoke = 0;
  ctx->lockstep_width = device_query(ctx->dev, WARP_SIZE);

  cuda_size_setup(ctx);
  return cuda_module_setup(ctx, src_fragments, extra_opts);
}

// Count up the runtime all the profiling_records that occured during execution.
// Also clears the buffer of profiling_records.
static cudaError_t cuda_tally_profiling_records(struct cuda_context *ctx) {
  cudaError_t err;
  for (int i = 0; i < ctx->profiling_records_used; i++) {
    struct profiling_record record = ctx->profiling_records[i];

    float ms;
    if ((err = cudaEventElapsedTime(&ms, record.events[0], record.events[1])) != cudaSuccess) {
      return err;
    }

    // CUDA provides milisecond resolution, but we want microseconds.
    *record.runs += 1;
    *record.runtime += ms*1000;

    if ((err = cudaEventDestroy(record.events[0])) != cudaSuccess) {
      return err;
    }
    if ((err = cudaEventDestroy(record.events[1])) != cudaSuccess) {
      return err;
    }

    free(record.events);
  }

  ctx->profiling_records_used = 0;

  return cudaSuccess;
}

// Returns pointer to two events.
static cudaEvent_t* cuda_get_events(struct cuda_context *ctx, int *runs, int64_t *runtime) {
    if (ctx->profiling_records_used == ctx->profiling_records_capacity) {
      ctx->profiling_records_capacity *= 2;
      ctx->profiling_records =
        realloc(ctx->profiling_records,
                ctx->profiling_records_capacity *
                sizeof(struct profiling_record));
    }
    cudaEvent_t *events = calloc(2, sizeof(cudaEvent_t));
    cudaEventCreate(&events[0]);
    cudaEventCreate(&events[1]);
    ctx->profiling_records[ctx->profiling_records_used].events = events;
    ctx->profiling_records[ctx->profiling_records_used].runs = runs;
    ctx->profiling_records[ctx->profiling_records_used].runtime = runtime;
    ctx->profiling_records_used++;
    return events;
}

static CUresult cuda_free_all(struct cuda_context *ctx);

static void cuda_cleanup(struct cuda_context *ctx) {
  CUDA_SUCCEED_FATAL(cuda_free_all(ctx));
  (void)cuda_tally_profiling_records(ctx);
  free(ctx->profiling_records);
  CUDA_SUCCEED_FATAL(cuModuleUnload(ctx->module));
  CUDA_SUCCEED_FATAL(cuCtxDestroy(ctx->cu_ctx));
}

static CUresult cuda_alloc(struct cuda_context *ctx, size_t min_size,
                           const char *tag, CUdeviceptr *mem_out) {
  if (min_size < sizeof(int)) {
    min_size = sizeof(int);
  }

  size_t size;
  if (free_list_find(&ctx->free_list, min_size, &size, mem_out) == 0) {
    if (size >= min_size) {
      return CUDA_SUCCESS;
    } else {
      CUresult res = cuMemFree(*mem_out);
      if (res != CUDA_SUCCESS) {
        return res;
      }
    }
  }

  CUresult res = cuMemAlloc(mem_out, min_size);
  while (res == CUDA_ERROR_OUT_OF_MEMORY) {
    CUdeviceptr mem;
    if (free_list_first(&ctx->free_list, &mem) == 0) {
      res = cuMemFree(mem);
      if (res != CUDA_SUCCESS) {
        return res;
      }
    } else {
      break;
    }
    res = cuMemAlloc(mem_out, min_size);
  }

  return res;
}

static CUresult cuda_free(struct cuda_context *ctx, CUdeviceptr mem,
                          const char *tag) {
  size_t size;
  CUdeviceptr existing_mem;

  // If there is already a block with this tag, then remove it.
  if (free_list_find(&ctx->free_list, -1, &size, &existing_mem) == 0) {
    CUresult res = cuMemFree(existing_mem);
    if (res != CUDA_SUCCESS) {
      return res;
    }
  }

  CUresult res = cuMemGetAddressRange(NULL, &size, mem);
  if (res == CUDA_SUCCESS) {
    free_list_insert(&ctx->free_list, size, mem, tag);
  }

  return res;
}

static CUresult cuda_free_all(struct cuda_context *ctx) {
  CUdeviceptr mem;
  free_list_pack(&ctx->free_list);
  while (free_list_first(&ctx->free_list, &mem) == 0) {
    CUresult res = cuMemFree(mem);
    if (res != CUDA_SUCCESS) {
      return res;
    }
  }

  return CUDA_SUCCESS;
}

// End of cuda.h.
