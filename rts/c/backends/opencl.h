// Start of backends/opencl.h

// Note [32-bit transpositions]
//
// Transposition kernels are much slower when they have to use 64-bit
// arithmetic.  I observed about 0.67x slowdown on an A100 GPU when
// transposing four-byte elements (much less when transposing 8-byte
// elements).  Unfortunately, 64-bit arithmetic is a requirement for
// large arrays (see #1953 for what happens otherwise).  We generate
// both 32- and 64-bit index arithmetic versions of transpositions,
// and dynamically pick between them at runtime.  This is an
// unfortunate code bloat, and it would be preferable if we could
// simply optimise the 64-bit version to make this distinction
// unnecessary.  Fortunately these kernels are quite small.

// Forward declarations.
struct opencl_device_option;
// Invoked by setup_opencl() after the platform and device has been
// found, but before the program is loaded.  Its intended use is to
// tune constants based on the selected platform and device.
static void post_opencl_setup(struct futhark_context*, struct opencl_device_option*);
static void set_tuning_params(struct futhark_context* ctx);
static char* get_failure_msg(int failure_idx, int64_t args[]);

#define OPENCL_SUCCEED_FATAL(e) opencl_succeed_fatal(e, #e, __FILE__, __LINE__)
#define OPENCL_SUCCEED_NONFATAL(e) opencl_succeed_nonfatal(e, #e, __FILE__, __LINE__)
// Take care not to override an existing error.
#define OPENCL_SUCCEED_OR_RETURN(e) {           \
    char *serror = OPENCL_SUCCEED_NONFATAL(e);  \
    if (serror) {                               \
      if (!ctx->error) {                        \
        ctx->error = serror;                    \
        return bad;                             \
      } else {                                  \
        free(serror);                           \
      }                                         \
    }                                           \
  }

// OPENCL_SUCCEED_OR_RETURN returns the value of the variable 'bad' in
// scope.  By default, it will be this one.  Create a local variable
// of some other type if needed.  This is a bit of a hack, but it
// saves effort in the code generator.
static const int bad = 1;

static const char* opencl_error_string(cl_int err) {
  switch (err) {
  case CL_SUCCESS:                            return "Success!";
  case CL_DEVICE_NOT_FOUND:                   return "Device not found.";
  case CL_DEVICE_NOT_AVAILABLE:               return "Device not available";
  case CL_COMPILER_NOT_AVAILABLE:             return "Compiler not available";
  case CL_MEM_OBJECT_ALLOCATION_FAILURE:      return "Memory object allocation failure";
  case CL_OUT_OF_RESOURCES:                   return "Out of resources";
  case CL_OUT_OF_HOST_MEMORY:                 return "Out of host memory";
  case CL_PROFILING_INFO_NOT_AVAILABLE:       return "Profiling information not available";
  case CL_MEM_COPY_OVERLAP:                   return "Memory copy overlap";
  case CL_IMAGE_FORMAT_MISMATCH:              return "Image format mismatch";
  case CL_IMAGE_FORMAT_NOT_SUPPORTED:         return "Image format not supported";
  case CL_BUILD_PROGRAM_FAILURE:              return "Program build failure";
  case CL_MAP_FAILURE:                        return "Map failure";
  case CL_INVALID_VALUE:                      return "Invalid value";
  case CL_INVALID_DEVICE_TYPE:                return "Invalid device type";
  case CL_INVALID_PLATFORM:                   return "Invalid platform";
  case CL_INVALID_DEVICE:                     return "Invalid device";
  case CL_INVALID_CONTEXT:                    return "Invalid context";
  case CL_INVALID_QUEUE_PROPERTIES:           return "Invalid queue properties";
  case CL_INVALID_COMMAND_QUEUE:              return "Invalid command queue";
  case CL_INVALID_HOST_PTR:                   return "Invalid host pointer";
  case CL_INVALID_MEM_OBJECT:                 return "Invalid memory object";
  case CL_INVALID_IMAGE_FORMAT_DESCRIPTOR:    return "Invalid image format descriptor";
  case CL_INVALID_IMAGE_SIZE:                 return "Invalid image size";
  case CL_INVALID_SAMPLER:                    return "Invalid sampler";
  case CL_INVALID_BINARY:                     return "Invalid binary";
  case CL_INVALID_BUILD_OPTIONS:              return "Invalid build options";
  case CL_INVALID_PROGRAM:                    return "Invalid program";
  case CL_INVALID_PROGRAM_EXECUTABLE:         return "Invalid program executable";
  case CL_INVALID_KERNEL_NAME:                return "Invalid kernel name";
  case CL_INVALID_KERNEL_DEFINITION:          return "Invalid kernel definition";
  case CL_INVALID_KERNEL:                     return "Invalid kernel";
  case CL_INVALID_ARG_INDEX:                  return "Invalid argument index";
  case CL_INVALID_ARG_VALUE:                  return "Invalid argument value";
  case CL_INVALID_ARG_SIZE:                   return "Invalid argument size";
  case CL_INVALID_KERNEL_ARGS:                return "Invalid kernel arguments";
  case CL_INVALID_WORK_DIMENSION:             return "Invalid work dimension";
  case CL_INVALID_WORK_GROUP_SIZE:            return "Invalid work group size";
  case CL_INVALID_WORK_ITEM_SIZE:             return "Invalid work item size";
  case CL_INVALID_GLOBAL_OFFSET:              return "Invalid global offset";
  case CL_INVALID_EVENT_WAIT_LIST:            return "Invalid event wait list";
  case CL_INVALID_EVENT:                      return "Invalid event";
  case CL_INVALID_OPERATION:                  return "Invalid operation";
  case CL_INVALID_GL_OBJECT:                  return "Invalid OpenGL object";
  case CL_INVALID_BUFFER_SIZE:                return "Invalid buffer size";
  case CL_INVALID_MIP_LEVEL:                  return "Invalid mip-map level";
  default:                                    return "Unknown";
  }
}

static void opencl_succeed_fatal(cl_int ret,
                                 const char *call,
                                 const char *file,
                                 int line) {
  if (ret != CL_SUCCESS) {
    futhark_panic(-1, "%s:%d: OpenCL call\n  %s\nfailed with error code %d (%s)\n",
                  file, line, call, ret, opencl_error_string(ret));
  }
}

static char* opencl_succeed_nonfatal(cl_int ret,
                                     const char *call,
                                     const char *file,
                                     int line) {
  if (ret != CL_SUCCESS) {
    return msgprintf("%s:%d: OpenCL call\n  %s\nfailed with error code %d (%s)\n",
                     file, line, call, ret, opencl_error_string(ret));
  } else {
    return NULL;
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

  char* program;
  int preferred_device_num;
  char* preferred_platform;
  char* preferred_device;
  int ignore_blacklist;

  char* dump_binary_to;
  char* load_binary_from;

  size_t default_group_size;
  size_t default_num_groups;
  size_t default_tile_size;
  size_t default_reg_tile_size;
  size_t default_threshold;

  int default_group_size_changed;
  int default_tile_size_changed;
  int num_build_opts;
  char* *build_opts;

  cl_command_queue queue;
  int queue_set;
};

static void backend_context_config_setup(struct futhark_context_config* cfg) {
  cfg->num_build_opts = 0;
  cfg->build_opts = (char**) malloc(sizeof(const char*));
  cfg->build_opts[0] = NULL;
  cfg->preferred_device_num = 0;
  cfg->preferred_platform = strdup("");
  cfg->preferred_device = strdup("");
  cfg->ignore_blacklist = 0;
  cfg->dump_binary_to = NULL;
  cfg->load_binary_from = NULL;
  cfg->program = strconcat(gpu_program);

  // The following are dummy sizes that mean the concrete defaults
  // will be set during initialisation via hardware-inspection-based
  // heuristics.
  cfg->default_group_size = 0;
  cfg->default_num_groups = 0;
  cfg->default_tile_size = 0;
  cfg->default_reg_tile_size = 0;
  cfg->default_threshold = 0;

  cfg->default_group_size_changed = 0;
  cfg->default_tile_size_changed = 0;

  cfg->queue_set = 0;
}

static void backend_context_config_teardown(struct futhark_context_config* cfg) {
  for (int i = 0; i < cfg->num_build_opts; i++) {
    free(cfg->build_opts[i]);
  }
  free(cfg->build_opts);
  free(cfg->dump_binary_to);
  free(cfg->load_binary_from);
  free(cfg->preferred_device);
  free(cfg->preferred_platform);
  free(cfg->program);
}

void futhark_context_config_add_build_option(struct futhark_context_config* cfg, const char *opt) {
  cfg->build_opts[cfg->num_build_opts] = strdup(opt);
  cfg->num_build_opts++;
  cfg->build_opts = (char**) realloc(cfg->build_opts, (cfg->num_build_opts+1) * sizeof(char*));
  cfg->build_opts[cfg->num_build_opts] = NULL;
}

void futhark_context_config_set_device(struct futhark_context_config *cfg, const char* s) {
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
  cfg->ignore_blacklist = 1;
}

void futhark_context_config_set_platform(struct futhark_context_config *cfg, const char *s) {
  free(cfg->preferred_platform);
  cfg->preferred_platform = strdup(s);
  cfg->ignore_blacklist = 1;
}

void futhark_context_config_set_command_queue(struct futhark_context_config *cfg, cl_command_queue q) {
  cfg->queue = q;
  cfg->queue_set = 1;
}

struct opencl_device_option {
  cl_platform_id platform;
  cl_device_id device;
  cl_device_type device_type;
  char *platform_name;
  char *device_name;
};

static char* opencl_platform_info(cl_platform_id platform,
                                  cl_platform_info param) {
  size_t req_bytes;
  char *info;

  OPENCL_SUCCEED_FATAL(clGetPlatformInfo(platform, param, 0, NULL, &req_bytes));

  info = (char*) malloc(req_bytes);

  OPENCL_SUCCEED_FATAL(clGetPlatformInfo(platform, param, req_bytes, info, NULL));

  return info;
}

static char* opencl_device_info(cl_device_id device,
                                cl_device_info param) {
  size_t req_bytes;
  char *info;

  OPENCL_SUCCEED_FATAL(clGetDeviceInfo(device, param, 0, NULL, &req_bytes));

  info = (char*) malloc(req_bytes);

  OPENCL_SUCCEED_FATAL(clGetDeviceInfo(device, param, req_bytes, info, NULL));

  return info;
}

static int is_blacklisted(const char *platform_name, const char *device_name,
                          const struct futhark_context_config *cfg) {
  if (strcmp(cfg->preferred_platform, "") != 0 ||
      strcmp(cfg->preferred_device, "") != 0) {
    return 0;
  } else if (strstr(platform_name, "Apple") != NULL &&
             strstr(device_name, "Intel(R) Core(TM)") != NULL) {
    return 1;
  } else {
    return 0;
  }
}

static void opencl_all_device_options(struct opencl_device_option **devices_out,
                                      size_t *num_devices_out) {
  size_t num_devices = 0, num_devices_added = 0;

  cl_platform_id *all_platforms;
  cl_uint *platform_num_devices;

  cl_uint num_platforms;

  // Find the number of platforms.
  OPENCL_SUCCEED_FATAL(clGetPlatformIDs(0, NULL, &num_platforms));

  // Make room for them.
  all_platforms = calloc(num_platforms, sizeof(cl_platform_id));
  platform_num_devices = calloc(num_platforms, sizeof(cl_uint));

  // Fetch all the platforms.
  OPENCL_SUCCEED_FATAL(clGetPlatformIDs(num_platforms, all_platforms, NULL));

  // Count the number of devices for each platform, as well as the
  // total number of devices.
  for (cl_uint i = 0; i < num_platforms; i++) {
    if (clGetDeviceIDs(all_platforms[i], CL_DEVICE_TYPE_ALL,
                       0, NULL, &platform_num_devices[i]) == CL_SUCCESS) {
      num_devices += platform_num_devices[i];
    } else {
      platform_num_devices[i] = 0;
    }
  }

  // Make room for all the device options.
  struct opencl_device_option *devices =
    calloc(num_devices, sizeof(struct opencl_device_option));

  // Loop through the platforms, getting information about their devices.
  for (cl_uint i = 0; i < num_platforms; i++) {
    cl_platform_id platform = all_platforms[i];
    cl_uint num_platform_devices = platform_num_devices[i];

    if (num_platform_devices == 0) {
      continue;
    }

    char *platform_name = opencl_platform_info(platform, CL_PLATFORM_NAME);
    cl_device_id *platform_devices =
      calloc(num_platform_devices, sizeof(cl_device_id));

    // Fetch all the devices.
    OPENCL_SUCCEED_FATAL(clGetDeviceIDs(platform, CL_DEVICE_TYPE_ALL,
                                        num_platform_devices, platform_devices, NULL));

    // Loop through the devices, adding them to the devices array.
    for (cl_uint i = 0; i < num_platform_devices; i++) {
      char *device_name = opencl_device_info(platform_devices[i], CL_DEVICE_NAME);
      devices[num_devices_added].platform = platform;
      devices[num_devices_added].device = platform_devices[i];
      OPENCL_SUCCEED_FATAL(clGetDeviceInfo(platform_devices[i], CL_DEVICE_TYPE,
                                           sizeof(cl_device_type),
                                           &devices[num_devices_added].device_type,
                                           NULL));
      // We don't want the structs to share memory, so copy the platform name.
      // Each device name is already unique.
      devices[num_devices_added].platform_name = strclone(platform_name);
      devices[num_devices_added].device_name = device_name;
      num_devices_added++;
    }
    free(platform_devices);
    free(platform_name);
  }
  free(all_platforms);
  free(platform_num_devices);

  *devices_out = devices;
  *num_devices_out = num_devices;
}

void futhark_context_config_select_device_interactively(struct futhark_context_config *cfg) {
  struct opencl_device_option *devices;
  size_t num_devices;

  opencl_all_device_options(&devices, &num_devices);

  printf("Choose OpenCL device:\n");
  const char *cur_platform = "";
  for (size_t i = 0; i < num_devices; i++) {
    struct opencl_device_option device = devices[i];
    if (strcmp(cur_platform, device.platform_name) != 0) {
      printf("Platform: %s\n", device.platform_name);
      cur_platform = device.platform_name;
    }
    printf("[%d] %s\n", (int)i, device.device_name);
  }

  int selection;
  printf("Choice: ");
  if (scanf("%d", &selection) == 1) {
    cfg->preferred_platform = "";
    cfg->preferred_device = "";
    cfg->preferred_device_num = selection;
    cfg->ignore_blacklist = 1;
  }

  // Free all the platform and device names.
  for (size_t j = 0; j < num_devices; j++) {
    free(devices[j].platform_name);
    free(devices[j].device_name);
  }
  free(devices);
}

void futhark_context_config_list_devices(struct futhark_context_config *cfg) {
  (void)cfg;
  struct opencl_device_option *devices;
  size_t num_devices;

  opencl_all_device_options(&devices, &num_devices);

  const char *cur_platform = "";
  for (size_t i = 0; i < num_devices; i++) {
    struct opencl_device_option device = devices[i];
    if (strcmp(cur_platform, device.platform_name) != 0) {
      printf("Platform: %s\n", device.platform_name);
      cur_platform = device.platform_name;
    }
    printf("[%d]: %s\n", (int)i, device.device_name);
  }

  // Free all the platform and device names.
  for (size_t j = 0; j < num_devices; j++) {
    free(devices[j].platform_name);
    free(devices[j].device_name);
  }
  free(devices);
}

const char* futhark_context_config_get_program(struct futhark_context_config *cfg) {
  return cfg->program;
}

void futhark_context_config_set_program(struct futhark_context_config *cfg, const char *s) {
  free(cfg->program);
  cfg->program = strdup(s);
}

void futhark_context_config_dump_binary_to(struct futhark_context_config *cfg, const char *path) {
  free(cfg->dump_binary_to);
  cfg->dump_binary_to = strdup(path);
}

void futhark_context_config_load_binary_from(struct futhark_context_config *cfg, const char *path) {
  free(cfg->load_binary_from);
  cfg->load_binary_from = strdup(path);
}

void futhark_context_config_set_default_thread_block_size(struct futhark_context_config *cfg, int size) {
  cfg->default_group_size = size;
  cfg->default_group_size_changed = 1;
}

void futhark_context_config_set_default_group_size(struct futhark_context_config *cfg, int size) {
  futhark_context_config_set_default_thread_block_size(cfg, size);
}

void futhark_context_config_set_default_grid_size(struct futhark_context_config *cfg, int num) {
  cfg->default_num_groups = num;
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
    cfg->default_group_size = new_value;
    return 0;
  }
  if (strcmp(param_name, "default_grid_size") == 0 ||
      strcmp(param_name, "default_num_groups") == 0) {
    cfg->default_num_groups = new_value;
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
  struct program* program;
  bool program_initialised;
  // Uniform fields above.

  cl_mem global_failure;
  cl_mem global_failure_args;
  struct tuning_params tuning_params;
  // True if a potentially failing kernel has been enqueued.
  cl_int failure_is_an_option;
  int total_runs;
  long int total_runtime;
  int64_t peak_mem_usage_device;
  int64_t cur_mem_usage_device;

  cl_device_id device;
  cl_context ctx;
  cl_command_queue queue;
  cl_program clprogram;

  struct free_list gpu_free_list;

  size_t max_thread_block_size;
  size_t max_num_groups;
  size_t max_tile_size;
  size_t max_threshold;
  size_t max_shared_memory;
  size_t max_registers;
  size_t max_cache;

  size_t lockstep_width;

  struct builtin_kernels* kernels;
};

static cl_build_status build_gpu_program(cl_program program, cl_device_id device, const char* options, char** log) {
  cl_int clBuildProgram_error = clBuildProgram(program, 1, &device, options, NULL, NULL);

  // Avoid termination due to CL_BUILD_PROGRAM_FAILURE
  if (clBuildProgram_error != CL_SUCCESS &&
      clBuildProgram_error != CL_BUILD_PROGRAM_FAILURE) {
    OPENCL_SUCCEED_FATAL(clBuildProgram_error);
  }

  cl_build_status build_status;
  OPENCL_SUCCEED_FATAL(clGetProgramBuildInfo(program,
                                             device,
                                             CL_PROGRAM_BUILD_STATUS,
                                             sizeof(cl_build_status),
                                             &build_status,
                                             NULL));

  if (build_status != CL_BUILD_SUCCESS) {
    char *build_log;
    size_t ret_val_size;
    OPENCL_SUCCEED_FATAL(clGetProgramBuildInfo(program, device, CL_PROGRAM_BUILD_LOG, 0, NULL, &ret_val_size));

    build_log = (char*) malloc(ret_val_size+1);
    OPENCL_SUCCEED_FATAL(clGetProgramBuildInfo(program, device, CL_PROGRAM_BUILD_LOG, ret_val_size, build_log, NULL));

    // The spec technically does not say whether the build log is
    // zero-terminated, so let's be careful.
    build_log[ret_val_size] = '\0';
    *log = build_log;
  }

  return build_status;
}

static char* mk_compile_opts(struct futhark_context *ctx,
                             const char *extra_build_opts[],
                             struct opencl_device_option device_option) {
  int compile_opts_size = 1024;

  for (int i = 0; i < ctx->cfg->num_tuning_params; i++) {
    compile_opts_size += strlen(ctx->cfg->tuning_param_names[i]) + 20;
  }

  char** macro_names;
  int64_t* macro_vals;
  int num_macros = gpu_macros(ctx, &macro_names, &macro_vals);

  for (int i = 0; extra_build_opts[i] != NULL; i++) {
    compile_opts_size += strlen(extra_build_opts[i] + 1);
  }

  for (int i = 0; i < num_macros; i++) {
    compile_opts_size += strlen(macro_names[i]) + 1 + 20;
  }

  char *compile_opts = (char*) malloc(compile_opts_size);

  int w = snprintf(compile_opts, compile_opts_size,
                   "-DLOCKSTEP_WIDTH=%d ",
                   (int)ctx->lockstep_width);

  w += snprintf(compile_opts+w, compile_opts_size-w,
                "-D%s=%d ",
                "max_thread_block_size",
                (int)ctx->max_thread_block_size);

  w += snprintf(compile_opts+w, compile_opts_size-w,
                "-D%s=%d ",
                "max_shared_memory",
                (int)ctx->max_shared_memory);

  w += snprintf(compile_opts+w, compile_opts_size-w,
                "-D%s=%d ",
                "max_registers",
                (int)ctx->max_registers);

  for (int i = 0; i < ctx->cfg->num_tuning_params; i++) {
    w += snprintf(compile_opts+w, compile_opts_size-w,
                  "-D%s=%d ",
                  ctx->cfg->tuning_param_vars[i],
                  (int)ctx->cfg->tuning_params[i]);
  }

  for (int i = 0; extra_build_opts[i] != NULL; i++) {
    w += snprintf(compile_opts+w, compile_opts_size-w,
                  "%s ", extra_build_opts[i]);
  }

  for (int i = 0; i < num_macros; i++) {
    w += snprintf(compile_opts+w, compile_opts_size-w,
                  "-D%s=%zu ", macro_names[i], macro_vals[i]);
  }

  w += snprintf(compile_opts+w, compile_opts_size-w,
                "-DTR_BLOCK_DIM=%d -DTR_TILE_DIM=%d -DTR_ELEMS_PER_THREAD=%d ",
                TR_BLOCK_DIM, TR_TILE_DIM, TR_ELEMS_PER_THREAD);

  // Oclgrind claims to support cl_khr_fp16, but this is not actually
  // the case.
  if (strcmp(device_option.platform_name, "Oclgrind") == 0) {
    w += snprintf(compile_opts+w, compile_opts_size-w, "-DEMULATE_F16 ");
  }

  free(macro_names);
  free(macro_vals);

  return compile_opts;
}

static cl_event* opencl_event_new(struct futhark_context* ctx) {
  if (ctx->profiling && !ctx->profiling_paused) {
    return malloc(sizeof(cl_event));
  } else {
    return NULL;
  }
}

static int opencl_event_report(struct str_builder* sb, cl_event* e) {
  cl_int err;
  cl_ulong start_t, end_t;

  assert(e != NULL);
  OPENCL_SUCCEED_FATAL(clGetEventProfilingInfo(*e,
                                               CL_PROFILING_COMMAND_START,
                                               sizeof(start_t),
                                               &start_t,
                                               NULL));
  OPENCL_SUCCEED_FATAL(clGetEventProfilingInfo(*e,
                                               CL_PROFILING_COMMAND_END,
                                               sizeof(end_t),
                                               &end_t,
                                               NULL));

  // OpenCL provides nanosecond resolution, but we want microseconds.
  str_builder(sb, ",\"duration\":%f", (end_t - start_t)/1000.0);

  OPENCL_SUCCEED_FATAL(clReleaseEvent(*e));

  free(e);

  return 0;
}

int futhark_context_sync(struct futhark_context* ctx) {
  // Check for any delayed error.
  cl_int failure_idx = -1;
  if (ctx->failure_is_an_option) {
    OPENCL_SUCCEED_OR_RETURN(
                             clEnqueueReadBuffer(ctx->queue,
                                                 ctx->global_failure,
                                                 CL_FALSE,
                                                 0, sizeof(cl_int), &failure_idx,
                                                 0, NULL, NULL));
    ctx->failure_is_an_option = 0;
  }

  OPENCL_SUCCEED_OR_RETURN(clFinish(ctx->queue));

  if (failure_idx >= 0) {
    // We have to clear global_failure so that the next entry point
    // is not considered a failure from the start.
    cl_int no_failure = -1;
    OPENCL_SUCCEED_OR_RETURN(
                             clEnqueueWriteBuffer(ctx->queue, ctx->global_failure, CL_TRUE,
                                                  0, sizeof(cl_int), &no_failure,
                                                  0, NULL, NULL));

    int64_t args[max_failure_args+1];
    OPENCL_SUCCEED_OR_RETURN(
                             clEnqueueReadBuffer(ctx->queue,
                                                 ctx->global_failure_args,
                                                 CL_TRUE,
                                                 0, sizeof(args), &args,
                                                 0, NULL, NULL));

    ctx->error = get_failure_msg(failure_idx, args);

    return FUTHARK_PROGRAM_ERROR;
  }
  return 0;
}


// We take as input several strings representing the program, because
// C does not guarantee that the compiler supports particularly large
// literals.  Notably, Visual C has a limit of 2048 characters.  The
// array must be NULL-terminated.
static void setup_opencl_with_command_queue(struct futhark_context *ctx,
                                            cl_command_queue queue,
                                            const char* extra_build_opts[],
                                            const char* cache_fname) {
  int error;

  free_list_init(&ctx->gpu_free_list);
  ctx->queue = queue;

  OPENCL_SUCCEED_FATAL(clGetCommandQueueInfo(ctx->queue, CL_QUEUE_CONTEXT, sizeof(cl_context), &ctx->ctx, NULL));

  // Fill out the device info.  This is redundant work if we are
  // called from setup_opencl() (which is the common case), but I
  // doubt it matters much.
  struct opencl_device_option device_option;
  OPENCL_SUCCEED_FATAL(clGetCommandQueueInfo(ctx->queue, CL_QUEUE_DEVICE,
                                             sizeof(cl_device_id),
                                             &device_option.device,
                                             NULL));
  OPENCL_SUCCEED_FATAL(clGetDeviceInfo(device_option.device, CL_DEVICE_PLATFORM,
                                       sizeof(cl_platform_id),
                                       &device_option.platform,
                                       NULL));
  OPENCL_SUCCEED_FATAL(clGetDeviceInfo(device_option.device, CL_DEVICE_TYPE,
                                       sizeof(cl_device_type),
                                       &device_option.device_type,
                                       NULL));
  device_option.platform_name = opencl_platform_info(device_option.platform, CL_PLATFORM_NAME);
  device_option.device_name = opencl_device_info(device_option.device, CL_DEVICE_NAME);

  ctx->device = device_option.device;

  if (f64_required) {
    cl_uint supported;
    OPENCL_SUCCEED_FATAL(clGetDeviceInfo(device_option.device, CL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE,
                                         sizeof(cl_uint), &supported, NULL));
    if (!supported) {
      futhark_panic(1, "Program uses double-precision floats, but this is not supported on the chosen device: %s\n",
                    device_option.device_name);
    }
  }

  size_t max_thread_block_size;
  OPENCL_SUCCEED_FATAL(clGetDeviceInfo(device_option.device, CL_DEVICE_MAX_WORK_GROUP_SIZE,
                                       sizeof(size_t), &max_thread_block_size, NULL));

  size_t max_tile_size = sqrt(max_thread_block_size);

  cl_ulong max_shared_memory;
  OPENCL_SUCCEED_FATAL(clGetDeviceInfo(device_option.device, CL_DEVICE_LOCAL_MEM_SIZE,
                                       sizeof(size_t), &max_shared_memory, NULL));

  // Futhark reserves 4 bytes for bookkeeping information.
  max_shared_memory -= 4;

  // The OpenCL implementation may reserve some local memory bytes for
  // various purposes.  In principle, we should use
  // clGetKernelWorkGroupInfo() to figure out for each kernel how much
  // is actually available, but our current code generator design
  // makes this infeasible.  Instead, we have this nasty hack where we
  // arbitrarily subtract some bytes, based on empirical measurements
  // (but which might be arbitrarily wrong).  Fortunately, we rarely
  // try to really push the local memory usage.
  if (strstr(device_option.platform_name, "NVIDIA CUDA") != NULL) {
    max_shared_memory -= 12;
  } else if (strstr(device_option.platform_name, "AMD") != NULL) {
    max_shared_memory -= 16;
  }

  // Make sure this function is defined.
  post_opencl_setup(ctx, &device_option);

  if (max_thread_block_size < ctx->cfg->default_group_size) {
    if (ctx->cfg->default_group_size_changed) {
      fprintf(stderr, "Note: Device limits default group size to %zu (down from %zu).\n",
              max_thread_block_size, ctx->cfg->default_group_size);
    }
    ctx->cfg->default_group_size = max_thread_block_size;
  }

  if (max_tile_size < ctx->cfg->default_tile_size) {
    if (ctx->cfg->default_tile_size_changed) {
      fprintf(stderr, "Note: Device limits default tile size to %zu (down from %zu).\n",
              max_tile_size, ctx->cfg->default_tile_size);
    }
    ctx->cfg->default_tile_size = max_tile_size;
  }


  cl_ulong cache_size;
  OPENCL_SUCCEED_FATAL(clGetDeviceInfo(device_option.device, CL_DEVICE_GLOBAL_MEM_CACHE_SIZE,
                                       sizeof(cache_size), &cache_size, NULL));

  if (cache_size == 0) {
    // Some code assumes nonzero cache.
    cache_size = 1024*1024;
  }

  ctx->max_cache = cache_size;

  ctx->max_registers = 1<<16; // I cannot find a way to query for this.

  ctx->max_thread_block_size = max_thread_block_size;
  ctx->max_tile_size = max_tile_size; // No limit.
  ctx->max_threshold = ctx->max_num_groups = 0; // No limit.
  ctx->max_shared_memory = max_shared_memory;

  // Now we go through all the sizes, clamp them to the valid range,
  // or set them to the default.
  for (int i = 0; i < ctx->cfg->num_tuning_params; i++) {
    const char *size_class = ctx->cfg->tuning_param_classes[i];
    int64_t *size_value = &ctx->cfg->tuning_params[i];
    const char* size_name = ctx->cfg->tuning_param_names[i];
    int64_t max_value = 0, default_value = 0;

    if (strstr(size_class, "thread_block_size") == size_class) {
      max_value = max_thread_block_size;
      default_value = ctx->cfg->default_group_size;
    } else if (strstr(size_class, "grid_size") == size_class) {
      max_value = max_thread_block_size; // Futhark assumes this constraint.
      default_value = ctx->cfg->default_num_groups;
      // XXX: as a quick and dirty hack, use twice as many threads for
      // histograms by default.  We really should just be smarter
      // about sizes somehow.
      if (strstr(size_name, ".seghist_") != NULL) {
        default_value *= 2;
      }
    } else if (strstr(size_class, "tile_size") == size_class) {
      max_value = sqrt(max_thread_block_size);
      default_value = ctx->cfg->default_tile_size;
    } else if (strstr(size_class, "reg_tile_size") == size_class) {
      max_value = 0; // No limit.
      default_value = ctx->cfg->default_reg_tile_size;
    } else if (strstr(size_class, "threshold") == size_class) {
      // Threshold can be as large as it takes.
      default_value = ctx->cfg->default_threshold;
    } else {
      // Bespoke sizes have no limit or default.
    }
    if (*size_value == 0) {
      *size_value = default_value;
    } else if (max_value > 0 && *size_value > max_value) {
      fprintf(stderr, "Note: Device limits %s to %d (down from %d)\n",
              size_name, (int)max_value, (int)*size_value);
      *size_value = max_value;
    }
  }

  if (ctx->lockstep_width == 0) {
    ctx->lockstep_width = 1;
  }

  if (ctx->cfg->logging) {
    fprintf(stderr, "Lockstep width: %d\n", (int)ctx->lockstep_width);
    fprintf(stderr, "Default thread block size: %d\n", (int)ctx->cfg->default_group_size);
    fprintf(stderr, "Default number of thread blocks: %d\n", (int)ctx->cfg->default_num_groups);
  }

  char *compile_opts = mk_compile_opts(ctx, extra_build_opts, device_option);

  if (ctx->cfg->logging) {
    fprintf(stderr, "OpenCL compiler options: %s\n", compile_opts);
  }

  const char* opencl_src = ctx->cfg->program;
  cl_program prog;
  error = CL_SUCCESS;

  struct cache_hash h;

  int loaded_from_cache = 0;
  if (ctx->cfg->load_binary_from == NULL) {
    size_t src_size = 0;

    if (cache_fname != NULL) {
      if (ctx->cfg->logging) {
        fprintf(stderr, "Restoring cache from from %s...\n", cache_fname);
      }
      cache_hash_init(&h);
      cache_hash(&h, opencl_src, strlen(opencl_src));
      cache_hash(&h, compile_opts, strlen(compile_opts));

      unsigned char *buf;
      size_t bufsize;
      errno = 0;
      if (cache_restore(cache_fname, &h, &buf, &bufsize) != 0) {
        if (ctx->cfg->logging) {
          fprintf(stderr, "Failed to restore cache (errno: %s)\n", strerror(errno));
        }
      } else {
        if (ctx->cfg->logging) {
          fprintf(stderr, "Cache restored; loading OpenCL binary...\n");
        }

        cl_int status = 0;
        prog = clCreateProgramWithBinary(ctx->ctx, 1, &device_option.device,
                                         &bufsize, (const unsigned char**)&buf,
                                         &status, &error);
        if (status == CL_SUCCESS) {
          loaded_from_cache = 1;
          if (ctx->cfg->logging) {
            fprintf(stderr, "Loading succeeded.\n");
          }
        } else {
          if (ctx->cfg->logging) {
            fprintf(stderr, "Loading failed.\n");
          }
        }
      }
    }

    if (!loaded_from_cache) {
      if (ctx->cfg->logging) {
        fprintf(stderr, "Creating OpenCL program...\n");
      }

      const char* src_ptr[] = {opencl_src};
      prog = clCreateProgramWithSource(ctx->ctx, 1, src_ptr, &src_size, &error);
      OPENCL_SUCCEED_FATAL(error);
    }
  } else {
    if (ctx->cfg->logging) {
      fprintf(stderr, "Loading OpenCL binary from %s...\n", ctx->cfg->load_binary_from);
    }
    size_t binary_size;
    unsigned char *fut_opencl_bin =
      (unsigned char*) slurp_file(ctx->cfg->load_binary_from, &binary_size);
    assert(fut_opencl_bin != NULL);
    const unsigned char *binaries[1] = { fut_opencl_bin };
    cl_int status = 0;

    prog = clCreateProgramWithBinary(ctx->ctx, 1, &device_option.device,
                                     &binary_size, binaries,
                                     &status, &error);

    OPENCL_SUCCEED_FATAL(status);
    OPENCL_SUCCEED_FATAL(error);
  }

  if (ctx->cfg->logging) {
    fprintf(stderr, "Building OpenCL program...\n");
  }
  char* build_log;
  cl_build_status status =
    build_gpu_program(prog, device_option.device, compile_opts, &build_log);
  free(compile_opts);

  if (status != CL_BUILD_SUCCESS) {
    ctx->error = msgprintf("Compilation of OpenCL program failed.\nBuild log:\n%s",
                           build_log);
    // We are giving up on initialising this OpenCL context. That also
    // means we need to free all the OpenCL bits we have managed to
    // allocate thus far, as futhark_context_free() will not touch
    // these unless initialisation was completely successful.
    (void)clReleaseProgram(prog);
    (void)clReleaseCommandQueue(ctx->queue);
    (void)clReleaseContext(ctx->ctx);
    free(build_log);
    return;
  }

  size_t binary_size = 0;
  unsigned char *binary = NULL;
  int store_in_cache = cache_fname != NULL && !loaded_from_cache;
  if (store_in_cache || ctx->cfg->dump_binary_to != NULL) {
    OPENCL_SUCCEED_FATAL(clGetProgramInfo(prog, CL_PROGRAM_BINARY_SIZES,
                                          sizeof(size_t), &binary_size, NULL));
    binary = (unsigned char*) malloc(binary_size);
    OPENCL_SUCCEED_FATAL(clGetProgramInfo(prog, CL_PROGRAM_BINARIES,
                                          sizeof(unsigned char*), &binary, NULL));
  }

  if (store_in_cache) {
    if (ctx->cfg->logging) {
      fprintf(stderr, "Caching OpenCL binary in %s...\n", cache_fname);
    }
    if (cache_store(cache_fname, &h, binary, binary_size) != 0) {
      printf("Failed to cache binary: %s\n", strerror(errno));
    }
  }

  if (ctx->cfg->dump_binary_to != NULL) {
    if (ctx->cfg->logging) {
      fprintf(stderr, "Dumping OpenCL binary to %s...\n", ctx->cfg->dump_binary_to);
    }
    dump_file(ctx->cfg->dump_binary_to, binary, binary_size);
  }

  ctx->clprogram = prog;
}

static struct opencl_device_option get_preferred_device(struct futhark_context *ctx,
                                                        const struct futhark_context_config *cfg) {
  struct opencl_device_option *devices;
  size_t num_devices;

  opencl_all_device_options(&devices, &num_devices);

  int num_device_matches = 0;

  for (size_t i = 0; i < num_devices; i++) {
    struct opencl_device_option device = devices[i];
    if (strstr(device.platform_name, cfg->preferred_platform) != NULL &&
        strstr(device.device_name, cfg->preferred_device) != NULL &&
        (cfg->ignore_blacklist ||
         !is_blacklisted(device.platform_name, device.device_name, cfg)) &&
        num_device_matches++ == cfg->preferred_device_num) {
      // Free all the platform and device names, except the ones we have chosen.
      for (size_t j = 0; j < num_devices; j++) {
        if (j != i) {
          free(devices[j].platform_name);
          free(devices[j].device_name);
        }
      }
      free(devices);
      return device;
    }
  }

  ctx->error = strdup("Could not find acceptable OpenCL device.\n");
  struct opencl_device_option device;
  return device;
}

static void setup_opencl(struct futhark_context *ctx,
                         const char *extra_build_opts[],
                         const char* cache_fname) {
  struct opencl_device_option device_option = get_preferred_device(ctx, ctx->cfg);

  if (ctx->error != NULL) {
    return;
  }

  if (ctx->cfg->logging) {
    fprintf(stderr, "Using platform: %s\n", device_option.platform_name);
    fprintf(stderr, "Using device: %s\n", device_option.device_name);
  }

  // Note that NVIDIA's OpenCL requires the platform property
  cl_context_properties properties[] = {
    CL_CONTEXT_PLATFORM,
    (cl_context_properties)device_option.platform,
    0
  };

  cl_int clCreateContext_error;
  ctx->ctx = clCreateContext(properties, 1, &device_option.device, NULL, NULL, &clCreateContext_error);
  OPENCL_SUCCEED_FATAL(clCreateContext_error);

  cl_int clCreateCommandQueue_error;
  cl_command_queue queue =
    clCreateCommandQueue(ctx->ctx,
                         device_option.device,
                         ctx->cfg->profiling ? CL_QUEUE_PROFILING_ENABLE : 0,
                         &clCreateCommandQueue_error);
  OPENCL_SUCCEED_FATAL(clCreateCommandQueue_error);

  setup_opencl_with_command_queue(ctx, queue, extra_build_opts, cache_fname);
}

struct builtin_kernels* init_builtin_kernels(struct futhark_context* ctx);
void free_builtin_kernels(struct futhark_context* ctx, struct builtin_kernels* kernels);

int backend_context_setup(struct futhark_context* ctx) {
  ctx->lockstep_width = 0; // Real value set later.
  ctx->failure_is_an_option = 0;
  ctx->total_runs = 0;
  ctx->total_runtime = 0;
  ctx->peak_mem_usage_device = 0;
  ctx->cur_mem_usage_device = 0;
  ctx->kernels = NULL;

  if (ctx->cfg->queue_set) {
    setup_opencl_with_command_queue(ctx, ctx->cfg->queue, (const char**)ctx->cfg->build_opts, ctx->cfg->cache_fname);
  } else {
    setup_opencl(ctx, (const char**)ctx->cfg->build_opts, ctx->cfg->cache_fname);
  }

  if (ctx->error != NULL) {
    return 1;
  }

  cl_int error;
  cl_int no_error = -1;
  ctx->global_failure =
    clCreateBuffer(ctx->ctx,
                   CL_MEM_READ_WRITE | CL_MEM_COPY_HOST_PTR,
                   sizeof(cl_int), &no_error, &error);
  OPENCL_SUCCEED_OR_RETURN(error);

  // The +1 is to avoid zero-byte allocations.
  ctx->global_failure_args =
    clCreateBuffer(ctx->ctx,
                   CL_MEM_READ_WRITE,
                   sizeof(int64_t)*(max_failure_args+1), NULL, &error);
  OPENCL_SUCCEED_OR_RETURN(error);

  if ((ctx->kernels = init_builtin_kernels(ctx)) == NULL) {
    return 1;
  }

  return FUTHARK_SUCCESS;
}

static int gpu_free_all(struct futhark_context *ctx);

void backend_context_teardown(struct futhark_context* ctx) {
  if (ctx->kernels != NULL) {
    free_builtin_kernels(ctx, ctx->kernels);
    OPENCL_SUCCEED_FATAL(clReleaseMemObject(ctx->global_failure));
    OPENCL_SUCCEED_FATAL(clReleaseMemObject(ctx->global_failure_args));
    (void)gpu_free_all(ctx);
    (void)clReleaseProgram(ctx->clprogram);
    (void)clReleaseCommandQueue(ctx->queue);
    (void)clReleaseContext(ctx->ctx);
  }
  free_list_destroy(&ctx->gpu_free_list);
}

cl_command_queue futhark_context_get_command_queue(struct futhark_context* ctx) {
  return ctx->queue;
}

// GPU ABSTRACTION LAYER

// Types.

typedef cl_kernel gpu_kernel;
typedef cl_mem gpu_mem;

static void gpu_create_kernel(struct futhark_context *ctx,
                              gpu_kernel* kernel,
                              const char* name) {
  if (ctx->debugging) {
    fprintf(ctx->log, "Creating kernel %s.\n", name);
  }
  cl_int error;
  *kernel = clCreateKernel(ctx->clprogram, name, &error);
  OPENCL_SUCCEED_FATAL(error);
}

static void gpu_free_kernel(struct futhark_context *ctx,
                            gpu_kernel kernel) {
  (void)ctx;
  clReleaseKernel(kernel);
}

static int gpu_scalar_to_device(struct futhark_context* ctx,
                                gpu_mem dst, size_t offset, size_t size,
                                void *src) {
  cl_event* event = opencl_event_new(ctx);
  if (event != NULL) {
    add_event(ctx,
              "copy_scalar_to_dev",
              strdup(""),
              event,
              (event_report_fn)opencl_event_report);
  }
  OPENCL_SUCCEED_OR_RETURN
    (clEnqueueWriteBuffer
     (ctx->queue, dst, CL_TRUE,
      offset, size, src, 0, NULL, event));
  return 0;
}

static int gpu_scalar_from_device(struct futhark_context* ctx,
                                  void *dst,
                                  gpu_mem src, size_t offset, size_t size) {
  cl_event* event = opencl_event_new(ctx);
  if (event != NULL) {
    add_event(ctx,
              "copy_scalar_from_dev",
              strdup(""),
              event,
              (event_report_fn)opencl_event_report);
  }
  OPENCL_SUCCEED_OR_RETURN
    (clEnqueueReadBuffer
     (ctx->queue, src, ctx->failure_is_an_option ? CL_FALSE : CL_TRUE,
      offset, size, dst, 0, NULL, event));
  return 0;
}

static int gpu_memcpy(struct futhark_context* ctx,
                      gpu_mem dst, int64_t dst_offset,
                      gpu_mem src, int64_t src_offset,
                      int64_t nbytes) {
  if (nbytes > 0) {
    cl_event* event = opencl_event_new(ctx);
    if (event != NULL) {
      add_event(ctx,
                "copy_dev_to_dev",
                strdup(""),
                event,
                (event_report_fn)opencl_event_report);
    }
    // OpenCL swaps the usual order of operands for memcpy()-like
    // functions.  The order below is not a typo.
    OPENCL_SUCCEED_OR_RETURN
      (clEnqueueCopyBuffer
       (ctx->queue, src, dst, src_offset, dst_offset, nbytes,
        0, NULL, event));
    if (ctx->debugging) {
      OPENCL_SUCCEED_FATAL(clFinish(ctx->queue));
    }
  }
  return FUTHARK_SUCCESS;
}

static int memcpy_host2gpu(struct futhark_context* ctx, bool sync,
                           gpu_mem dst, int64_t dst_offset,
                           const unsigned char* src, int64_t src_offset,
                           int64_t nbytes) {
  if (nbytes > 0) {
    cl_event* event = opencl_event_new(ctx);
    if (event != NULL) {
      add_event(ctx,
                "copy_host_to_dev",
                strdup(""),
                event,
                (event_report_fn)opencl_event_report);
    }
    OPENCL_SUCCEED_OR_RETURN
      (clEnqueueWriteBuffer(ctx->queue,
                            dst,
                            sync ? CL_TRUE : CL_FALSE,
                            (size_t)dst_offset, (size_t)nbytes,
                            src + src_offset,
                            0, NULL, event));
    if (ctx->debugging) {
      OPENCL_SUCCEED_FATAL(clFinish(ctx->queue));
    }
  }
  return FUTHARK_SUCCESS;
}

static int memcpy_gpu2host(struct futhark_context* ctx, bool sync,
                           unsigned char* dst, int64_t dst_offset,
                           gpu_mem src, int64_t src_offset,
                           int64_t nbytes) {
  if (nbytes > 0) {
    cl_event* event = opencl_event_new(ctx);
    if (event != NULL) {
      add_event(ctx,
                "copy_dev_to_host",
                strdup(""),
                event,
                (event_report_fn)opencl_event_report);
    }
    OPENCL_SUCCEED_OR_RETURN
      (clEnqueueReadBuffer(ctx->queue, src,
                           ctx->failure_is_an_option ? CL_FALSE
                           : sync ? CL_TRUE : CL_FALSE,
                           src_offset, nbytes,
                           dst + dst_offset,
                           0, NULL, event));
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
  int64_t time_start = 0, time_end = 0;

  cl_event* event = opencl_event_new(ctx);
  if (event != NULL) {
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
              (event_report_fn)opencl_event_report);
  }

  if (ctx->debugging) {
    time_start = get_wall_time();
  }

  // Some implementations do not work with 0-byte shared memory.
  if (shared_mem_bytes == 0) {
    shared_mem_bytes = 4;
  }

  OPENCL_SUCCEED_OR_RETURN
    (clSetKernelArg(kernel, 0, shared_mem_bytes, NULL));
  for (int i = 0; i < num_args; i++) {
    OPENCL_SUCCEED_OR_RETURN
      (clSetKernelArg(kernel, i+1, args_sizes[i], args[i]));
  }

  const size_t global_work_size[3] =
    {(size_t)grid[0]*block[0],
     (size_t)grid[1]*block[1],
     (size_t)grid[2]*block[2]};
  const size_t local_work_size[3] =
    {block[0],
     block[1],
     block[2]};

  OPENCL_SUCCEED_OR_RETURN
    (clEnqueueNDRangeKernel(ctx->queue,
                            kernel,
                            3, NULL, global_work_size, local_work_size,
                            0, NULL, event));

  if (ctx->debugging) {
    OPENCL_SUCCEED_FATAL(clFinish(ctx->queue));
    time_end = get_wall_time();
    long int time_diff = time_end - time_start;
    fprintf(ctx->log, "  runtime: %ldus\n", time_diff);
  }
  if (ctx->logging) {
    fprintf(ctx->log, "\n");
  }

  return FUTHARK_SUCCESS;
}

// Allocate memory from driver. The problem is that OpenCL may perform
// lazy allocation, so we cannot know whether an allocation succeeded
// until the first time we try to use it.  Hence we immediately
// perform a write to see if the allocation succeeded.  This is slow,
// but the assumption is that this operation will be rare (most things
// will go through the free list).
static int gpu_alloc_actual(struct futhark_context *ctx, size_t size, gpu_mem *mem_out) {
  int error;
  *mem_out = clCreateBuffer(ctx->ctx, CL_MEM_READ_WRITE, size, NULL, &error);

  OPENCL_SUCCEED_OR_RETURN(error);

  int x = 2;
  error = clEnqueueWriteBuffer(ctx->queue, *mem_out,
                               CL_TRUE,
                               0, sizeof(x), &x,
                               0, NULL, NULL);

  // No need to wait for completion here. clWaitForEvents() cannot
  // return mem object allocation failures. This implies that the
  // buffer is faulted onto the device on enqueue. (Observation by
  // Andreas Kloeckner.)

  if (error == CL_MEM_OBJECT_ALLOCATION_FAILURE) {
    return FUTHARK_OUT_OF_MEMORY;
  }
  OPENCL_SUCCEED_OR_RETURN(error);
  return FUTHARK_SUCCESS;
}

static int gpu_free_actual(struct futhark_context *ctx, gpu_mem mem) {
  (void)ctx;
  OPENCL_SUCCEED_OR_RETURN(clReleaseMemObject(mem));
  return FUTHARK_SUCCESS;
}

// End of backends/opencl.h
