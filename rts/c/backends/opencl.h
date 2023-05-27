// Start of backends/opencl.h

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
  const char *cache_fname;
  int num_tuning_params;
  int64_t *tuning_params;
  const char** tuning_param_names;
  const char** tuning_param_vars;
  const char** tuning_param_classes;
  // Uniform fields above.

  int preferred_device_num;
  const char *preferred_platform;
  const char *preferred_device;
  int ignore_blacklist;

  const char* dump_program_to;
  const char* load_program_from;
  const char* dump_binary_to;
  const char* load_binary_from;

  size_t default_group_size;
  size_t default_num_groups;
  size_t default_tile_size;
  size_t default_reg_tile_size;
  size_t default_threshold;

  int default_group_size_changed;
  int default_tile_size_changed;
  int num_build_opts;
  const char **build_opts;

  cl_command_queue queue;
  int queue_set;
};

static void backend_context_config_setup(struct futhark_context_config* cfg) {
  cfg->num_build_opts = 0;
  cfg->build_opts = (const char**) malloc(sizeof(const char*));
  cfg->build_opts[0] = NULL;
  cfg->preferred_device_num = 0;
  cfg->preferred_platform = "";
  cfg->preferred_device = "";
  cfg->ignore_blacklist = 0;
  cfg->dump_program_to = NULL;
  cfg->load_program_from = NULL;
  cfg->dump_binary_to = NULL;
  cfg->load_binary_from = NULL;

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
  free(cfg->build_opts);
}

void futhark_context_config_add_build_option(struct futhark_context_config* cfg, const char *opt) {
  cfg->build_opts[cfg->num_build_opts] = opt;
  cfg->num_build_opts++;
  cfg->build_opts = (const char**) realloc(cfg->build_opts, (cfg->num_build_opts+1) * sizeof(const char*));
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
  cfg->preferred_device = s;
  cfg->preferred_device_num = x;
  cfg->ignore_blacklist = 1;
}

void futhark_context_config_set_platform(struct futhark_context_config *cfg, const char *s) {
  cfg->preferred_platform = s;
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

void futhark_context_config_dump_program_to(struct futhark_context_config *cfg, const char *path) {
  cfg->dump_program_to = path;
}

void futhark_context_config_load_program_from(struct futhark_context_config *cfg, const char *path) {
  cfg->load_program_from = path;
}

void futhark_context_config_dump_binary_to(struct futhark_context_config *cfg, const char *path) {
  cfg->dump_binary_to = path;
}

void futhark_context_config_load_binary_from(struct futhark_context_config *cfg, const char *path) {
  cfg->load_binary_from = path;
}

void futhark_context_config_set_default_group_size(struct futhark_context_config *cfg, int size) {
  cfg->default_group_size = size;
  cfg->default_group_size_changed = 1;
}

void futhark_context_config_set_default_num_groups(struct futhark_context_config *cfg, int num) {
  cfg->default_num_groups = num;
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
  if (strcmp(param_name, "default_group_size") == 0) {
    cfg->default_group_size = new_value;
    return 0;
  }
  if (strcmp(param_name, "default_num_groups") == 0) {
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

// A record of something that happened.
struct profiling_record {
  cl_event *event;
  int *runs;
  int64_t *runtime;
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
  int64_t peak_mem_usage_default;
  int64_t cur_mem_usage_default;
  struct program* program;

  // Common fields above.

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

  struct free_list cl_free_list;

  size_t max_group_size;
  size_t max_num_groups;
  size_t max_tile_size;
  size_t max_threshold;
  size_t max_local_memory;

  size_t lockstep_width;

  struct profiling_record *profiling_records;
  int profiling_records_capacity;
  int profiling_records_used;

};

static cl_build_status build_opencl_program(cl_program program, cl_device_id device, const char* options) {
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

  if (build_status != CL_SUCCESS) {
    char *build_log;
    size_t ret_val_size;
    OPENCL_SUCCEED_FATAL(clGetProgramBuildInfo(program, device, CL_PROGRAM_BUILD_LOG, 0, NULL, &ret_val_size));

    build_log = (char*) malloc(ret_val_size+1);
    OPENCL_SUCCEED_FATAL(clGetProgramBuildInfo(program, device, CL_PROGRAM_BUILD_LOG, ret_val_size, build_log, NULL));

    // The spec technically does not say whether the build log is zero-terminated, so let's be careful.
    build_log[ret_val_size] = '\0';

    fprintf(stderr, "Build log:\n%s\n", build_log);

    free(build_log);
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

  for (int i = 0; extra_build_opts[i] != NULL; i++) {
    compile_opts_size += strlen(extra_build_opts[i] + 1);
  }

  char *compile_opts = (char*) malloc(compile_opts_size);

  int w = snprintf(compile_opts, compile_opts_size,
                   "-DLOCKSTEP_WIDTH=%d ",
                   (int)ctx->lockstep_width);

  w += snprintf(compile_opts+w, compile_opts_size-w,
                "-D%s=%d ",
                "max_group_size",
                (int)ctx->max_group_size);

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

  // Oclgrind claims to support cl_khr_fp16, but this is not actually
  // the case.
  if (strcmp(device_option.platform_name, "Oclgrind") == 0) {
    w += snprintf(compile_opts+w, compile_opts_size-w, "-DEMULATE_F16 ");
  }

  return compile_opts;
}

// Count up the runtime all the profiling_records that occured during execution.
// Also clears the buffer of profiling_records.
static cl_int opencl_tally_profiling_records(struct futhark_context *ctx) {
  cl_int err;
  for (int i = 0; i < ctx->profiling_records_used; i++) {
    struct profiling_record record = ctx->profiling_records[i];

    cl_ulong start_t, end_t;

    if ((err = clGetEventProfilingInfo(*record.event,
                                       CL_PROFILING_COMMAND_START,
                                       sizeof(start_t),
                                       &start_t,
                                       NULL)) != CL_SUCCESS) {
      return err;
    }

    if ((err = clGetEventProfilingInfo(*record.event,
                                       CL_PROFILING_COMMAND_END,
                                       sizeof(end_t),
                                       &end_t,
                                       NULL)) != CL_SUCCESS) {
      return err;
    }

    // OpenCL provides nanosecond resolution, but we want
    // microseconds.
    *record.runs += 1;
    *record.runtime += (end_t - start_t)/1000;

    if ((err = clReleaseEvent(*record.event)) != CL_SUCCESS) {
      return err;
    }
    free(record.event);
  }

  ctx->profiling_records_used = 0;

  return CL_SUCCESS;
}

// If profiling, produce an event associated with a profiling record.
static cl_event* opencl_get_event(struct futhark_context *ctx, int *runs, int64_t *runtime) {
  if (ctx->profiling_records_used == ctx->profiling_records_capacity) {
    ctx->profiling_records_capacity *= 2;
    ctx->profiling_records =
      realloc(ctx->profiling_records,
              ctx->profiling_records_capacity *
              sizeof(struct profiling_record));
  }
  cl_event *event = malloc(sizeof(cl_event));
  ctx->profiling_records[ctx->profiling_records_used].event = event;
  ctx->profiling_records[ctx->profiling_records_used].runs = runs;
  ctx->profiling_records[ctx->profiling_records_used].runtime = runtime;
  ctx->profiling_records_used++;
  return event;
}

// Allocate memory from driver. The problem is that OpenCL may perform
// lazy allocation, so we cannot know whether an allocation succeeded
// until the first time we try to use it.  Hence we immediately
// perform a write to see if the allocation succeeded.  This is slow,
// but the assumption is that this operation will be rare (most things
// will go through the free list).
static int opencl_alloc_actual(struct futhark_context *ctx, size_t size, cl_mem *mem_out) {
  int error;
  *mem_out = clCreateBuffer(ctx->ctx, CL_MEM_READ_WRITE, size, NULL, &error);

  if (error != CL_SUCCESS) {
    return error;
  }

  int x = 2;
  error = clEnqueueWriteBuffer(ctx->queue, *mem_out,
                               CL_TRUE,
                               0, sizeof(x), &x,
                               0, NULL, NULL);

  // No need to wait for completion here. clWaitForEvents() cannot
  // return mem object allocation failures. This implies that the
  // buffer is faulted onto the device on enqueue. (Observation by
  // Andreas Kloeckner.)

  return error;
}

static int opencl_alloc(struct futhark_context *ctx, FILE *log,
                        size_t min_size, const char *tag,
                        cl_mem *mem_out, size_t *size_out) {
  (void)tag;
  if (min_size < sizeof(int)) {
    min_size = sizeof(int);
  }

  cl_mem* memptr;
  if (free_list_find(&ctx->cl_free_list, min_size, tag, size_out, (fl_mem*)&memptr) == 0) {
    // Successfully found a free block.  Is it big enough?
    if (*size_out >= min_size) {
      if (ctx->cfg->debugging) {
        fprintf(log, "No need to allocate: Found a block in the free list.\n");
      }
      *mem_out = *memptr;
      free(memptr);
      return CL_SUCCESS;
    } else {
      if (ctx->cfg->debugging) {
        fprintf(log, "Found a free block, but it was too small.\n");
      }
      int error = clReleaseMemObject(*memptr);
      free(*memptr);
      if (error != CL_SUCCESS) {
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

  int error = opencl_alloc_actual(ctx, min_size, mem_out);

  while (error == CL_MEM_OBJECT_ALLOCATION_FAILURE) {
    if (ctx->cfg->debugging) {
      fprintf(log, "Out of OpenCL memory: releasing entry from the free list...\n");
    }
    cl_mem* memptr;
    if (free_list_first(&ctx->cl_free_list, (fl_mem*)&memptr) == 0) {
      cl_mem mem = *memptr;
      free(memptr);
      error = clReleaseMemObject(mem);
      if (error != CL_SUCCESS) {
        return error;
      }
    } else {
      break;
    }
    error = opencl_alloc_actual(ctx, min_size, mem_out);
  }

  return error;
}

static int opencl_free(struct futhark_context *ctx,
                       cl_mem mem, size_t size, const char *tag) {
  cl_mem* memptr = malloc(sizeof(cl_mem));
  *memptr = mem;
  free_list_insert(&ctx->cl_free_list, size, (fl_mem)memptr, tag);
  return CL_SUCCESS;
}

static int opencl_free_all(struct futhark_context *ctx) {
  free_list_pack(&ctx->cl_free_list);
  cl_mem* memptr;
  while (free_list_first(&ctx->cl_free_list, (fl_mem*)&memptr) == 0) {
    cl_mem mem = *memptr;
    free(memptr);
    int error = clReleaseMemObject(mem);
    if (error != CL_SUCCESS) {
      return error;
    }
  }

  return CL_SUCCESS;
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
                                            const char *srcs[],
                                            const char *extra_build_opts[],
                                            const char* cache_fname) {
  int error;

  free_list_init(&ctx->cl_free_list);
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

  size_t max_group_size;
  OPENCL_SUCCEED_FATAL(clGetDeviceInfo(device_option.device, CL_DEVICE_MAX_WORK_GROUP_SIZE,
                                       sizeof(size_t), &max_group_size, NULL));

  size_t max_tile_size = sqrt(max_group_size);

  cl_ulong max_local_memory;
  OPENCL_SUCCEED_FATAL(clGetDeviceInfo(device_option.device, CL_DEVICE_LOCAL_MEM_SIZE,
                                       sizeof(size_t), &max_local_memory, NULL));

  // Futhark reserves 4 bytes for bookkeeping information.
  max_local_memory -= 4;

  // The OpenCL implementation may reserve some local memory bytes for
  // various purposes.  In principle, we should use
  // clGetKernelWorkGroupInfo() to figure out for each kernel how much
  // is actually available, but our current code generator design
  // makes this infeasible.  Instead, we have this nasty hack where we
  // arbitrarily subtract some bytes, based on empirical measurements
  // (but which might be arbitrarily wrong).  Fortunately, we rarely
  // try to really push the local memory usage.
  if (strstr(device_option.platform_name, "NVIDIA CUDA") != NULL) {
    max_local_memory -= 12;
  } else if (strstr(device_option.platform_name, "AMD") != NULL) {
    max_local_memory -= 16;
  }

  // Make sure this function is defined.
  post_opencl_setup(ctx, &device_option);

  if (max_group_size < ctx->cfg->default_group_size) {
    if (ctx->cfg->default_group_size_changed) {
      fprintf(stderr, "Note: Device limits default group size to %zu (down from %zu).\n",
              max_group_size, ctx->cfg->default_group_size);
    }
    ctx->cfg->default_group_size = max_group_size;
  }

  if (max_tile_size < ctx->cfg->default_tile_size) {
    if (ctx->cfg->default_tile_size_changed) {
      fprintf(stderr, "Note: Device limits default tile size to %zu (down from %zu).\n",
              max_tile_size, ctx->cfg->default_tile_size);
    }
    ctx->cfg->default_tile_size = max_tile_size;
  }

  ctx->max_group_size = max_group_size;
  ctx->max_tile_size = max_tile_size; // No limit.
  ctx->max_threshold = ctx->max_num_groups = 0; // No limit.
  ctx->max_local_memory = max_local_memory;

  // Now we go through all the sizes, clamp them to the valid range,
  // or set them to the default.
  for (int i = 0; i < ctx->cfg->num_tuning_params; i++) {
    const char *size_class = ctx->cfg->tuning_param_classes[i];
    int64_t *size_value = &ctx->cfg->tuning_params[i];
    const char* size_name = ctx->cfg->tuning_param_names[i];
    int64_t max_value = 0, default_value = 0;

    if (strstr(size_class, "group_size") == size_class) {
      max_value = max_group_size;
      default_value = ctx->cfg->default_group_size;
    } else if (strstr(size_class, "num_groups") == size_class) {
      max_value = max_group_size; // Futhark assumes this constraint.
      default_value = ctx->cfg->default_num_groups;
      // XXX: as a quick and dirty hack, use twice as many threads for
      // histograms by default.  We really should just be smarter
      // about sizes somehow.
      if (strstr(size_name, ".seghist_") != NULL) {
        default_value *= 2;
      }
    } else if (strstr(size_class, "tile_size") == size_class) {
      max_value = sqrt(max_group_size);
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
    fprintf(stderr, "Default group size: %d\n", (int)ctx->cfg->default_group_size);
    fprintf(stderr, "Default number of groups: %d\n", (int)ctx->cfg->default_num_groups);
  }

  char *compile_opts = mk_compile_opts(ctx, extra_build_opts, device_option);

  if (ctx->cfg->logging) {
    fprintf(stderr, "OpenCL compiler options: %s\n", compile_opts);
  }

  char *fut_opencl_src = NULL;
  cl_program prog;
  error = CL_SUCCESS;

  struct cache_hash h;

  int loaded_from_cache = 0;
  if (ctx->cfg->load_binary_from == NULL) {
    size_t src_size = 0;

    // Maybe we have to read OpenCL source from somewhere else (used for debugging).
    if (ctx->cfg->load_program_from != NULL) {
      fut_opencl_src = slurp_file(ctx->cfg->load_program_from, NULL);
      assert(fut_opencl_src != NULL);
    } else {
      // Construct the OpenCL source concatenating all the fragments.
      for (const char **src = srcs; src && *src; src++) {
        src_size += strlen(*src);
      }

      fut_opencl_src = (char*) malloc(src_size + 1);

      size_t n, i;
      for (i = 0, n = 0; srcs && srcs[i]; i++) {
        strncpy(fut_opencl_src+n, srcs[i], src_size-n);
        n += strlen(srcs[i]);
      }
      fut_opencl_src[src_size] = 0;
    }

    if (ctx->cfg->dump_program_to != NULL) {
      if (ctx->cfg->logging) {
        fprintf(stderr, "Dumping OpenCL source to %s...\n", ctx->cfg->dump_program_to);
      }

      dump_file(ctx->cfg->dump_program_to, fut_opencl_src, strlen(fut_opencl_src));
    }

    if (cache_fname != NULL) {
      if (ctx->cfg->logging) {
        fprintf(stderr, "Restoring cache from from %s...\n", cache_fname);
      }
      cache_hash_init(&h);
      cache_hash(&h, fut_opencl_src, strlen(fut_opencl_src));
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

      const char* src_ptr[] = {fut_opencl_src};
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
  OPENCL_SUCCEED_FATAL(build_opencl_program(prog, device_option.device, compile_opts));

  free(compile_opts);
  free(fut_opencl_src);

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

static struct opencl_device_option get_preferred_device(const struct futhark_context_config *cfg) {
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

  futhark_panic(1, "Could not find acceptable OpenCL device.\n");
  exit(1); // Never reached
}

static void setup_opencl(struct futhark_context *ctx,
                         const char *srcs[],
                         const char *extra_build_opts[],
                         const char* cache_fname) {
  struct opencl_device_option device_option = get_preferred_device(ctx->cfg);

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

  setup_opencl_with_command_queue(ctx, queue, srcs, extra_build_opts, cache_fname);
}

int backend_context_setup(struct futhark_context* ctx) {
  ctx->lockstep_width = 0; // Real value set later.
  ctx->profiling_records_capacity = 200;
  ctx->profiling_records_used = 0;
  ctx->profiling_records =
    malloc(ctx->profiling_records_capacity *
           sizeof(struct profiling_record));
  ctx->failure_is_an_option = 0;
  ctx->total_runs = 0;
  ctx->total_runtime = 0;
  ctx->peak_mem_usage_device = 0;
  ctx->cur_mem_usage_device = 0;

  if (ctx->cfg->queue_set) {
    setup_opencl_with_command_queue(ctx, ctx->cfg->queue, opencl_program, ctx->cfg->build_opts, ctx->cfg->cache_fname);
  } else {
    setup_opencl(ctx, opencl_program, ctx->cfg->build_opts, ctx->cfg->cache_fname);
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
  return 0;
}

void backend_context_teardown(struct futhark_context* ctx) {
  OPENCL_SUCCEED_FATAL(clReleaseMemObject(ctx->global_failure));
  OPENCL_SUCCEED_FATAL(clReleaseMemObject(ctx->global_failure_args));
  (void)opencl_tally_profiling_records(ctx);
  free(ctx->profiling_records);
  (void)opencl_free_all(ctx);
  (void)clReleaseProgram(ctx->clprogram);
  (void)clReleaseCommandQueue(ctx->queue);
  (void)clReleaseContext(ctx->ctx);
}

cl_command_queue futhark_context_get_command_queue(struct futhark_context* ctx) {
  return ctx->queue;
}

// End of backends/opencl.h
