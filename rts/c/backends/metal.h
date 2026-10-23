// Start of backends/metal.h.
//
// The Metal runtime system is written in Objective-C without ARC.
// The rest of the generated code is plain C, so device memory
// ('gpu_mem') is a void pointer that holds an id<MTLBuffer>.
//
// Execution model: all work goes into a single serial compute
// encoder, which is committed only when the host needs results
// (scalar reads, copies to the host, synchronisation).  Dispatches in
// a serial encoder run in order, so no barriers are needed between
// kernels.  Device-to-device copies and scalar writes are tiny
// kernels, so they do not split the encoder either.
//
// Kernel arguments: every kernel takes its arguments as a struct in
// buffer 0 (see prelude.metal), with device memory passed as GPU
// addresses.  The struct layout is obtained by pipeline reflection.
// Since kernels access buffers only through their GPU addresses, all
// buffers are made resident through a residency set, and command
// buffers are chained through an event, as untracked buffers would
// otherwise let them overlap.  Some devices, such as the paravirtual
// GPU of a virtual machine, do not support residency sets; there the
// buffers passed to each kernel are made resident with useResource
// instead.

#import <Metal/Metal.h>

static void set_tuning_params(struct futhark_context* ctx);
static char* get_failure_msg(int failure_idx, int64_t args[]);

#define METAL_SUCCEED_FATAL(ok, what)                                   \
  if (!(ok)) {                                                          \
    futhark_panic(-1, "%s:%d: Metal call failed: %s\n", __FILE__, __LINE__, what); \
  }

struct futhark_context_config {
  int in_use;
  int debugging;
  int profiling;
  int logging;
  char *cache_fname;
  struct tuning_param tuning_params[NUM_TUNING_PARAMS];
  // Uniform fields above.

  char* program;
  int num_build_opts;
  char* *build_opts;

  char* preferred_device;
  int preferred_device_num;

  id<MTLCommandQueue> queue;

  int unified_memory;

  struct gpu_config gpu;
};

static void backend_context_config_setup(struct futhark_context_config* cfg) {
  cfg->num_build_opts = 0;
  cfg->build_opts = (char**) malloc(sizeof(char*));
  cfg->build_opts[0] = NULL;

  cfg->program = strconcat(gpu_program);

  cfg->preferred_device_num = 0;
  cfg->preferred_device = strdup("");

  cfg->queue = nil;

  // All Apple GPUs have unified memory.
  cfg->unified_memory = 1;

  cfg->gpu = gpu_config_initial;
  cfg->gpu.default_block_size = 256;
  cfg->gpu.default_tile_size = 16;
  cfg->gpu.default_reg_tile_size = 2;
  cfg->gpu.default_threshold = 32*1024;
}

static void backend_context_config_teardown(struct futhark_context_config* cfg) {
  for (int i = 0; i < cfg->num_build_opts; i++) {
    free(cfg->build_opts[i]);
  }
  free(cfg->build_opts);
  free(cfg->preferred_device);
  free(cfg->program);
  [cfg->queue release];
}

void futhark_context_config_add_build_option(struct futhark_context_config* cfg, const char *opt) {
  cfg->build_opts[cfg->num_build_opts] = strdup(opt);
  cfg->num_build_opts++;
  cfg->build_opts = (char**) realloc(cfg->build_opts, (cfg->num_build_opts + 1) * sizeof(char*));
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
}

void futhark_context_config_list_devices(struct futhark_context_config *cfg) {
  (void)cfg;
  @autoreleasepool {
    NSArray<id<MTLDevice>> *devices = MTLCopyAllDevices();
    for (NSUInteger i = 0; i < [devices count]; i++) {
      printf("Device #%d: %s\n", (int)i, [[devices[i] name] UTF8String]);
    }
    [devices release];
  }
}

const char* futhark_context_config_get_program(struct futhark_context_config *cfg) {
  return cfg->program;
}

void futhark_context_config_set_program(struct futhark_context_config *cfg, const char *s) {
  free(cfg->program);
  cfg->program = strdup(s);
}

void futhark_context_config_set_command_queue(struct futhark_context_config *cfg, void* queue) {
  [cfg->queue release];
  cfg->queue = [(id<MTLCommandQueue>)queue retain];
}

void futhark_context_config_set_unified_memory(struct futhark_context_config* cfg, int flag) {
  // Ignored: all Apple GPUs have unified memory.
  cfg->unified_memory = flag;
}

// A kernel and the layout of its argument struct.  The pipeline is
// created when the kernel is first launched, as creating all of them
// up front is slow for large programs.
struct metal_kernel {
  const char *name;
  id<MTLComputePipelineState> pipeline;
  size_t args_size;
  int num_args;
  size_t *arg_offsets;
  bool *arg_is_pointer;
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

  void* global_failure;
  void* global_failure_args;
  // True if a potentially failing kernel has been enqueued.
  int32_t failure_is_an_option;
  int total_runs;
  long int total_runtime;
  int64_t peak_mem_usage_device;
  int64_t cur_mem_usage_device;

  id<MTLDevice> device;
  id<MTLCommandQueue> queue;
  id<MTLLibrary> library;
  // Nil if the device does not support residency sets.
  id<MTLResidencySet> residency;
  // The command buffer and encoder currently being filled, if any.
  id<MTLCommandBuffer> cb;
  id<MTLComputeCommandEncoder> encoder;
  // Signalled by every command buffer, and waited for by the next.
  id<MTLEvent> timeline;
  uint64_t timeline_value;
  struct metal_kernel* copy_kernel;
  struct metal_kernel* write_kernel;

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

// GPU ABSTRACTION LAYER

// Types.

typedef struct metal_kernel* gpu_kernel;
typedef void* gpu_mem;

static id<MTLBuffer> metal_buffer(gpu_mem mem) {
  return (id<MTLBuffer>)mem;
}

// Command submission.

static id<MTLComputeCommandEncoder> metal_encoder(struct futhark_context *ctx) {
  if (ctx->encoder == nil) {
    @autoreleasepool {
      ctx->cb = [[ctx->queue commandBufferWithUnretainedReferences] retain];
      [ctx->cb encodeWaitForEvent:ctx->timeline value:ctx->timeline_value];
      ctx->encoder =
        [[ctx->cb computeCommandEncoderWithDispatchType:MTLDispatchTypeSerial] retain];
    }
  }
  return ctx->encoder;
}

// Make a buffer resident for the current encoder, if it is not
// already resident through the residency set.
static void metal_use_buffer(struct futhark_context *ctx, id<MTLBuffer> b) {
  if (ctx->residency == nil && b != nil) {
    [metal_encoder(ctx) useResource:b
                              usage:MTLResourceUsageRead | MTLResourceUsageWrite];
  }
}

// Commit the current command buffer, if any, and return it (retained),
// or nil.
static id<MTLCommandBuffer> metal_commit(struct futhark_context *ctx) {
  if (ctx->encoder == nil) {
    return nil;
  }
  [ctx->encoder endEncoding];
  [ctx->encoder release];
  ctx->encoder = nil;
  id<MTLCommandBuffer> cb = ctx->cb;
  ctx->cb = nil;
  [cb encodeSignalEvent:ctx->timeline value:++ctx->timeline_value];
  [cb commit];
  return cb;
}

// Wait for a committed command buffer and release it.
static int metal_wait(struct futhark_context *ctx, id<MTLCommandBuffer> cb) {
  int ret = FUTHARK_SUCCESS;
  [cb waitUntilCompleted];
  if ([cb status] == MTLCommandBufferStatusError) {
    @autoreleasepool {
      set_error(ctx, msgprintf("Metal command buffer failed: %s\n",
                               [[[cb error] localizedDescription] UTF8String]));
    }
    ret = 1;
  }
  [cb release];
  return ret;
}

// Commit all enqueued work and wait for it to finish.
static int metal_finish(struct futhark_context *ctx) {
  id<MTLCommandBuffer> cb = metal_commit(ctx);
  if (cb != nil) {
    return metal_wait(ctx, cb);
  }
  return FUTHARK_SUCCESS;
}

// Profiling: a profiled operation gets its own command buffer, whose
// GPU start and end times are the duration of the operation.
struct metal_event {
  id<MTLCommandBuffer> cb;
};

static struct metal_event* metal_event_new(struct futhark_context* ctx) {
  if (ctx->profiling && !ctx->profiling_paused) {
    // Isolate the operation in its own command buffer.
    [metal_commit(ctx) release];
    struct metal_event* e = malloc(sizeof(struct metal_event));
    e->cb = nil;
    return e;
  } else {
    return NULL;
  }
}

static void metal_event_end(struct futhark_context* ctx, struct metal_event* e) {
  if (e != NULL) {
    e->cb = metal_commit(ctx);
  }
}

static int metal_event_report(struct str_builder* sb, struct metal_event* e) {
  double us = 0;
  if (e->cb != nil) {
    [e->cb waitUntilCompleted];
    us = ([e->cb GPUEndTime] - [e->cb GPUStartTime]) * 1e6;
    [e->cb release];
  }
  str_builder(sb, ",\"duration\":%f", us);
  free(e);
  return 0;
}

int futhark_context_sync(struct futhark_context* ctx) {
  if (metal_finish(ctx) != FUTHARK_SUCCESS) {
    return 1;
  }
  if (ctx->profiling) {
    // Profiled operations are committed in their own command buffers,
    // which are executed in order, so waiting for an empty one waits
    // for all of them.
    @autoreleasepool {
      id<MTLCommandBuffer> cb = [ctx->queue commandBuffer];
      [cb commit];
      [cb waitUntilCompleted];
    }
  }
  if (ctx->failure_is_an_option) {
    // Check for any delayed error.
    int32_t *failure_idx = [metal_buffer(ctx->global_failure) contents];
    ctx->failure_is_an_option = 0;

    if (*failure_idx >= 0) {
      int64_t args[max_failure_args+1];
      memcpy(args, [metal_buffer(ctx->global_failure_args) contents], sizeof(args));
      ctx->error = get_failure_msg(*failure_idx, args);
      // We have to clear global_failure so that the next entry point
      // is not considered a failure from the start.
      *failure_idx = -1;
      return FUTHARK_PROGRAM_ERROR;
    }
  }
  return 0;
}

// Kernels.

static struct metal_kernel* metal_kernel_new(const char *name) {
  struct metal_kernel *k = calloc(1, sizeof(struct metal_kernel));
  k->name = name;
  k->pipeline = nil;
  return k;
}

static void metal_kernel_free(struct metal_kernel *k) {
  [k->pipeline release];
  free(k->arg_offsets);
  free(k->arg_is_pointer);
  free(k);
}

// Create the pipeline for a kernel and find the layout of its
// argument struct.
static int metal_kernel_prepare(struct futhark_context *ctx, id<MTLLibrary> library,
                                struct metal_kernel *k) {
  if (k->pipeline != nil) {
    return FUTHARK_SUCCESS;
  }
  int64_t t_start = get_wall_time();
  @autoreleasepool {
    id<MTLFunction> f = [[library newFunctionWithName:@(k->name)] autorelease];
    if (f == nil) {
      set_error(ctx, msgprintf("Metal library contains no kernel %s.\n", k->name));
      return 1;
    }
    NSError *err = nil;
    MTLComputePipelineReflection *reflection = nil;
    k->pipeline =
      [ctx->device newComputePipelineStateWithFunction:f
                                               options:MTLPipelineOptionBindingInfo |
                                                       MTLPipelineOptionBufferTypeInfo
                                            reflection:&reflection
                                                 error:&err];
    if (k->pipeline == nil) {
      set_error(ctx, msgprintf("Cannot create Metal pipeline for kernel %s:\n%s\n",
                               k->name, [[err localizedDescription] UTF8String]));
      return 1;
    }
    // The argument struct is the buffer in slot 0.  It is absent if
    // the kernel has no arguments.
    k->args_size = 0;
    k->num_args = 0;
    for (id<MTLBinding> b in [reflection bindings]) {
      if ([b type] == MTLBindingTypeBuffer && [b index] == 0) {
        id<MTLBufferBinding> bb = (id<MTLBufferBinding>)b;
        NSArray<MTLStructMember*> *members = [[bb bufferStructType] members];
        k->args_size = [bb bufferDataSize];
        k->num_args = (int)[members count];
        k->arg_offsets = malloc(sizeof(size_t) * k->num_args);
        k->arg_is_pointer = malloc(sizeof(bool) * k->num_args);
        for (int i = 0; i < k->num_args; i++) {
          k->arg_offsets[i] = [members[i] offset];
          k->arg_is_pointer[i] = [members[i] dataType] == MTLDataTypePointer;
        }
      }
    }
  }
  if (ctx->debugging) {
    fprintf(ctx->log, "Created Metal pipeline for %s in %.3fms.\n",
            k->name, (get_wall_time() - t_start) / 1000.0);
  }
  return FUTHARK_SUCCESS;
}

static void gpu_create_kernel(struct futhark_context *ctx,
                              gpu_kernel* kernel,
                              const char* name) {
  (void)ctx;
  *kernel = metal_kernel_new(name);
}

static void gpu_free_kernel(struct futhark_context *ctx,
                            gpu_kernel kernel) {
  (void)ctx;
  metal_kernel_free(kernel);
}

static void metal_dispatch(struct futhark_context *ctx, struct metal_kernel *k,
                           const void *args, size_t args_size,
                           const int32_t grid[3], const int32_t block[3],
                           unsigned int shared_mem_bytes) {
  id<MTLComputeCommandEncoder> encoder = metal_encoder(ctx);
  [encoder setComputePipelineState:k->pipeline];
  if (args_size > 0) {
    [encoder setBytes:args length:args_size atIndex:0];
  }
  // Must be a multiple of 16.
  [encoder setThreadgroupMemoryLength:(shared_mem_bytes + 15) / 16 * 16 atIndex:0];
  [encoder dispatchThreadgroups:MTLSizeMake(grid[0], grid[1], grid[2])
          threadsPerThreadgroup:MTLSizeMake(block[0], block[1], block[2])];
}

static int gpu_launch_kernel(struct futhark_context* ctx,
                             gpu_kernel kernel,
                             const char *name,
                             const char *provenance,
                             const int32_t grid[3],
                             const int32_t block[3],
                             unsigned int shared_mem_bytes,
                             int num_args,
                             void* args[num_args],
                             size_t args_sizes[num_args]) {
  if (metal_kernel_prepare(ctx, ctx->library, kernel) != FUTHARK_SUCCESS) {
    return 1;
  }

  NSUInteger shared_mem_total =
    shared_mem_bytes + [kernel->pipeline staticThreadgroupMemoryLength];
  if (shared_mem_total > [ctx->device maxThreadgroupMemoryLength]) {
    set_error(ctx, msgprintf("Kernel %s with %d bytes of memory exceeds device limit of %d\n",
                             name, (int)shared_mem_total,
                             (int)[ctx->device maxThreadgroupMemoryLength]));
    return 1;
  }

  NSUInteger block_size = block[0] * block[1] * block[2];
  if (block_size > [kernel->pipeline maxTotalThreadsPerThreadgroup]) {
    set_error(ctx, msgprintf("Kernel %s with thread block size %d exceeds its limit of %d\n",
                             name, (int)block_size,
                             (int)[kernel->pipeline maxTotalThreadsPerThreadgroup]));
    return 1;
  }

  if (num_args != kernel->num_args) {
    set_error(ctx, msgprintf("Kernel %s passed %d arguments, but takes %d.\n",
                             name, num_args, kernel->num_args));
    return 1;
  }

  int64_t time_start = 0, time_end = 0;
  if (ctx->debugging) {
    time_start = get_wall_time();
  }

  struct metal_event* event = metal_event_new(ctx);
  if (event != NULL) {
    struct kvs *kvs = kvs_new();
    kvs_printf(kvs, "kernel", "\"%s\"", name);
    kvs_printf(kvs, "grid", "[%d,%d,%d]", grid[0], grid[1], grid[2]);
    kvs_printf(kvs, "block", "[%d,%d,%d]", block[0], block[1], block[2]);
    kvs_printf(kvs, "shared memory", "%d", shared_mem_bytes);
    add_event(ctx, name, provenance, kvs, event,
              (event_report_fn)metal_event_report);
  }

  // The +1 is to avoid a zero-length array.
  unsigned char args_struct[kernel->args_size+1];
  for (int i = 0; i < num_args; i++) {
    if (kernel->arg_is_pointer[i]) {
      id<MTLBuffer> b = metal_buffer(*(gpu_mem*)args[i]);
      uint64_t addr = b == nil ? 0 : [b gpuAddress];
      metal_use_buffer(ctx, b);
      memcpy(args_struct + kernel->arg_offsets[i], &addr, sizeof(addr));
    } else {
      memcpy(args_struct + kernel->arg_offsets[i], args[i], args_sizes[i]);
    }
  }
  metal_dispatch(ctx, kernel, args_struct, kernel->args_size,
                 grid, block, shared_mem_bytes);

  metal_event_end(ctx, event);

  if (ctx->debugging) {
    if (metal_finish(ctx) != FUTHARK_SUCCESS) {
      return 1;
    }
    time_end = get_wall_time();
    long int time_diff = time_end - time_start;
    fprintf(ctx->log, "  runtime: %ldus\n", time_diff);
  }
  if (ctx->logging) {
    fprintf(ctx->log, "\n");
  }

  return FUTHARK_SUCCESS;
}

// Copies and scalar writes are done with these kernels, such that
// they can go in the current compute encoder.
static const char *metal_helpers_program =
  "#include <metal_stdlib>\n"
  "using namespace metal;\n"
  "struct futhark_copy_args { device uchar *dst; device uchar *src; ulong n; };\n"
  "kernel void futhark_copy(constant futhark_copy_args *a [[buffer(0)]],\n"
  "                         uint i [[thread_position_in_grid]]) {\n"
  "  ulong n = a->n;\n"
  "  if (((ulong)a->dst | (ulong)a->src | n) % 16 == 0) {\n"
  "    if (i < n/16) { ((device uint4*)a->dst)[i] = ((device uint4*)a->src)[i]; }\n"
  "  } else if (((ulong)a->dst | (ulong)a->src | n) % 4 == 0) {\n"
  "    if (i < n/4) { ((device uint*)a->dst)[i] = ((device uint*)a->src)[i]; }\n"
  "  } else {\n"
  "    if (i < n) { a->dst[i] = a->src[i]; }\n"
  "  }\n"
  "}\n"
  "struct futhark_write_args { device uchar *dst; ulong n; uchar bytes[8]; };\n"
  "kernel void futhark_write(constant futhark_write_args *a [[buffer(0)]],\n"
  "                          uint i [[thread_position_in_grid]]) {\n"
  "  if (i < a->n) { a->dst[i] = a->bytes[i]; }\n"
  "}\n";

static int gpu_memcpy(struct futhark_context* ctx,
                      const char *provenance,
                      gpu_mem dst, int64_t dst_offset,
                      gpu_mem src, int64_t src_offset,
                      int64_t nbytes) {
  if (nbytes > 0) {
    struct metal_event* event = metal_event_new(ctx);
    if (event != NULL) {
      add_event(ctx, "copy_dev_to_dev", provenance, NULL, event,
                (event_report_fn)metal_event_report);
    }
    struct { uint64_t dst, src, n; } args =
      { [metal_buffer(dst) gpuAddress] + dst_offset,
        [metal_buffer(src) gpuAddress] + src_offset,
        (uint64_t)nbytes };
    metal_use_buffer(ctx, metal_buffer(dst));
    metal_use_buffer(ctx, metal_buffer(src));
    // Must match the choice made by the kernel.
    int64_t bytes_per_thread =
      (args.dst | args.src | args.n) % 16 == 0 ? 16
      : (args.dst | args.src | args.n) % 4 == 0 ? 4
      : 1;
    int64_t threads = nbytes / bytes_per_thread;
    const int32_t block[3] = {256, 1, 1};
    const int32_t grid[3] = {(int32_t)((threads + 255) / 256), 1, 1};
    metal_dispatch(ctx, ctx->copy_kernel, &args, sizeof(args), grid, block, 0);
    metal_event_end(ctx, event);
  }
  return FUTHARK_SUCCESS;
}

static int gpu_scalar_to_device(struct futhark_context* ctx,
                                const char *provenance,
                                gpu_mem dst, size_t offset, size_t size,
                                void *src) {
  assert(size <= 8);
  struct metal_event* event = metal_event_new(ctx);
  if (event != NULL) {
    add_event(ctx, "copy_scalar_to_dev", provenance, NULL, event,
              (event_report_fn)metal_event_report);
  }
  struct { uint64_t dst, n; unsigned char bytes[8]; } args;
  args.dst = [metal_buffer(dst) gpuAddress] + offset;
  args.n = size;
  memcpy(args.bytes, src, size);
  metal_use_buffer(ctx, metal_buffer(dst));
  const int32_t block[3] = {8, 1, 1};
  const int32_t grid[3] = {1, 1, 1};
  metal_dispatch(ctx, ctx->write_kernel, &args, sizeof(args), grid, block, 0);
  metal_event_end(ctx, event);
  return FUTHARK_SUCCESS;
}

// Memory is shared with the host, so the remaining copies are just
// memcpy() after waiting for the enqueued work.

static int gpu_scalar_from_device(struct futhark_context* ctx,
                                  const char *provenance,
                                  void *dst,
                                  gpu_mem src, size_t offset, size_t size) {
  (void)provenance;
  if (metal_finish(ctx) != FUTHARK_SUCCESS) {
    return 1;
  }
  memcpy(dst, (unsigned char*)[metal_buffer(src) contents] + offset, size);
  return FUTHARK_SUCCESS;
}

static int memcpy_host2gpu(struct futhark_context* ctx,
                           const char *provenance,
                           bool sync,
                           gpu_mem dst, int64_t dst_offset,
                           const unsigned char* src, int64_t src_offset,
                           int64_t nbytes) {
  (void)provenance; (void)sync;
  if (nbytes > 0) {
    // Enqueued work may still use the destination.
    if (metal_finish(ctx) != FUTHARK_SUCCESS) {
      return 1;
    }
    memcpy((unsigned char*)[metal_buffer(dst) contents] + dst_offset,
           src + src_offset, nbytes);
  }
  return FUTHARK_SUCCESS;
}

static int memcpy_gpu2host(struct futhark_context* ctx,
                           const char *provenance,
                           bool sync,
                           unsigned char* dst, int64_t dst_offset,
                           gpu_mem src, int64_t src_offset,
                           int64_t nbytes) {
  (void)provenance;
  if (nbytes > 0) {
    if (metal_finish(ctx) != FUTHARK_SUCCESS) {
      return 1;
    }
    memcpy(dst + dst_offset,
           (unsigned char*)[metal_buffer(src) contents] + src_offset, nbytes);
    if (sync &&
        ctx->failure_is_an_option &&
        futhark_context_sync(ctx) != 0) {
      return 1;
    }
  }
  return FUTHARK_SUCCESS;
}

static int gpu_alloc_actual(struct futhark_context *ctx, size_t size, gpu_mem *mem_out) {
  id<MTLBuffer> b =
    [ctx->device newBufferWithLength:size
                             options:MTLResourceStorageModeShared |
                                     MTLResourceHazardTrackingModeUntracked];
  if (b == nil) {
    return FUTHARK_OUT_OF_MEMORY;
  }
  // The commit applies to command buffers committed after it, which
  // includes the one currently being encoded.  Messages to a nil
  // residency set do nothing.
  [ctx->residency addAllocation:b];
  [ctx->residency commit];
  *mem_out = b;
  return FUTHARK_SUCCESS;
}

static int gpu_free_actual(struct futhark_context *ctx, gpu_mem mem) {
  // Enqueued work may still use the buffer.
  if (metal_finish(ctx) != FUTHARK_SUCCESS) {
    return 1;
  }
  [ctx->residency removeAllocation:metal_buffer(mem)];
  [ctx->residency commit];
  [metal_buffer(mem) release];
  return FUTHARK_SUCCESS;
}

// Initialisation.

static id<MTLDevice> metal_select_device(struct futhark_context_config *cfg) {
  if (cfg->queue != nil) {
    return [[cfg->queue device] retain];
  }
  if (*cfg->preferred_device == 0 && cfg->preferred_device_num == 0) {
    return MTLCreateSystemDefaultDevice();
  }
  id<MTLDevice> device = nil;
  @autoreleasepool {
    NSArray<id<MTLDevice>> *devices = MTLCopyAllDevices();
    int num_matching = 0;
    for (id<MTLDevice> d in devices) {
      if (strstr([[d name] UTF8String], cfg->preferred_device) != NULL &&
          num_matching++ == cfg->preferred_device_num) {
        device = [d retain];
        break;
      }
    }
    [devices release];
  }
  return device;
}

static void metal_size_setup(struct futhark_context *ctx) {
  struct futhark_context_config *cfg = ctx->cfg;

  ctx->max_thread_block_size = [ctx->device maxThreadsPerThreadgroup].width;
  ctx->max_grid_size = 1U<<31;
  ctx->max_tile_size = sqrt(ctx->max_thread_block_size);
  ctx->max_threshold = 1U<<31; // No limit.
  ctx->max_bespoke = 1U<<31; // No limit.

  if (cfg->gpu.default_registers != 0) {
    ctx->max_registers = cfg->gpu.default_registers;
  } else {
    ctx->max_registers = 1<<16;
  }

  if (cfg->gpu.default_shared_memory != 0) {
    ctx->max_shared_memory = cfg->gpu.default_shared_memory;
  } else {
    // Leave room for the statically allocated threadgroup memory
    // used for error handling.
    ctx->max_shared_memory = [ctx->device maxThreadgroupMemoryLength] - 16;
  }

  if (cfg->gpu.default_cache != 0) {
    ctx->max_cache = cfg->gpu.default_cache;
  } else {
    ctx->max_cache = 8*1024*1024;
  }

  // Threads within a SIMD group are not guaranteed to execute in
  // lockstep.
  ctx->lockstep_width = 1;

  if (cfg->gpu.default_block_size > ctx->max_thread_block_size) {
    if (cfg->gpu.default_block_size_changed) {
      fprintf(stderr,
              "Note: Device limits default block size to %zu (down from %zu).\n",
              ctx->max_thread_block_size, cfg->gpu.default_block_size);
    }
    cfg->gpu.default_block_size = ctx->max_thread_block_size;
  }
  if (cfg->gpu.default_tile_size > ctx->max_tile_size) {
    if (cfg->gpu.default_tile_size_changed) {
      fprintf(stderr,
              "Note: Device limits default tile size to %zu (down from %zu).\n",
              ctx->max_tile_size, cfg->gpu.default_tile_size);
    }
    cfg->gpu.default_tile_size = ctx->max_tile_size;
  }
  if (!cfg->gpu.default_grid_size_changed) {
    cfg->gpu.default_grid_size = 256;
  }

  for (int i = 0; i < NUM_TUNING_PARAMS; i++) {
    const char *size_class = cfg->tuning_params[i].class;
    int64_t *size_value = &cfg->tuning_params[i].val;
    const char* size_name = cfg->tuning_params[i].name;
    int64_t max_value = 0, default_value = 0;

    if (strstr(size_class, "thread_block_size") == size_class) {
      max_value = ctx->max_thread_block_size;
      default_value = cfg->gpu.default_block_size;
    } else if (strstr(size_class, "grid_size") == size_class) {
      max_value = ctx->max_grid_size;
      default_value = cfg->gpu.default_grid_size;
      // XXX: as a quick and dirty hack, use twice as many threads for
      // histograms by default.  We really should just be smarter
      // about sizes somehow.
      if (strstr(size_name, ".seghist_") != NULL) {
        default_value *= 2;
      }
    } else if (strstr(size_class, "tile_size") == size_class) {
      max_value = ctx->max_tile_size;
      default_value = cfg->gpu.default_tile_size;
    } else if (strstr(size_class, "reg_tile_size") == size_class) {
      max_value = 0; // No limit.
      default_value = cfg->gpu.default_reg_tile_size;
    } else if (strstr(size_class, "shared_memory") == size_class) {
      max_value = ctx->max_shared_memory;
      default_value = ctx->max_shared_memory;
    } else if (strstr(size_class, "cache") == size_class) {
      max_value = ctx->max_cache;
      default_value = ctx->max_cache;
    } else if (strstr(size_class, "threshold") == size_class) {
      // Threshold can be as large as it takes.
      default_value = cfg->gpu.default_threshold;
    } else {
      // Bespoke sizes have no limit or default.
    }

    if (*size_value == 0) {
      *size_value = default_value;
    } else if (max_value > 0 && *size_value > max_value) {
      fprintf(stderr, "Note: Device limits %s to %zu (down from %zu)\n",
              size_name, (size_t)max_value, (size_t)*size_value);
      *size_value = max_value;
    }
  }
}

// The macros that parameterise the program.
static NSDictionary* metal_macros(struct futhark_context *ctx) {
  NSMutableDictionary *m = [NSMutableDictionary dictionary];

  char** macro_names;
  int64_t* macro_vals;
  int num_macros = gpu_macros(ctx, &macro_names, &macro_vals);
  for (int i = 0; i < num_macros; i++) {
    m[@(macro_names[i])] = @(macro_vals[i]);
  }
  free(macro_names);
  free(macro_vals);

  for (int i = 0; i < NUM_TUNING_PARAMS; i++) {
    char name[1024];
    snprintf(name, sizeof(name), "set_%s", ctx->cfg->tuning_params[i].var);
    m[@(name)] = @((int64_t)ctx->cfg->tuning_params[i].set);
    snprintf(name, sizeof(name), "val_%s", ctx->cfg->tuning_params[i].var);
    m[@(name)] = @((int64_t)ctx->cfg->tuning_params[i].val);
  }

  m[@"LOCKSTEP_WIDTH"] = @((int64_t)ctx->lockstep_width);
  m[@"max_thread_block_size"] = @((int64_t)ctx->max_thread_block_size);
  m[@"max_shared_memory"] = @((int64_t)ctx->max_shared_memory);
  m[@"max_registers"] = @((int64_t)ctx->max_registers);
  m[@"TR_BLOCK_DIM"] = @(TR_BLOCK_DIM);
  m[@"TR_TILE_DIM"] = @(TR_TILE_DIM);
  m[@"TR_ELEMS_PER_THREAD"] = @(TR_ELEMS_PER_THREAD);

  for (int i = 0; i < ctx->cfg->num_build_opts; i++) {
    const char *opt = ctx->cfg->build_opts[i];
    if (strncmp(opt, "-D", 2) != 0) {
      futhark_panic(1, "Unsupported Metal build option: %s\n", opt);
    }
    const char *eq = strchr(opt, '=');
    if (eq != NULL) {
      NSString *name = [[[NSString alloc] initWithBytes:opt+2
                                                 length:eq-opt-2
                                               encoding:NSUTF8StringEncoding]
                         autorelease];
      m[name] = @(eq+1);
    } else {
      m[@(opt+2)] = @"1";
    }
  }
  return m;
}

static id<MTLLibrary> metal_compile(struct futhark_context *ctx,
                                    const char *src, NSDictionary *macros) {
  id<MTLLibrary> library = nil;
  @autoreleasepool {
    MTLCompileOptions *opts = [[MTLCompileOptions new] autorelease];
    opts.preprocessorMacros = macros;
    opts.mathMode = MTLMathModeSafe;
    opts.mathFloatingPointFunctions = MTLMathFloatingPointFunctionsPrecise;
    NSError *err = nil;
    library = [ctx->device newLibraryWithSource:@(src) options:opts error:&err];
    if (library == nil) {
      futhark_panic(1, "Metal program compilation failed:\n%s\n",
                    [[err localizedDescription] UTF8String]);
    }
  }
  return library;
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
  ctx->cb = nil;
  ctx->encoder = nil;

  ctx->device = metal_select_device(ctx->cfg);
  if (ctx->device == nil) {
    futhark_panic(-1, "No suitable Metal device found.\n");
  }
  if (ctx->cfg->logging) {
    fprintf(ctx->log, "Using device: %s\n", [[ctx->device name] UTF8String]);
  }

  if (ctx->cfg->queue != nil) {
    ctx->queue = [ctx->cfg->queue retain];
  } else {
    ctx->queue = [ctx->device newCommandQueue];
  }
  ctx->timeline = [ctx->device newEvent];
  ctx->timeline_value = 0;

  @autoreleasepool {
    MTLResidencySetDescriptor *desc = [[MTLResidencySetDescriptor new] autorelease];
    NSError *err = nil;
    ctx->residency = [ctx->device newResidencySetWithDescriptor:desc error:&err];
    if (ctx->residency == nil && ctx->cfg->logging) {
      fprintf(ctx->log, "Residency sets not supported (%s); using useResource.\n",
              [[err localizedDescription] UTF8String]);
    }
  }
  if (ctx->residency != nil) {
    [ctx->queue addResidencySet:ctx->residency];
  }

  free_list_init(&ctx->gpu_free_list);
  metal_size_setup(ctx);
  gpu_init_log(ctx);

  int64_t t_start = get_wall_time();
  @autoreleasepool {
    ctx->library = metal_compile(ctx, ctx->cfg->program, metal_macros(ctx));
  }
  if (ctx->cfg->logging) {
    fprintf(ctx->log, "Compiled Metal program in %.3fs.\n",
            (get_wall_time() - t_start) / 1e6);
  }

  id<MTLLibrary> helpers = metal_compile(ctx, metal_helpers_program, @{});
  ctx->copy_kernel = metal_kernel_new("futhark_copy");
  ctx->write_kernel = metal_kernel_new("futhark_write");
  if (metal_kernel_prepare(ctx, helpers, ctx->copy_kernel) != FUTHARK_SUCCESS ||
      metal_kernel_prepare(ctx, helpers, ctx->write_kernel) != FUTHARK_SUCCESS) {
    futhark_panic(-1, "%s", ctx->error);
  }
  [helpers release];

  int32_t no_error = -1;
  METAL_SUCCEED_FATAL(gpu_alloc_actual(ctx, sizeof(no_error), &ctx->global_failure)
                      == FUTHARK_SUCCESS, "allocating global_failure");
  memcpy([metal_buffer(ctx->global_failure) contents], &no_error, sizeof(no_error));
  // The +1 is to avoid zero-byte allocations.
  METAL_SUCCEED_FATAL(gpu_alloc_actual(ctx, sizeof(int64_t)*(max_failure_args+1),
                                       &ctx->global_failure_args)
                      == FUTHARK_SUCCESS, "allocating global_failure_args");

  if ((ctx->kernels = init_builtin_kernels(ctx)) == NULL) {
    return 1;
  }

  return 0;
}

void backend_context_teardown(struct futhark_context* ctx) {
  if (ctx->kernels != NULL) {
    (void)metal_finish(ctx);
    free_builtin_kernels(ctx, ctx->kernels);
    (void)gpu_free_actual(ctx, ctx->global_failure);
    (void)gpu_free_actual(ctx, ctx->global_failure_args);
    (void)gpu_free_all(ctx);
    metal_kernel_free(ctx->copy_kernel);
    metal_kernel_free(ctx->write_kernel);
    [ctx->library release];
    if (ctx->residency != nil) {
      [ctx->queue removeResidencySet:ctx->residency];
    }
    [ctx->residency release];
    [ctx->timeline release];
    [ctx->queue release];
    [ctx->device release];
  }
  free_list_destroy(&ctx->gpu_free_list);
}

void* futhark_context_get_command_queue(struct futhark_context* ctx) {
  return ctx->queue;
}

// End of backends/metal.h.
