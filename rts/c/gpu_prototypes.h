// Start of gpu_prototypes.h

// Constants used for transpositions.  In principle these should be configurable.
#define TR_BLOCK_DIM 16
#define TR_TILE_DIM (TR_BLOCK_DIM*2)
#define TR_ELEMS_PER_THREAD 8

// Config stuff included in every GPU backend.
struct gpu_config {
  size_t default_block_size;
  size_t default_grid_size;
  size_t default_tile_size;
  size_t default_reg_tile_size;
  size_t default_cache;
  size_t default_shared_memory;
  size_t default_registers;
  size_t default_threshold;

  int default_block_size_changed;
  int default_grid_size_changed;
  int default_tile_size_changed;
};

// The following are dummy sizes that mean the concrete defaults
// will be set during initialisation via hardware-inspection-based
// heuristics.
struct gpu_config gpu_config_initial = {
  0
};

// Must be defined by the user.
static int gpu_macros(struct futhark_context *ctx, char*** names, int64_t** values);

static void gpu_init_log(struct futhark_context *ctx);
struct builtin_kernels* init_builtin_kernels(struct futhark_context* ctx);
void free_builtin_kernels(struct futhark_context* ctx, struct builtin_kernels* kernels);
static int gpu_free_all(struct futhark_context *ctx);

// End of gpu_prototypes.h
