// Start of gpu_prototypes.h

// Constants used for transpositions.  In principle these should be configurable.
#define TR_BLOCK_DIM 16
#define TR_TILE_DIM (TR_BLOCK_DIM*2)
#define TR_ELEMS_PER_THREAD 8

struct builtin_kernels* init_builtin_kernels(struct futhark_context* ctx);
void free_builtin_kernels(struct futhark_context* ctx, struct builtin_kernels* kernels);
static int gpu_free_all(struct futhark_context *ctx);

// End of gpu_prototypes.h
