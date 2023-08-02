// Start of gpu_prototypes.h

struct builtin_kernels* init_builtin_kernels(struct futhark_context* ctx);
void free_builtin_kernels(struct futhark_context* ctx, struct builtin_kernels* kernels);
static int gpu_free_all(struct futhark_context *ctx);

// End of gpu_prototypes.h
