// Start of context_prototypes.h
//
// Prototypes for the functions in context.h that need to be
// available very early.

struct futhark_context_config;
struct futhark_context;

static void set_error(struct futhark_context* ctx, char *error);

// These are called in context new/free functions and contain shared setup.
static void context_setup(struct futhark_context *ctx);
static void context_teardown(struct futhark_context *ctx);

// Allocate host memory.  Must be freed with host_free().
static void host_alloc(struct futhark_context* ctx, size_t size, const char* tag, size_t* size_out, void** mem_out);
// Allocate memory allocated with host_alloc().
static void host_free(struct futhark_context* ctx, size_t size, const char* tag, void* mem);

// End of of context_prototypes.h
