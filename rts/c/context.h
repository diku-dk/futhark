// Start of context.h

// Eventually it would be nice to move the context definition in here
// instead of generating it in the compiler.  For now it defines
// various helper functions that must be available.

// Internal functions.

static void set_error(struct futhark_context* ctx, char *error) {
  lock_lock(&ctx->error_lock);
  if (ctx->error == NULL) {
    ctx->error = error;
  } else {
    free(error);
  }
  lock_unlock(&ctx->error_lock);
}

// XXX: should be static, but used in ispc_util.h
void lexical_realloc_error(struct futhark_context* ctx, size_t new_size) {
  set_error(ctx,
            msgprintf("Failed to allocate memory.\nAttempted allocation: %12lld bytes\n",
                      (long long) new_size));
}

static int lexical_realloc(struct futhark_context *ctx,
                           unsigned char **ptr,
                           int64_t *old_size,
                           int64_t new_size) {
  unsigned char *new = realloc(*ptr, (size_t)new_size);
  if (new == NULL) {
    lexical_realloc_error(ctx, new_size);
    return FUTHARK_OUT_OF_MEMORY;
  } else {
    *ptr = new;
    *old_size = new_size;
    return FUTHARK_SUCCESS;
  }
}

// End of context.h
