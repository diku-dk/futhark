#include "phantomsize.h"
#include <stdlib.h>
#include <assert.h>

int main() {
  struct futhark_context_config *cfg = futhark_context_config_new();
  struct futhark_context *ctx = futhark_context_new(cfg);

  int64_t n = 1000000, m;
  struct futhark_opaque_state *s;
  assert(futhark_entry_construct(ctx, &s, n) == 0);
  assert(futhark_context_sync(ctx) == 0);

  void *bytes = NULL;
  size_t num_bytes = 0;
  assert(futhark_store_opaque_state(ctx, s, &bytes, &num_bytes) == 0);
  assert(futhark_free_opaque_state(ctx, s) == 0);
  s = NULL;

  // Point here is to check that we don't need to store a large array.
  assert(num_bytes < 100);

  s = futhark_restore_opaque_state(ctx, bytes);
  assert(s != NULL);
  assert(futhark_context_sync(ctx) == 0);

  assert(futhark_entry_destruct(ctx, &m, s) == 0);
  assert(futhark_context_sync(ctx) == 0);

  assert(n == m);

  assert(futhark_free_opaque_state(ctx, s) == 0);
  free(bytes);
  futhark_context_free(ctx);
  futhark_context_config_free(cfg);
}
