#include "restore.h"
#include <assert.h>
#include <stdlib.h>
#include <string.h>

int main() {
  struct futhark_context_config *cfg = futhark_context_config_new();
  struct futhark_context *ctx = futhark_context_new(cfg);

  int err;
  struct futhark_opaque_whatever *whatever;

  err = futhark_entry_mk(ctx, &whatever, 1);
  assert(err == 0);
  err = futhark_context_sync(ctx);
  assert(err == 0);

  void *bytes0 = NULL;
  void *bytes1;
  size_t n0, n1;

  err = futhark_store_opaque_whatever(ctx, whatever, &bytes0, &n0);
  assert(err == 0);
  err = futhark_store_opaque_whatever(ctx, whatever, NULL, &n1);
  assert(err == 0);
  assert(n0 == n1);
  bytes1 = malloc(n1);
  err = futhark_store_opaque_whatever(ctx, whatever, &bytes1, &n1);
  err = futhark_context_sync(ctx);
  assert(err == 0);

  assert(memcmp(bytes0, bytes1, n0) == 0);

  err = futhark_free_opaque_whatever(ctx, whatever);
  assert(err == 0);

  whatever = futhark_restore_opaque_whatever(ctx, bytes0);
  assert(whatever != NULL);

  struct futhark_i64_1d *out0;
  struct futhark_bool_1d *out1;
  bool out2;

  err = futhark_entry_unmk(ctx, &out0, &out1, &out2, whatever);
  assert(err == 0);
  assert(out2 == 1);

  free(bytes0);
  free(bytes1);

  err = futhark_free_i64_1d(ctx, out0);
  assert(err == 0);

  err = futhark_free_bool_1d(ctx, out1);
  assert(err == 0);

  // Test that passing in garbage fails predictably.
  bytes1 = calloc(n0, 1);
  whatever = futhark_restore_opaque_whatever(ctx, bytes1);
  assert(whatever == NULL);
  free(bytes1);

  futhark_context_free(ctx);
  futhark_context_config_free(cfg);
}
