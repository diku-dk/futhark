// Check that a failed allocation in futhark_new_*() is reported as an
// error, and that the context remains usable afterwards.

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "liboom.h"

int main(void) {
  struct futhark_context_config *cfg = futhark_context_config_new();
  struct futhark_context *ctx = futhark_context_new(cfg);

  int64_t x = 0;
  struct futhark_i64_1d *arr = futhark_new_i64_1d(ctx, &x, (int64_t)1 << 28);
  if (arr != NULL) {
    fprintf(stderr, "futhark_new_i64_1d() unexpectedly succeeded.\n");
    return 1;
  }
  char *err = futhark_context_get_error(ctx);
  if (err == NULL || strstr(err, "Failed to allocate") == NULL) {
    fprintf(stderr, "Unexpected error: %s\n", err ? err : "(none)");
    return 1;
  }
  free(err);

  struct futhark_i64_1d *out;
  if (futhark_entry_main(ctx, &out, 3) != 0 || futhark_context_sync(ctx) != 0) {
    fprintf(stderr, "Context unusable after failed allocation.\n");
    return 1;
  }
  int64_t res[3];
  if (futhark_values_i64_1d(ctx, out, res) != 0
      || futhark_context_sync(ctx) != 0
      || res[0] != 1
      || res[1] != 2
      || res[2] != 3) {
    fprintf(stderr, "Wrong result after failed allocation.\n");
    return 1;
  }
  futhark_free_i64_1d(ctx, out);

  futhark_context_free(ctx);
  futhark_context_config_free(cfg);
  return 0;
}
