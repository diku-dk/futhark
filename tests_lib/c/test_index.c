#include "index.h"
#include <assert.h>
#include <stdlib.h>
#include <stdint.h>

int main() {
  struct futhark_context_config *cfg = futhark_context_config_new();
  struct futhark_context *ctx = futhark_context_new(cfg);
  assert(futhark_context_get_error(ctx) == NULL);

  char* err;

  struct futhark_i64_1d *a;
  assert(futhark_entry_fun1(ctx, &a, 10) == 0);

  // Error case: index too high.
  assert(futhark_index_i64_1d(ctx, NULL, a, 100) == 1);
  assert((err = futhark_context_get_error(ctx)) != NULL);
  free(err);

  // Error case: index negative.
  assert(futhark_index_i64_1d(ctx, NULL, a, -1) == 1);
  assert((err = futhark_context_get_error(ctx)) != NULL);
  free(err);

  // Correct indexing.
  int64_t x;
  assert(futhark_index_i64_1d(ctx, &x, a, 5) == 0);
  assert(futhark_context_sync(ctx) == 0);
  assert(x == 5);
  assert(futhark_free_i64_1d(ctx, a) == 0);

  struct futhark_i64_2d *b;
  assert(futhark_entry_fun2(ctx, &b, 10) == 0);

  // Error case: index too high along one dimension, but not the
  // other.
  assert(futhark_index_i64_2d(ctx, NULL, b, 0,10) == 1);
  assert((err = futhark_context_get_error(ctx)) != NULL);
  free(err);

  // Correct indexing.
  int64_t y;
  assert(futhark_index_i64_2d(ctx, &y, b, 3, 4) == 0);
  assert(futhark_context_sync(ctx) == 0);
  assert(y == 3+4);
  assert(futhark_free_i64_2d(ctx, b) == 0);

  futhark_context_free(ctx);
  futhark_context_config_free(cfg);
}
