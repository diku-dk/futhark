#include "a.h"
#include <assert.h>

int main() {
  struct futhark_context_config *cfg = futhark_context_config_new();
  struct futhark_context *ctx = futhark_context_new(cfg);

  int err;

  int x, y;
  x = 3;
  err = futhark_entry_a(ctx, &y, x);
  assert(err == 0);
  assert(y==x+2);

  struct futhark_opaque_t1 *t1;
  err = futhark_entry_b(ctx, &t1, x);
  assert(err == 0);

  struct futhark_opaque_t2 *t2;
  err = futhark_entry_c(ctx, &t2, t1);
  assert(err == 0);

  err = futhark_free_opaque_t1(ctx, t1);
  assert(err == 0);

  err = futhark_entry_d(ctx, &y, t2);
  assert(err == 0);
  assert(y==x+3);

  err = futhark_free_opaque_t2(ctx, t2);
  assert(err == 0);

  futhark_context_free(ctx);
  futhark_context_config_free(cfg);
}
