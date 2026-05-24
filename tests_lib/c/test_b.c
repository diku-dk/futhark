#include "b.h"
#include <assert.h>

int main() {
  struct futhark_context_config *cfg = futhark_context_config_new();
  struct futhark_context *ctx = futhark_context_new(cfg);

  int err;

  struct futhark_opaque_tup2_i32_r *out;

  err = futhark_entry_a(ctx, &out, true);
  assert(err == 0);

  struct futhark_opaque_r *r;
  int x;

  err = futhark_project_opaque_tup2_i32_r_0(ctx, &x, out);
  assert(err == 0);
  assert(x == 32);

  err = futhark_project_opaque_tup2_i32_r_1(ctx, &r, out);
  assert(err == 0);

  err = futhark_free_opaque_tup2_i32_r(ctx, out);
  assert(err == 0);

  err = futhark_free_opaque_r(ctx, r);
  assert(err == 0);

  futhark_context_free(ctx);
  futhark_context_config_free(cfg);
}
