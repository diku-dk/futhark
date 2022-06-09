#include "d.h"
#include <string.h>
#include <assert.h>

int main() {
  struct futhark_context_config *cfg = futhark_context_config_new();
  struct futhark_context *ctx = futhark_context_new(cfg);

  int err;

  float xs[9] = { 0, 1, 2, 3, 4, 5, 6, 7, 8 };
  float ys[9];

  struct futhark_opaque_m33 *m;

  err = futhark_entry_toM33(ctx, &m,
                            xs[0], xs[1], xs[2],
                            xs[3], xs[4], xs[5],
                            xs[6], xs[7], xs[8]);
  assert(err == 0);

  err = futhark_entry_fromM33(ctx,
                              &ys[0], &ys[1], &ys[2],
                              &ys[3], &ys[4], &ys[5],
                              &ys[6], &ys[7], &ys[8],
                              m);
  assert(err == 0);

  assert(memcmp(xs, ys, sizeof(xs)) == 0);

  err = futhark_free_opaque_m33(ctx, m);
  assert(err == 0);

  futhark_context_free(ctx);
  futhark_context_config_free(cfg);
}
