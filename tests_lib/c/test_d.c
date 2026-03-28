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

  struct futhark_opaque_tup9_f32_f32_f32_f32_f32_f32_f32_f32_f32 *tup;
  err = futhark_entry_fromM33(ctx,
                              &tup,
                              m);
  assert(err == 0);

  err = futhark_project_opaque_tup9_f32_f32_f32_f32_f32_f32_f32_f32_f32_0(ctx, &ys[0], tup);
  assert(err == 0);
  err = futhark_project_opaque_tup9_f32_f32_f32_f32_f32_f32_f32_f32_f32_1(ctx, &ys[1], tup);
  assert(err == 0);
  err = futhark_project_opaque_tup9_f32_f32_f32_f32_f32_f32_f32_f32_f32_2(ctx, &ys[2], tup);
  assert(err == 0);
  err = futhark_project_opaque_tup9_f32_f32_f32_f32_f32_f32_f32_f32_f32_3(ctx, &ys[3], tup);
  assert(err == 0);
  err = futhark_project_opaque_tup9_f32_f32_f32_f32_f32_f32_f32_f32_f32_4(ctx, &ys[4], tup);
  assert(err == 0);
  err = futhark_project_opaque_tup9_f32_f32_f32_f32_f32_f32_f32_f32_f32_5(ctx, &ys[5], tup);
  assert(err == 0);
  err = futhark_project_opaque_tup9_f32_f32_f32_f32_f32_f32_f32_f32_f32_6(ctx, &ys[6], tup);
  assert(err == 0);
  err = futhark_project_opaque_tup9_f32_f32_f32_f32_f32_f32_f32_f32_f32_7(ctx, &ys[7], tup);
  assert(err == 0);
  err = futhark_project_opaque_tup9_f32_f32_f32_f32_f32_f32_f32_f32_f32_8(ctx, &ys[8], tup);
  assert(err == 0);

  err = futhark_free_opaque_tup9_f32_f32_f32_f32_f32_f32_f32_f32_f32(ctx, tup);
  assert(err == 0);

  assert(memcmp(xs, ys, sizeof(xs)) == 0);

  err = futhark_free_opaque_m33(ctx, m);
  assert(err == 0);

  futhark_context_free(ctx);
  futhark_context_config_free(cfg);
}
