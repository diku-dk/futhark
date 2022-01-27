#include "e.h"
#include <assert.h>
#include <stdlib.h>
#include <string.h>

int main() {
  struct futhark_context_config *cfg = futhark_context_config_new();
  struct futhark_context *ctx = futhark_context_new(cfg);
  int err;

  float xs[] = { 42, 1, 2, 3, 4, 5, 6, 7, 8, 9 };
  int is0[] = { -1 };
  int is1[] = { 0 };

  struct futhark_f32_1d *xs_fut = futhark_new_f32_1d(ctx, xs, 10);
  struct futhark_i32_1d *is0_fut = futhark_new_i32_1d(ctx, is0, 1);
  struct futhark_i32_1d *is1_fut = futhark_new_i32_1d(ctx, is1, 1);

  float out[1];
  struct futhark_f32_1d *out_fut = NULL;

  err = futhark_entry_main(ctx, &out_fut, xs_fut, is0_fut);

#if defined(FUTHARK_BACKEND_c) || defined(FUTHARK_BACKEND_multicore)
  assert(err == FUTHARK_PROGRAM_ERROR);
  err = futhark_context_sync(ctx);
  assert(err == 0);
#else
  assert(err == 0);
  err = futhark_context_sync(ctx);
  assert(err == FUTHARK_PROGRAM_ERROR);
#endif

  char *error = futhark_context_get_error(ctx);
  assert(strstr(error, "Index [-1] out of bounds") != NULL);
  free(error);
  if (out_fut != NULL) {
    futhark_free_f32_1d(ctx, out_fut);
  }

  err = futhark_entry_main(ctx, &out_fut, xs_fut, is1_fut);
  assert(err == 0);
  err = futhark_context_sync(ctx);
  assert(err == 0);

  err = futhark_values_f32_1d(ctx, out_fut, out);
  assert(out[0] == xs[is1[0]]);

  err = futhark_free_f32_1d(ctx, xs_fut);
  assert(err == 0);
  err = futhark_free_i32_1d(ctx, is0_fut);
  assert(err == 0);
  err = futhark_free_i32_1d(ctx, is1_fut);
  assert(err == 0);
  err = futhark_free_f32_1d(ctx, out_fut);
  assert(err == 0);

  futhark_context_free(ctx);
  futhark_context_config_free(cfg);
}
