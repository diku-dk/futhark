#include "e.h"
#include <assert.h>
#include <stdlib.h>
#include <string.h>

int main() {
  struct futhark_context_config *cfg = futhark_context_config_new();
#if defined(FUTHARK_BACKEND_multicore) || defined(FUTHARK_BACKEND_ispc)
  // Ensure the large calls below are split across several subtasks
  // even on a machine with few cores.
  futhark_context_config_set_num_threads(cfg, 4);
#endif
  struct futhark_context *ctx = futhark_context_new(cfg);
  int err;

  float xs[] = { 42, 1, 2, 3, 4, 5, 6, 7, 8, 9 };
  int is0[] = { -1 };
  int is1[] = { 0 };

  struct futhark_f32_1d *xs_fut = futhark_new_f32_1d(ctx, xs, 10);
  struct futhark_i32_1d *is0_fut = futhark_new_i32_1d(ctx, is0, 1);
  struct futhark_i32_1d *is1_fut = futhark_new_i32_1d(ctx, is1, 1);

  assert(xs_fut != NULL);
  assert(is0_fut != NULL);
  assert(is1_fut != NULL);

  float out[1];
  struct futhark_f32_1d *out_fut = NULL;

  err = futhark_entry_main(ctx, &out_fut, xs_fut, is0_fut);

#if defined(FUTHARK_BACKEND_c) || defined(FUTHARK_BACKEND_multicore) || defined(FUTHARK_BACKEND_ispc)
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
  assert(err == 0);
  err = futhark_context_sync(ctx);
  assert(err == 0);
  assert(out[0] == xs[is1[0]]);

  err = futhark_free_f32_1d(ctx, xs_fut);
  assert(err == 0);
  err = futhark_free_i32_1d(ctx, is0_fut);
  assert(err == 0);
  err = futhark_free_i32_1d(ctx, is1_fut);
  assert(err == 0);
  err = futhark_free_f32_1d(ctx, out_fut);
  assert(err == 0);

  // Same again, but with enough work that the parallel backends
  // actually split the failing and the subsequent call into multiple
  // subtasks.  The out-of-bounds index is placed last, such that the
  // failing call still performs (and is timed as performing) real
  // work - otherwise the scheduler may decide to run the subsequent
  // call sequentially, which would not exercise the error state.
  int n = 1 << 20;
  int *is_large0 = calloc(n, sizeof(int));
  int *is_large1 = calloc(n, sizeof(int));
  float *out_large = malloc(n * sizeof(float));
  assert(is_large0 != NULL && is_large1 != NULL && out_large != NULL);
  is_large0[n-1] = 10;

  xs_fut = futhark_new_f32_1d(ctx, xs, 10);
  is0_fut = futhark_new_i32_1d(ctx, is_large0, n);
  is1_fut = futhark_new_i32_1d(ctx, is_large1, n);
  assert(xs_fut != NULL);
  assert(is0_fut != NULL);
  assert(is1_fut != NULL);
  out_fut = NULL;

  err = futhark_entry_main(ctx, &out_fut, xs_fut, is0_fut);
  if (err == 0) {
    err = futhark_context_sync(ctx);
  }
  assert(err == FUTHARK_PROGRAM_ERROR);

  error = futhark_context_get_error(ctx);
  assert(strstr(error, "Index [10] out of bounds") != NULL);
  free(error);
  if (out_fut != NULL) {
    futhark_free_f32_1d(ctx, out_fut);
  }

  err = futhark_entry_main(ctx, &out_fut, xs_fut, is1_fut);
  assert(err == 0);
  err = futhark_context_sync(ctx);
  assert(err == 0);

  err = futhark_values_f32_1d(ctx, out_fut, out_large);
  assert(err == 0);
  err = futhark_context_sync(ctx);
  assert(err == 0);
  for (int i = 0; i < n; i++) {
    assert(out_large[i] == xs[0]);
  }

  err = futhark_free_f32_1d(ctx, xs_fut);
  assert(err == 0);
  err = futhark_free_i32_1d(ctx, is0_fut);
  assert(err == 0);
  err = futhark_free_i32_1d(ctx, is1_fut);
  assert(err == 0);
  err = futhark_free_f32_1d(ctx, out_fut);
  assert(err == 0);
  free(is_large0);
  free(is_large1);
  free(out_large);

  futhark_context_free(ctx);
  futhark_context_config_free(cfg);
}
