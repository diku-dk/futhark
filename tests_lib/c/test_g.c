#include "g.h"
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

int main() {
  struct futhark_context_config *cfg = futhark_context_config_new();
  struct futhark_context *ctx = futhark_context_new(cfg);

  int err;

  int64_t n = 1;
  int64_t m = 1000;

  struct futhark_opaque_vec *vec_n, *vec_m;

  err = futhark_entry_mk_vec(ctx, &vec_n, n);
  assert(err == 0);

  err = futhark_entry_mk_vec(ctx, &vec_m, m);
  assert(err == 0);

  int64_t out = 42;
  err = futhark_entry_use_vec(ctx, &out, vec_n, vec_n);
  assert(err == 0);
  assert(out == 0);

  err = futhark_entry_use_vec(ctx, &out, vec_n, vec_m);
  assert(err != 0);

  char *err_s = futhark_context_get_error(ctx);
  assert(err_s != NULL);
  free(err_s);

  err = futhark_free_opaque_vec(ctx, vec_n);
  assert(err == 0);

  err = futhark_free_opaque_vec(ctx, vec_m);
  assert(err == 0);

  futhark_context_free(ctx);
  futhark_context_config_free(cfg);
}
