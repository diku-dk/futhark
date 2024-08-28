#include "opaque_array.h"
#include <assert.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

void test1(struct futhark_context *ctx) {
  int32_t a = 42;
  float b[2] = {1,2};
  char* err;

  struct futhark_f32_1d *b_fut = futhark_new_f32_1d(ctx, b, 2);
  assert(b_fut != NULL);

  struct futhark_opaque_tup2_i32_arr1d_f32* a_b_fut;
  assert(futhark_new_opaque_tup2_i32_arr1d_f32(ctx, &a_b_fut, a, b_fut) == 0);

  struct futhark_opaque_t* t;
  assert(futhark_entry_mk(ctx, &t, a, b_fut) == 0);

  struct futhark_opaque_arr1d_t* arr_t;
  assert(futhark_entry_arr(ctx, &arr_t, t) == 0);

  // Test shape.
  assert(futhark_shape_opaque_arr1d_t(ctx, arr_t)[0] == 2);

  // Test index out of bounds.
  assert(futhark_index_opaque_arr1d_t(ctx, NULL, arr_t, 2) != 0);
  assert((err = futhark_context_get_error(ctx)) != NULL);
  free(err);

  // Test correct indexing.
  {
    struct futhark_opaque_t* out;
    assert(futhark_index_opaque_arr1d_t(ctx, &out, arr_t, 1) == 0);
    int32_t out0;
    struct futhark_f32_1d *out1;
    assert(futhark_entry_unmk(ctx, &out0, &out1, out) == 0);

    assert(out0 == a);

    float out1_host[2];
    assert(futhark_values_f32_1d(ctx, out1, out1_host) == 0);
    assert(futhark_context_sync(ctx) == 0);
    assert(memcmp(out1_host, b, sizeof(float)*2) == 0);

    assert(futhark_free_opaque_t(ctx, out) == 0);
    assert(futhark_free_f32_1d(ctx, out1) == 0);
  }

  assert(futhark_free_f32_1d(ctx, b_fut) == 0);
  assert(futhark_free_opaque_t(ctx, t) == 0);
  assert(futhark_free_opaque_arr1d_t(ctx, arr_t) == 0);
  assert(futhark_free_opaque_tup2_i32_arr1d_f32(ctx, a_b_fut) == 0);
}

int main() {
  struct futhark_context_config *cfg = futhark_context_config_new();
  struct futhark_context *ctx = futhark_context_new(cfg);
  assert(futhark_context_get_error(ctx) == NULL);

  test1(ctx);

  futhark_context_free(ctx);
  futhark_context_config_free(cfg);
}
