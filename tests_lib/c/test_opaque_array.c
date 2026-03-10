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
  struct futhark_opaque_t* elems[] = {t, t};
  assert(futhark_new_opaque_arr1d_t(ctx, &arr_t, elems, 2) == 0);

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

// Test construction of 2D opaque array.
void test2(struct futhark_context *ctx) {
  float bs[] = {1,2,3,4,5,6,7,8,9,10,11,12};
  struct futhark_f32_1d* bs_fut[6];
  struct futhark_opaque_t* elems[6];
  char* err;

  for (int i = 0; i < 6; i++) {
    bs_fut[i] = futhark_new_f32_1d(ctx, &bs[i*2], 2);
    assert(bs_fut[i] != NULL);
    assert(futhark_entry_mk(ctx, &elems[i], i, bs_fut[i]) == 0);
  }

  struct futhark_opaque_arr2d_t* arr2_t;
  assert(futhark_new_opaque_arr2d_t(ctx, &arr2_t, elems, 2, 3) == 0);

  // Test correct shape.
  assert(futhark_shape_opaque_arr2d_t(ctx, arr2_t)[0] == 2);
  assert(futhark_shape_opaque_arr2d_t(ctx, arr2_t)[1] == 3);

  // Test that the values are right.
  for (int i = 0; i < 2; i++) {
    for (int j = 0; j < 3; j++) {
      struct futhark_opaque_t* out;
      assert(futhark_index_opaque_arr2d_t(ctx, &out, arr2_t, i, j) == 0);
      assert(futhark_context_sync(ctx) == 0);
      int32_t out0;
      struct futhark_f32_1d *out1;
      assert(futhark_entry_unmk(ctx, &out0, &out1, out) == 0);

      assert(out0 == i*3+j);

      float out1_host[2];
      assert(futhark_values_f32_1d(ctx, out1, out1_host) == 0);
      assert(futhark_context_sync(ctx) == 0);
      assert(memcmp(out1_host, &bs[i*6+j*2], sizeof(float)*2) == 0);

      futhark_free_opaque_t(ctx, out);
      futhark_free_f32_1d(ctx, out1);
    }
  }

  // Test update
  {
    // Out of bounds.
    assert(futhark_set_opaque_arr2d_t(ctx, arr2_t, elems[0], 2, 0) != 0);
    assert((err = futhark_context_get_error(ctx)) != NULL);
    free(err);

    // In bounds.
    assert(futhark_set_opaque_arr2d_t(ctx, arr2_t, elems[0], 1, 2) == 0);

    // Now let us see if anything changed.
    struct futhark_opaque_t* out;
    assert(futhark_index_opaque_arr2d_t(ctx, &out, arr2_t, 1, 2) == 0);
    assert(futhark_context_sync(ctx) == 0);
    int32_t out0;
    struct futhark_f32_1d *out1;
    assert(futhark_entry_unmk(ctx, &out0, &out1, out) == 0);

    assert(out0 == 0);

    float out1_host[2];
    assert(futhark_values_f32_1d(ctx, out1, out1_host) == 0);
    assert(futhark_context_sync(ctx) == 0);
    assert(memcmp(out1_host, &bs[0], sizeof(float)*2) == 0);

    futhark_free_opaque_t(ctx, out);
    futhark_free_f32_1d(ctx, out1);
  }

  for (int i = 0; i < 6; i++) {
    assert(futhark_free_f32_1d(ctx, bs_fut[i]) == 0);
    assert(futhark_free_opaque_t(ctx, elems[i]) == 0);
  }

  futhark_free_opaque_arr2d_t(ctx, arr2_t);
}

int main() {
  struct futhark_context_config *cfg = futhark_context_config_new();
  struct futhark_context *ctx = futhark_context_new(cfg);
  assert(futhark_context_get_error(ctx) == NULL);

  test1(ctx);
  test2(ctx);

  futhark_context_free(ctx);
  futhark_context_config_free(cfg);
}
