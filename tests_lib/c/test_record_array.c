#include "record_array.h"
#include <assert.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

void test1(struct futhark_context *ctx) {
  char* err;
  int32_t a[] = {1,2,3};
  float b[] = {1,2,3};

  struct futhark_i32_1d *a_fut = futhark_new_i32_1d(ctx, a, 3);
  assert(a_fut != NULL);
  struct futhark_f32_1d *b_fut = futhark_new_f32_1d(ctx, b, 3);
  assert(b_fut != NULL);
  struct futhark_f32_1d *b_short_fut = futhark_new_f32_1d(ctx, b, 2);
  assert(b_short_fut != NULL);

  struct futhark_opaque_arr1d_tup2_f32_f32 *b_b_fut;

  // Error case for zip.
  assert(futhark_zip_opaque_arr1d_tup2_f32_f32(ctx, &b_b_fut, b_fut, b_short_fut) == 1);
  err = futhark_context_get_error(ctx);
  assert(err != NULL);
  free(err);

  // Correct use of zip.
  assert(futhark_zip_opaque_arr1d_tup2_f32_f32(ctx, &b_b_fut, b_fut, b_fut) == 0);
  struct futhark_opaque_arr1d_tup2_i32_tup2_f32_f32 *a_b_b_fut;
  assert(futhark_zip_opaque_arr1d_tup2_i32_tup2_f32_f32(ctx, &a_b_b_fut, a_fut, b_b_fut) == 0);

  assert(futhark_shape_opaque_arr1d_tup2_i32_tup2_f32_f32(ctx, a_b_b_fut)[0] == 3);

  // Test indexing: out of bounds.
  assert(futhark_index_opaque_arr1d_tup2_i32_tup2_f32_f32(ctx, NULL, a_b_b_fut, 3) != 0);
  err = futhark_context_get_error(ctx);
  assert(err != NULL);
  free(err);

  // Test indexing: in bounds.
  struct futhark_opaque_tup2_i32_tup2_f32_f32* trip_fut;
  assert(futhark_index_opaque_arr1d_tup2_i32_tup2_f32_f32(ctx, &trip_fut, a_b_b_fut, 1) == 0);
  struct futhark_opaque_tup2_f32_f32* pair_fut;
  assert(futhark_project_opaque_tup2_i32_tup2_f32_f32_1(ctx, &pair_fut, trip_fut) == 0);
  {
    int x;
    assert(futhark_project_opaque_tup2_i32_tup2_f32_f32_0(ctx, &x, trip_fut) == 0);
    assert(x == a[1]);
  }
  {
    float x;
    assert(futhark_project_opaque_tup2_f32_f32_0(ctx, &x, pair_fut) == 0);
    assert(x == b[1]);
  }
  {
    float x;
    assert(futhark_project_opaque_tup2_f32_f32_1(ctx, &x, pair_fut) == 0);
    assert(x == b[1]);
  }

  assert(futhark_free_opaque_tup2_f32_f32(ctx, pair_fut) == 0);
  assert(futhark_free_opaque_tup2_i32_tup2_f32_f32(ctx, trip_fut) == 0);
  assert(futhark_free_opaque_arr1d_tup2_i32_tup2_f32_f32(ctx, a_b_b_fut) == 0);
  assert(futhark_free_f32_1d(ctx, b_fut) == 0);
  assert(futhark_free_f32_1d(ctx, b_short_fut) == 0);
  assert(futhark_free_i32_1d(ctx, a_fut) == 0);
  assert(futhark_free_opaque_arr1d_tup2_f32_f32(ctx, b_b_fut) == 0);

}

void test2(struct futhark_context *ctx) {
  struct futhark_opaque_arr1d_tup2_arr1d_i32_f32 *a_b_fut;
  int32_t a[] = {1,2,3,4};
  float b[] = {5,6};

  struct futhark_i32_2d *a_fut = futhark_new_i32_2d(ctx, a, 2, 2);
  assert(a_fut != NULL);
  struct futhark_f32_1d *b_fut = futhark_new_f32_1d(ctx, b, 2);
  assert(b_fut != NULL);

  assert(futhark_zip_opaque_arr1d_tup2_arr1d_i32_f32(ctx, &a_b_fut, a_fut, b_fut) == 0);

  assert(futhark_free_f32_1d(ctx, b_fut) == 0);
  assert(futhark_free_i32_2d(ctx, a_fut) == 0);

  // Valid indexing.
  struct futhark_opaque_tup2_arr1d_i32_f32* a_b_elem_fut;
  assert(futhark_index_opaque_arr1d_tup2_arr1d_i32_f32(ctx, &a_b_elem_fut, a_b_fut, 1) == 0);

  {
    struct futhark_i32_1d *out_fut;
    assert(futhark_project_opaque_tup2_arr1d_i32_f32_0(ctx, &out_fut, a_b_elem_fut) == 0);
    int32_t out[2];
    assert(futhark_values_i32_1d(ctx, out_fut, out) == 0);
    assert(futhark_context_sync(ctx) == 0);
    assert(memcmp(out, &a[2], sizeof(int32_t)*2) == 0);
    assert(futhark_free_i32_1d(ctx, out_fut) == 0);
  }
  {
    float out;
    assert(futhark_project_opaque_tup2_arr1d_i32_f32_1(ctx, &out, a_b_elem_fut) == 0);
    assert(out == b[1]);
  }

  assert(futhark_free_opaque_arr1d_tup2_arr1d_i32_f32(ctx, a_b_fut) == 0);
  assert(futhark_free_opaque_tup2_arr1d_i32_f32(ctx, a_b_elem_fut) == 0);
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
