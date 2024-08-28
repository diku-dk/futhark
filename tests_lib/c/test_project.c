#include "project.h"
#include <assert.h>
#include <string.h>

int main() {
  struct futhark_context_config *cfg = futhark_context_config_new();
  struct futhark_context *ctx = futhark_context_new(cfg);

  int err;

  struct futhark_opaque_sum *sum;

  err = futhark_new_opaque_sum_foo(ctx, &sum, 42);
  assert(err == 0);

  uint32_t u32_data[3] = { 42, 1337, 420 };
  struct futhark_u32_1d* u32_arr = futhark_new_u32_1d(ctx, u32_data, 3);
  assert(u32_arr != NULL);

  float f32_data[3] = { 42, 1337, 420 };
  struct futhark_f32_1d* f32_arr = futhark_new_f32_1d(ctx, f32_data, 3);
  assert(f32_arr != NULL);

  struct futhark_opaque_t0 *t0;
  err = futhark_new_opaque_t0(ctx, &t0, u32_arr, 10, sum);
  assert(err == 0);

  err = futhark_free_u32_1d(ctx, u32_arr);
  assert(err == 0);

  for (int i = 0; i < 2; i++) {
    struct futhark_u32_1d* u32_arr_projected;
    err = futhark_project_opaque_t0_0(ctx, &u32_arr_projected, t0);
    assert(err == 0);
    uint32_t u32_data_projected[3];
    err = futhark_values_u32_1d(ctx, u32_arr_projected, u32_data_projected);
    assert(err == 0);
    err = futhark_context_sync(ctx);
    assert(err == 0);
    assert(memcmp(u32_data, u32_data_projected, sizeof(uint32_t) * 3) == 0);
    err = futhark_free_u32_1d(ctx, u32_arr_projected);
    assert(err == 0);
  }

  struct futhark_opaque_t1 *t1;
  err = futhark_new_opaque_t1(ctx, &t1, t0, f32_arr);
  assert(err == 0);

  for (int i = 0; i < 2; i++) {
    struct futhark_opaque_t0* t0;
    err = futhark_project_opaque_t1_0(ctx, &t0, t1);
    assert(err == 0);
    struct futhark_u32_1d* u32_arr_projected;
    err = futhark_project_opaque_t0_0(ctx, &u32_arr_projected, t0);
    assert(err == 0);
    uint32_t u32_data_projected[3];
    err = futhark_values_u32_1d(ctx, u32_arr_projected, u32_data_projected);
    assert(err == 0);
    err = futhark_context_sync(ctx);
    assert(err == 0);
    assert(memcmp(u32_data, u32_data_projected, sizeof(uint32_t) * 3) == 0);
    err = futhark_free_u32_1d(ctx, u32_arr_projected);
    assert(err == 0);
    err = futhark_free_opaque_t0(ctx, t0);
    assert(err == 0);
  }

  err = futhark_free_opaque_t1(ctx, t1);
  assert(err == 0);
  err = futhark_free_opaque_t0(ctx, t0);
  assert(err == 0);
  err = futhark_free_opaque_sum(ctx, sum);
  assert(err == 0);
  err = futhark_free_f32_1d(ctx, f32_arr);
  assert(err == 0);

  futhark_context_free(ctx);
  futhark_context_config_free(cfg);
}
