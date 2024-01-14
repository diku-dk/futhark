#include "sum.h"
#include <assert.h>
#include <string.h>
#include <stdlib.h>

int main() {
  int rounds = 10;

  struct futhark_context_config *cfg = futhark_context_config_new();
  struct futhark_context *ctx = futhark_context_new(cfg);

  struct futhark_opaque_contrived* v;
  {
    int32_t data[] = { 1, 2, 3 };
    struct futhark_i32_1d* arr = futhark_new_i32_1d(ctx, data, sizeof(data)/sizeof(int32_t));
    assert(arr != NULL);
    assert(futhark_context_sync(ctx) == FUTHARK_SUCCESS);
    assert(futhark_new_opaque_contrived_foo(ctx, &v, arr, true) == FUTHARK_SUCCESS);
    futhark_free_i32_1d(ctx, arr);
  }

  for (int i = 0; i < rounds; i++) {

    switch (futhark_variant_opaque_contrived(ctx, v)) {
    case 0:
      {
        bool v0;
        struct futhark_u32_1d *v1;
        futhark_destruct_opaque_contrived_bar(ctx, &v0, &v1, v);
        futhark_free_u32_1d(ctx, v1);
      }
      break;
    case 1:
      {
        struct futhark_i32_1d *v0;
        struct futhark_i32_1d *v1;
        futhark_destruct_opaque_contrived_baz(ctx, &v0, &v1, v);
        futhark_free_i32_1d(ctx, v0);
        futhark_free_i32_1d(ctx, v1);
      }
      break;
    case 2:
      {
        struct futhark_i32_1d *v0;
        bool v1;
        futhark_destruct_opaque_contrived_foo(ctx, &v0, &v1, v);
        futhark_free_i32_1d(ctx, v0);
      }
      break;
    default:
      abort();
    };

    struct futhark_opaque_contrived* v_new;
    assert(futhark_entry_next(ctx, &v_new, v) == FUTHARK_SUCCESS);
    assert(futhark_free_opaque_contrived(ctx, v) == FUTHARK_SUCCESS);
    v = v_new;
  }

  assert(futhark_free_opaque_contrived(ctx, v) == FUTHARK_SUCCESS);
  futhark_context_free(ctx);
  futhark_context_config_free(cfg);
}
