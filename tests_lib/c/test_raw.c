// Fiddling around with the raw arrays API.

#include "raw.h"
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

int main() {
  struct futhark_context_config *cfg = futhark_context_config_new();
  struct futhark_context *ctx = futhark_context_new(cfg);

#ifdef FUTHARK_BACKEND_c

  int32_t data[3] = {1,2,3};

  struct futhark_i32_1d *in = futhark_new_raw_i32_1d(ctx, (unsigned char*)&data, 3);
  assert(in != NULL);
  struct futhark_i32_1d *out;

  assert(futhark_entry_main(ctx, &out, in) == FUTHARK_SUCCESS);

  int32_t *out_ptr = (int32_t*)futhark_values_raw_i32_1d(ctx, out);

  assert(futhark_context_sync(ctx) == FUTHARK_SUCCESS);

  assert(out_ptr[0] == 3);
  assert(out_ptr[1] == 4);
  assert(out_ptr[2] == 5);

  futhark_free_i32_1d(ctx, in);
  futhark_free_i32_1d(ctx, out);

#endif

  futhark_context_free(ctx);
  futhark_context_config_free(cfg);
}
