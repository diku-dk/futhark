#include <assert.h>
#include "record_pattern.h"

int main(void) {
  struct futhark_context_config *cfg = futhark_context_config_new();
  struct futhark_context *ctx = futhark_context_new(cfg);

  struct futhark_opaque_bda45ecc4522eff7bd51d26d27e66014 *record;
  assert(futhark_new_opaque_bda45ecc4522eff7bd51d26d27e66014(ctx, &record, 42, 43) == FUTHARK_SUCCESS);

  int out;
  assert(futhark_entry_foo(ctx, &out, record) == FUTHARK_SUCCESS);
  assert(out == 42+43);
  assert(futhark_entry_bar(ctx, &out, record) == FUTHARK_SUCCESS);
  assert(out == 42+43);

  assert(futhark_free_opaque_bda45ecc4522eff7bd51d26d27e66014(ctx, record) == FUTHARK_SUCCESS);

  futhark_context_free(ctx);
  futhark_context_config_free(cfg);
}
