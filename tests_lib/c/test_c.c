#include "c.h"
#include <assert.h>
#include <stdio.h>

// Test repeated creations and destructions of context.  If the
// context does not clean up properly after itself, then it is likely
// that this test will fail to run.

static const int runs = 100;
static const int alloc_per_run = 1024*1024*1024; // 1GiB

int main() {
  for (int i = 0; i < runs; i++) {
    struct futhark_context_config *cfg = futhark_context_config_new();
    struct futhark_context *ctx = futhark_context_new(cfg);

    int err;

    struct futhark_i64_1d *arr;
    err = futhark_entry_main(ctx, &arr, alloc_per_run/8);
    assert(err == 0);

    err = futhark_free_i64_1d(ctx, arr);
    assert(err == 0);

    futhark_context_free(ctx);
    futhark_context_config_free(cfg);
  }
}
