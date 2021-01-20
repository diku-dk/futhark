#include "const_error.h"
#include <stdlib.h>
#include <string.h>
#include <assert.h>

int main() {
  struct futhark_context_config *cfg = futhark_context_config_new();
  struct futhark_context *ctx = futhark_context_new(cfg);

  char *err = futhark_context_get_error(ctx);

  assert(err != NULL);
  assert(strstr(err, "false") != NULL);

  free(err);
  futhark_context_free(ctx);
  futhark_context_config_free(cfg);
}
