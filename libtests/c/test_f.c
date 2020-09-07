#include "f.h"
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <stdio.h>

int xs[] = { 42, 1, 2, 3, 4, 5, 6, 7, 8, 9 };
const int n = sizeof(xs)/sizeof(int);

struct thread_args {
  struct futhark_context *ctx;
  struct futhark_i32_1d *xs_fut;
};

void* thread_fn(void* arg) {
  struct thread_args args = *(struct thread_args*)arg;
  struct futhark_context *ctx = args.ctx;
  struct futhark_i32_1d *out_fut = NULL;
  int err;

  err = futhark_entry_main(ctx, &out_fut, args.xs_fut);
  assert(err == 0);
  int out[n];
  err = futhark_values_i32_1d(ctx, out_fut, out);
  assert(err == 0);
  err = futhark_context_sync(ctx);
  assert(err == 0);
  for (int i = 0; i < n; i++) {
    assert(out[i] == xs[i]+2);
  }
  err = futhark_free_i32_1d(ctx, out_fut);
  assert(err == 0);
  err = futhark_free_i32_1d(ctx, args.xs_fut);
  assert(err == 0);
  return NULL;
}

int main() {
  struct futhark_context_config *cfg = futhark_context_config_new();
  struct futhark_context *ctx = futhark_context_new(cfg);
  int err;

  struct futhark_i32_1d *xs_fut = futhark_new_i32_1d(ctx, xs, n);

  struct thread_args args;
  args.ctx = ctx;
  args.xs_fut = xs_fut;

  pthread_t tid;
  err = pthread_create(&tid, NULL, thread_fn, &args);
  assert(err == 0);

  err = pthread_join(tid, NULL);
  assert(err == 0);

  futhark_context_free(ctx);
  futhark_context_config_free(cfg);
}
