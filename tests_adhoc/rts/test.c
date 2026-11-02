// This file has a large amount of boilerplate to make the header files work.

#include <stdint.h>
#include <stdio.h>
#include <stdarg.h>
#include <assert.h>
#include <stdlib.h>

#include "util.h"
#include "lock.h"
#include "event_list.h"
#include "free_list.h"
#include "context_prototypes.h"
#include "errors.h"

#define NUM_TUNING_PARAMS 0
char* tuning_param_names[NUM_TUNING_PARAMS+1] = {};
char* tuning_param_classes[NUM_TUNING_PARAMS+1] = {};
char* tuning_param_vars[NUM_TUNING_PARAMS+1] = {};
int64_t tuning_param_defaults[NUM_TUNING_PARAMS+1] = {};

struct constants { int dummy; };

#include "backends/c.h"

static int init_constants(struct futhark_context *ctx) {
  (void)ctx;
  return 0;
}

static int free_constants(struct futhark_context *ctx) {
  (void)ctx;
  return 0;
}

int futhark_context_clear_caches(struct futhark_context *ctx) {
  (void) ctx;
  return 0;
}

static void setup_program(struct futhark_context *ctx) {
  (void) ctx;
}

static void teardown_program(struct futhark_context *ctx) {
  (void) ctx;
}

#include "context.h"
#include "copy.h"

void test_lmad_memcpyable_rowmajor(void) {
  int r = 2;
  int64_t dst_strides[] = { 5, 1 };
  int64_t src_strides[] = { 5, 1 };
  int64_t shape[] = { 4, 5 };

  assert(lmad_memcpyable(r, dst_strides, src_strides, shape) == true);
}

void test_lmad_memcpyable_colmajor(void) {
  int r = 2;
  int64_t dst_strides[] = { 1, 4 };
  int64_t src_strides[] = { 1, 4 };
  int64_t shape[] = { 4, 5 };

  assert(lmad_memcpyable(r, dst_strides, src_strides, shape) == true);
}

void test_lmad_map_tr_2d(void) {
  int r = 2;
  int64_t dst_strides[] = { 1, 4 };
  int64_t src_strides[] = { 5, 1 };
  int64_t shape[] = { 4, 5 };

  struct lmad_transpose tr;
  assert(lmad_map_tr(&tr, r, dst_strides, src_strides, shape) == true);

  assert(tr.rows == 4);
  assert(tr.cols == 5);
  assert(tr.num_arrays == 1);
  assert(tr.dst_array_stride == 0);
  assert(tr.dst_row_stride == 4);
  assert(tr.src_array_stride == 0);
  assert(tr.src_row_stride == 5);
}

int main() {
  test_lmad_memcpyable_rowmajor();
  test_lmad_memcpyable_colmajor();
  test_lmad_map_tr_2d();
}
