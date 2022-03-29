
// We need to define _GNU_SOURCE before
// _any_ headers files are imported to get
// the usage statistics of a thread (i.e. have RUSAGE_THREAD) on GNU/Linux
// https://manpages.courier-mta.org/htmlman2/getrusage.2.html
#ifndef _GNU_SOURCE // Avoid possible double-definition warning.
#define _GNU_SOURCE
#endif

#ifdef __clang__
#pragma clang diagnostic ignored "-Wunused-function"
#pragma clang diagnostic ignored "-Wunused-variable"
#pragma clang diagnostic ignored "-Wparentheses"
#pragma clang diagnostic ignored "-Wunused-label"
#elif __GNUC__
#pragma GCC diagnostic ignored "-Wunused-function"
#pragma GCC diagnostic ignored "-Wunused-variable"
#pragma GCC diagnostic ignored "-Wparentheses"
#pragma GCC diagnostic ignored "-Wunused-label"
#pragma GCC diagnostic ignored "-Wunused-but-set-variable"
#endif

// Headers\n")
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <stdio.h>
#include <float.h>

#ifdef __cplusplus
extern "C" {
#endif

// Initialisation
struct futhark_context_config;
struct futhark_context_config *futhark_context_config_new(void);
void futhark_context_config_free(struct futhark_context_config *cfg);
void futhark_context_config_set_debugging(struct futhark_context_config *cfg,
                                          int flag);
void futhark_context_config_set_profiling(struct futhark_context_config *cfg,
                                          int flag);
void futhark_context_config_set_logging(struct futhark_context_config *cfg,
                                        int flag);
struct futhark_context;
struct futhark_context *futhark_context_new(struct futhark_context_config *cfg);
void futhark_context_free(struct futhark_context *ctx);
int futhark_context_config_set_tuning_param(struct futhark_context_config *cfg,
                                            const char *param_name,
                                            size_t param_value);
int futhark_get_tuning_param_count(void);
const char *futhark_get_tuning_param_name(int);
const char *futhark_get_tuning_param_class(int);

// Arrays
struct futhark_f32_1d;
struct futhark_f32_1d *futhark_new_f32_1d(struct futhark_context *ctx, const
                                          float *data, int64_t dim0);
struct futhark_f32_1d *futhark_new_raw_f32_1d(struct futhark_context *ctx, const
                                              unsigned char *data,
                                              int64_t offset, int64_t dim0);
int futhark_free_f32_1d(struct futhark_context *ctx,
                        struct futhark_f32_1d *arr);
int futhark_values_f32_1d(struct futhark_context *ctx,
                          struct futhark_f32_1d *arr, float *data);
unsigned char *futhark_values_raw_f32_1d(struct futhark_context *ctx,
                                         struct futhark_f32_1d *arr);
const int64_t *futhark_shape_f32_1d(struct futhark_context *ctx,
                                    struct futhark_f32_1d *arr);
struct futhark_f32_2d;
struct futhark_f32_2d *futhark_new_f32_2d(struct futhark_context *ctx, const
                                          float *data, int64_t dim0,
                                          int64_t dim1);
struct futhark_f32_2d *futhark_new_raw_f32_2d(struct futhark_context *ctx, const
                                              unsigned char *data,
                                              int64_t offset, int64_t dim0,
                                              int64_t dim1);
int futhark_free_f32_2d(struct futhark_context *ctx,
                        struct futhark_f32_2d *arr);
int futhark_values_f32_2d(struct futhark_context *ctx,
                          struct futhark_f32_2d *arr, float *data);
unsigned char *futhark_values_raw_f32_2d(struct futhark_context *ctx,
                                         struct futhark_f32_2d *arr);
const int64_t *futhark_shape_f32_2d(struct futhark_context *ctx,
                                    struct futhark_f32_2d *arr);
struct futhark_f32_3d;
struct futhark_f32_3d *futhark_new_f32_3d(struct futhark_context *ctx, const
                                          float *data, int64_t dim0,
                                          int64_t dim1, int64_t dim2);
struct futhark_f32_3d *futhark_new_raw_f32_3d(struct futhark_context *ctx, const
                                              unsigned char *data,
                                              int64_t offset, int64_t dim0,
                                              int64_t dim1, int64_t dim2);
int futhark_free_f32_3d(struct futhark_context *ctx,
                        struct futhark_f32_3d *arr);
int futhark_values_f32_3d(struct futhark_context *ctx,
                          struct futhark_f32_3d *arr, float *data);
unsigned char *futhark_values_raw_f32_3d(struct futhark_context *ctx,
                                         struct futhark_f32_3d *arr);
const int64_t *futhark_shape_f32_3d(struct futhark_context *ctx,
                                    struct futhark_f32_3d *arr);
struct futhark_i32_2d;
struct futhark_i32_2d *futhark_new_i32_2d(struct futhark_context *ctx, const
                                          int32_t *data, int64_t dim0,
                                          int64_t dim1);
struct futhark_i32_2d *futhark_new_raw_i32_2d(struct futhark_context *ctx, const
                                              unsigned char *data,
                                              int64_t offset, int64_t dim0,
                                              int64_t dim1);
int futhark_free_i32_2d(struct futhark_context *ctx,
                        struct futhark_i32_2d *arr);
int futhark_values_i32_2d(struct futhark_context *ctx,
                          struct futhark_i32_2d *arr, int32_t *data);
unsigned char *futhark_values_raw_i32_2d(struct futhark_context *ctx,
                                         struct futhark_i32_2d *arr);
const int64_t *futhark_shape_i32_2d(struct futhark_context *ctx,
                                    struct futhark_i32_2d *arr);

// Opaque values


// Entry points
int futhark_entry_main(struct futhark_context *ctx,
                       struct futhark_f32_3d **out0,
                       struct futhark_f32_3d **out1,
                       struct futhark_f32_3d **out2,
                       struct futhark_f32_3d **out3,
                       struct futhark_f32_3d **out4,
                       struct futhark_f32_3d **out5,
                       struct futhark_f32_2d **out6, const
                       struct futhark_f32_3d *in0, const
                       struct futhark_f32_3d *in1, const
                       struct futhark_f32_3d *in2, const
                       struct futhark_f32_3d *in3, const
                       struct futhark_f32_3d *in4, const
                       struct futhark_f32_3d *in5, const
                       struct futhark_f32_3d *in6, const
                       struct futhark_f32_3d *in7, const
                       struct futhark_f32_3d *in8, const
                       struct futhark_f32_3d *in9, const
                       struct futhark_f32_3d *in10, const
                       struct futhark_f32_3d *in11, const
                       struct futhark_f32_1d *in12, const
                       struct futhark_f32_1d *in13, const
                       struct futhark_f32_1d *in14, const
                       struct futhark_f32_1d *in15, const
                       struct futhark_f32_1d *in16, const
                       struct futhark_f32_1d *in17, const
                       struct futhark_f32_1d *in18, const
                       struct futhark_f32_1d *in19, const
                       struct futhark_i32_2d *in20, const
                       struct futhark_f32_3d *in21, const
                       struct futhark_f32_3d *in22, const
                       struct futhark_f32_3d *in23, const
                       struct futhark_f32_2d *in24);

// Miscellaneous
int futhark_context_sync(struct futhark_context *ctx);
char *futhark_context_report(struct futhark_context *ctx);
char *futhark_context_get_error(struct futhark_context *ctx);
void futhark_context_set_logging_file(struct futhark_context *ctx, FILE *f);
void futhark_context_pause_profiling(struct futhark_context *ctx);
void futhark_context_unpause_profiling(struct futhark_context *ctx);
int futhark_context_clear_caches(struct futhark_context *ctx);
#define FUTHARK_BACKEND_c
#define FUTHARK_SUCCESS 0
#define FUTHARK_PROGRAM_ERROR 2
#define FUTHARK_OUT_OF_MEMORY 3

#ifdef __cplusplus
}
#endif

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <math.h>
#include <stdint.h>
// If NDEBUG is set, the assert() macro will do nothing. Since Futhark
// (unfortunately) makes use of assert() for error detection (and even some
// side effects), we want to avoid that.
#undef NDEBUG
#include <assert.h>
#include <stdarg.h>
// Start of util.h.
//
// Various helper functions that are useful in all generated C code.

#include <errno.h>
#include <string.h>

static const char *fut_progname = "(embedded Futhark)";

static void futhark_panic(int eval, const char *fmt, ...) __attribute__((noreturn));
static char* msgprintf(const char *s, ...);
static void* slurp_file(const char *filename, size_t *size);
static int dump_file(const char *file, const void *buf, size_t n);
struct str_builder;
static void str_builder_init(struct str_builder *b);
static void str_builder(struct str_builder *b, const char *s, ...);

static void futhark_panic(int eval, const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  fprintf(stderr, "%s: ", fut_progname);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
  exit(eval);
}

// For generating arbitrary-sized error messages.  It is the callers
// responsibility to free the buffer at some point.
static char* msgprintf(const char *s, ...) {
  va_list vl;
  va_start(vl, s);
  size_t needed = 1 + (size_t)vsnprintf(NULL, 0, s, vl);
  char *buffer = (char*) malloc(needed);
  va_start(vl, s); // Must re-init.
  vsnprintf(buffer, needed, s, vl);
  return buffer;
}

static inline void check_err(int errval, int sets_errno, const char *fun, int line,
                             const char *msg, ...) {
  if (errval) {
    char errnum[10];

    va_list vl;
    va_start(vl, msg);

    fprintf(stderr, "ERROR: ");
    vfprintf(stderr, msg, vl);
    fprintf(stderr, " in %s() at line %d with error code %s\n",
            fun, line,
            sets_errno ? strerror(errno) : errnum);
    exit(errval);
  }
}

#define CHECK_ERR(err, ...) check_err(err, 0, __func__, __LINE__, __VA_ARGS__)
#define CHECK_ERRNO(err, ...) check_err(err, 1, __func__, __LINE__, __VA_ARGS__)

// Read the rest of an open file into a NUL-terminated string; returns
// NULL on error.
static void* fslurp_file(FILE *f, size_t *size) {
  long start = ftell(f);
  fseek(f, 0, SEEK_END);
  long src_size = ftell(f)-start;
  fseek(f, start, SEEK_SET);
  unsigned char *s = (unsigned char*) malloc((size_t)src_size + 1);
  if (fread(s, 1, (size_t)src_size, f) != (size_t)src_size) {
    free(s);
    s = NULL;
  } else {
    s[src_size] = '\0';
  }

  if (size) {
    *size = (size_t)src_size;
  }

  return s;
}

// Read a file into a NUL-terminated string; returns NULL on error.
static void* slurp_file(const char *filename, size_t *size) {
  FILE *f = fopen(filename, "rb"); // To avoid Windows messing with linebreaks.
  if (f == NULL) return NULL;
  unsigned char *s = fslurp_file(f, size);
  fclose(f);
  return s;
}

// Dump 'n' bytes from 'buf' into the file at the designated location.
// Returns 0 on success.
static int dump_file(const char *file, const void *buf, size_t n) {
  FILE *f = fopen(file, "w");

  if (f == NULL) {
    return 1;
  }

  if (fwrite(buf, sizeof(char), n, f) != n) {
    return 1;
  }

  if (fclose(f) != 0) {
    return 1;
  }

  return 0;
}

struct str_builder {
  char *str;
  size_t capacity; // Size of buffer.
  size_t used; // Bytes used, *not* including final zero.
};

static void str_builder_init(struct str_builder *b) {
  b->capacity = 10;
  b->used = 0;
  b->str = malloc(b->capacity);
  b->str[0] = 0;
}

static void str_builder(struct str_builder *b, const char *s, ...) {
  va_list vl;
  va_start(vl, s);
  size_t needed = (size_t)vsnprintf(NULL, 0, s, vl);

  while (b->capacity < b->used + needed + 1) {
    b->capacity *= 2;
    b->str = realloc(b->str, b->capacity);
  }

  va_start(vl, s); // Must re-init.
  vsnprintf(b->str+b->used, b->capacity-b->used, s, vl);
  b->used += needed;
}

static int lexical_realloc(char **error, unsigned char **ptr, size_t *old_size, size_t new_size) {
  unsigned char *new = realloc(*ptr, new_size);
  if (new == NULL) {
    *error = msgprintf("Failed to allocate memory.\nAttempted allocation: %12lld bytes\n",
                       (long long) new_size);
    return FUTHARK_OUT_OF_MEMORY;
  } else {
    *ptr = new;
    *old_size = new_size;
    return FUTHARK_SUCCESS;
  }
}

// End of util.h.
// Start of half.h.

// Conversion functions are from http://half.sourceforge.net/, but
// translated to C.
//
// Copyright (c) 2012-2021 Christian Rau
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.

#ifndef __OPENCL_VERSION__
#define __constant
#endif

__constant static const uint16_t base_table[512] = {
  0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
  0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
  0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
  0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
  0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
  0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
  0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0001, 0x0002, 0x0004, 0x0008, 0x0010, 0x0020, 0x0040, 0x0080, 0x0100,
  0x0200, 0x0400, 0x0800, 0x0C00, 0x1000, 0x1400, 0x1800, 0x1C00, 0x2000, 0x2400, 0x2800, 0x2C00, 0x3000, 0x3400, 0x3800, 0x3C00,
  0x4000, 0x4400, 0x4800, 0x4C00, 0x5000, 0x5400, 0x5800, 0x5C00, 0x6000, 0x6400, 0x6800, 0x6C00, 0x7000, 0x7400, 0x7800, 0x7C00,
  0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00,
  0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00,
  0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00,
  0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00,
  0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00,
  0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00,
  0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00, 0x7C00,
  0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000,
  0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000,
  0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000,
  0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000,
  0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000,
  0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000,
  0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8000, 0x8001, 0x8002, 0x8004, 0x8008, 0x8010, 0x8020, 0x8040, 0x8080, 0x8100,
  0x8200, 0x8400, 0x8800, 0x8C00, 0x9000, 0x9400, 0x9800, 0x9C00, 0xA000, 0xA400, 0xA800, 0xAC00, 0xB000, 0xB400, 0xB800, 0xBC00,
  0xC000, 0xC400, 0xC800, 0xCC00, 0xD000, 0xD400, 0xD800, 0xDC00, 0xE000, 0xE400, 0xE800, 0xEC00, 0xF000, 0xF400, 0xF800, 0xFC00,
  0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00,
  0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00,
  0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00,
  0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00,
  0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00,
  0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00,
  0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00, 0xFC00 };

__constant static const unsigned char shift_table[512] = {
  24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24,
  24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24,
  24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24,
  24, 24, 24, 24, 24, 24, 24, 23, 22, 21, 20, 19, 18, 17, 16, 15, 14, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13,
  13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24,
  24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24,
  24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24,
  24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 13,
  24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24,
  24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24,
  24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24,
  24, 24, 24, 24, 24, 24, 24, 23, 22, 21, 20, 19, 18, 17, 16, 15, 14, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13,
  13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24,
  24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24,
  24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24,
  24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 13 };

__constant static const uint32_t mantissa_table[2048] = {
  0x00000000, 0x33800000, 0x34000000, 0x34400000, 0x34800000, 0x34A00000, 0x34C00000, 0x34E00000, 0x35000000, 0x35100000, 0x35200000, 0x35300000, 0x35400000, 0x35500000, 0x35600000, 0x35700000,
  0x35800000, 0x35880000, 0x35900000, 0x35980000, 0x35A00000, 0x35A80000, 0x35B00000, 0x35B80000, 0x35C00000, 0x35C80000, 0x35D00000, 0x35D80000, 0x35E00000, 0x35E80000, 0x35F00000, 0x35F80000,
  0x36000000, 0x36040000, 0x36080000, 0x360C0000, 0x36100000, 0x36140000, 0x36180000, 0x361C0000, 0x36200000, 0x36240000, 0x36280000, 0x362C0000, 0x36300000, 0x36340000, 0x36380000, 0x363C0000,
  0x36400000, 0x36440000, 0x36480000, 0x364C0000, 0x36500000, 0x36540000, 0x36580000, 0x365C0000, 0x36600000, 0x36640000, 0x36680000, 0x366C0000, 0x36700000, 0x36740000, 0x36780000, 0x367C0000,
  0x36800000, 0x36820000, 0x36840000, 0x36860000, 0x36880000, 0x368A0000, 0x368C0000, 0x368E0000, 0x36900000, 0x36920000, 0x36940000, 0x36960000, 0x36980000, 0x369A0000, 0x369C0000, 0x369E0000,
  0x36A00000, 0x36A20000, 0x36A40000, 0x36A60000, 0x36A80000, 0x36AA0000, 0x36AC0000, 0x36AE0000, 0x36B00000, 0x36B20000, 0x36B40000, 0x36B60000, 0x36B80000, 0x36BA0000, 0x36BC0000, 0x36BE0000,
  0x36C00000, 0x36C20000, 0x36C40000, 0x36C60000, 0x36C80000, 0x36CA0000, 0x36CC0000, 0x36CE0000, 0x36D00000, 0x36D20000, 0x36D40000, 0x36D60000, 0x36D80000, 0x36DA0000, 0x36DC0000, 0x36DE0000,
  0x36E00000, 0x36E20000, 0x36E40000, 0x36E60000, 0x36E80000, 0x36EA0000, 0x36EC0000, 0x36EE0000, 0x36F00000, 0x36F20000, 0x36F40000, 0x36F60000, 0x36F80000, 0x36FA0000, 0x36FC0000, 0x36FE0000,
  0x37000000, 0x37010000, 0x37020000, 0x37030000, 0x37040000, 0x37050000, 0x37060000, 0x37070000, 0x37080000, 0x37090000, 0x370A0000, 0x370B0000, 0x370C0000, 0x370D0000, 0x370E0000, 0x370F0000,
  0x37100000, 0x37110000, 0x37120000, 0x37130000, 0x37140000, 0x37150000, 0x37160000, 0x37170000, 0x37180000, 0x37190000, 0x371A0000, 0x371B0000, 0x371C0000, 0x371D0000, 0x371E0000, 0x371F0000,
  0x37200000, 0x37210000, 0x37220000, 0x37230000, 0x37240000, 0x37250000, 0x37260000, 0x37270000, 0x37280000, 0x37290000, 0x372A0000, 0x372B0000, 0x372C0000, 0x372D0000, 0x372E0000, 0x372F0000,
  0x37300000, 0x37310000, 0x37320000, 0x37330000, 0x37340000, 0x37350000, 0x37360000, 0x37370000, 0x37380000, 0x37390000, 0x373A0000, 0x373B0000, 0x373C0000, 0x373D0000, 0x373E0000, 0x373F0000,
  0x37400000, 0x37410000, 0x37420000, 0x37430000, 0x37440000, 0x37450000, 0x37460000, 0x37470000, 0x37480000, 0x37490000, 0x374A0000, 0x374B0000, 0x374C0000, 0x374D0000, 0x374E0000, 0x374F0000,
  0x37500000, 0x37510000, 0x37520000, 0x37530000, 0x37540000, 0x37550000, 0x37560000, 0x37570000, 0x37580000, 0x37590000, 0x375A0000, 0x375B0000, 0x375C0000, 0x375D0000, 0x375E0000, 0x375F0000,
  0x37600000, 0x37610000, 0x37620000, 0x37630000, 0x37640000, 0x37650000, 0x37660000, 0x37670000, 0x37680000, 0x37690000, 0x376A0000, 0x376B0000, 0x376C0000, 0x376D0000, 0x376E0000, 0x376F0000,
  0x37700000, 0x37710000, 0x37720000, 0x37730000, 0x37740000, 0x37750000, 0x37760000, 0x37770000, 0x37780000, 0x37790000, 0x377A0000, 0x377B0000, 0x377C0000, 0x377D0000, 0x377E0000, 0x377F0000,
  0x37800000, 0x37808000, 0x37810000, 0x37818000, 0x37820000, 0x37828000, 0x37830000, 0x37838000, 0x37840000, 0x37848000, 0x37850000, 0x37858000, 0x37860000, 0x37868000, 0x37870000, 0x37878000,
  0x37880000, 0x37888000, 0x37890000, 0x37898000, 0x378A0000, 0x378A8000, 0x378B0000, 0x378B8000, 0x378C0000, 0x378C8000, 0x378D0000, 0x378D8000, 0x378E0000, 0x378E8000, 0x378F0000, 0x378F8000,
  0x37900000, 0x37908000, 0x37910000, 0x37918000, 0x37920000, 0x37928000, 0x37930000, 0x37938000, 0x37940000, 0x37948000, 0x37950000, 0x37958000, 0x37960000, 0x37968000, 0x37970000, 0x37978000,
  0x37980000, 0x37988000, 0x37990000, 0x37998000, 0x379A0000, 0x379A8000, 0x379B0000, 0x379B8000, 0x379C0000, 0x379C8000, 0x379D0000, 0x379D8000, 0x379E0000, 0x379E8000, 0x379F0000, 0x379F8000,
  0x37A00000, 0x37A08000, 0x37A10000, 0x37A18000, 0x37A20000, 0x37A28000, 0x37A30000, 0x37A38000, 0x37A40000, 0x37A48000, 0x37A50000, 0x37A58000, 0x37A60000, 0x37A68000, 0x37A70000, 0x37A78000,
  0x37A80000, 0x37A88000, 0x37A90000, 0x37A98000, 0x37AA0000, 0x37AA8000, 0x37AB0000, 0x37AB8000, 0x37AC0000, 0x37AC8000, 0x37AD0000, 0x37AD8000, 0x37AE0000, 0x37AE8000, 0x37AF0000, 0x37AF8000,
  0x37B00000, 0x37B08000, 0x37B10000, 0x37B18000, 0x37B20000, 0x37B28000, 0x37B30000, 0x37B38000, 0x37B40000, 0x37B48000, 0x37B50000, 0x37B58000, 0x37B60000, 0x37B68000, 0x37B70000, 0x37B78000,
  0x37B80000, 0x37B88000, 0x37B90000, 0x37B98000, 0x37BA0000, 0x37BA8000, 0x37BB0000, 0x37BB8000, 0x37BC0000, 0x37BC8000, 0x37BD0000, 0x37BD8000, 0x37BE0000, 0x37BE8000, 0x37BF0000, 0x37BF8000,
  0x37C00000, 0x37C08000, 0x37C10000, 0x37C18000, 0x37C20000, 0x37C28000, 0x37C30000, 0x37C38000, 0x37C40000, 0x37C48000, 0x37C50000, 0x37C58000, 0x37C60000, 0x37C68000, 0x37C70000, 0x37C78000,
  0x37C80000, 0x37C88000, 0x37C90000, 0x37C98000, 0x37CA0000, 0x37CA8000, 0x37CB0000, 0x37CB8000, 0x37CC0000, 0x37CC8000, 0x37CD0000, 0x37CD8000, 0x37CE0000, 0x37CE8000, 0x37CF0000, 0x37CF8000,
  0x37D00000, 0x37D08000, 0x37D10000, 0x37D18000, 0x37D20000, 0x37D28000, 0x37D30000, 0x37D38000, 0x37D40000, 0x37D48000, 0x37D50000, 0x37D58000, 0x37D60000, 0x37D68000, 0x37D70000, 0x37D78000,
  0x37D80000, 0x37D88000, 0x37D90000, 0x37D98000, 0x37DA0000, 0x37DA8000, 0x37DB0000, 0x37DB8000, 0x37DC0000, 0x37DC8000, 0x37DD0000, 0x37DD8000, 0x37DE0000, 0x37DE8000, 0x37DF0000, 0x37DF8000,
  0x37E00000, 0x37E08000, 0x37E10000, 0x37E18000, 0x37E20000, 0x37E28000, 0x37E30000, 0x37E38000, 0x37E40000, 0x37E48000, 0x37E50000, 0x37E58000, 0x37E60000, 0x37E68000, 0x37E70000, 0x37E78000,
  0x37E80000, 0x37E88000, 0x37E90000, 0x37E98000, 0x37EA0000, 0x37EA8000, 0x37EB0000, 0x37EB8000, 0x37EC0000, 0x37EC8000, 0x37ED0000, 0x37ED8000, 0x37EE0000, 0x37EE8000, 0x37EF0000, 0x37EF8000,
  0x37F00000, 0x37F08000, 0x37F10000, 0x37F18000, 0x37F20000, 0x37F28000, 0x37F30000, 0x37F38000, 0x37F40000, 0x37F48000, 0x37F50000, 0x37F58000, 0x37F60000, 0x37F68000, 0x37F70000, 0x37F78000,
  0x37F80000, 0x37F88000, 0x37F90000, 0x37F98000, 0x37FA0000, 0x37FA8000, 0x37FB0000, 0x37FB8000, 0x37FC0000, 0x37FC8000, 0x37FD0000, 0x37FD8000, 0x37FE0000, 0x37FE8000, 0x37FF0000, 0x37FF8000,
  0x38000000, 0x38004000, 0x38008000, 0x3800C000, 0x38010000, 0x38014000, 0x38018000, 0x3801C000, 0x38020000, 0x38024000, 0x38028000, 0x3802C000, 0x38030000, 0x38034000, 0x38038000, 0x3803C000,
  0x38040000, 0x38044000, 0x38048000, 0x3804C000, 0x38050000, 0x38054000, 0x38058000, 0x3805C000, 0x38060000, 0x38064000, 0x38068000, 0x3806C000, 0x38070000, 0x38074000, 0x38078000, 0x3807C000,
  0x38080000, 0x38084000, 0x38088000, 0x3808C000, 0x38090000, 0x38094000, 0x38098000, 0x3809C000, 0x380A0000, 0x380A4000, 0x380A8000, 0x380AC000, 0x380B0000, 0x380B4000, 0x380B8000, 0x380BC000,
  0x380C0000, 0x380C4000, 0x380C8000, 0x380CC000, 0x380D0000, 0x380D4000, 0x380D8000, 0x380DC000, 0x380E0000, 0x380E4000, 0x380E8000, 0x380EC000, 0x380F0000, 0x380F4000, 0x380F8000, 0x380FC000,
  0x38100000, 0x38104000, 0x38108000, 0x3810C000, 0x38110000, 0x38114000, 0x38118000, 0x3811C000, 0x38120000, 0x38124000, 0x38128000, 0x3812C000, 0x38130000, 0x38134000, 0x38138000, 0x3813C000,
  0x38140000, 0x38144000, 0x38148000, 0x3814C000, 0x38150000, 0x38154000, 0x38158000, 0x3815C000, 0x38160000, 0x38164000, 0x38168000, 0x3816C000, 0x38170000, 0x38174000, 0x38178000, 0x3817C000,
  0x38180000, 0x38184000, 0x38188000, 0x3818C000, 0x38190000, 0x38194000, 0x38198000, 0x3819C000, 0x381A0000, 0x381A4000, 0x381A8000, 0x381AC000, 0x381B0000, 0x381B4000, 0x381B8000, 0x381BC000,
  0x381C0000, 0x381C4000, 0x381C8000, 0x381CC000, 0x381D0000, 0x381D4000, 0x381D8000, 0x381DC000, 0x381E0000, 0x381E4000, 0x381E8000, 0x381EC000, 0x381F0000, 0x381F4000, 0x381F8000, 0x381FC000,
  0x38200000, 0x38204000, 0x38208000, 0x3820C000, 0x38210000, 0x38214000, 0x38218000, 0x3821C000, 0x38220000, 0x38224000, 0x38228000, 0x3822C000, 0x38230000, 0x38234000, 0x38238000, 0x3823C000,
  0x38240000, 0x38244000, 0x38248000, 0x3824C000, 0x38250000, 0x38254000, 0x38258000, 0x3825C000, 0x38260000, 0x38264000, 0x38268000, 0x3826C000, 0x38270000, 0x38274000, 0x38278000, 0x3827C000,
  0x38280000, 0x38284000, 0x38288000, 0x3828C000, 0x38290000, 0x38294000, 0x38298000, 0x3829C000, 0x382A0000, 0x382A4000, 0x382A8000, 0x382AC000, 0x382B0000, 0x382B4000, 0x382B8000, 0x382BC000,
  0x382C0000, 0x382C4000, 0x382C8000, 0x382CC000, 0x382D0000, 0x382D4000, 0x382D8000, 0x382DC000, 0x382E0000, 0x382E4000, 0x382E8000, 0x382EC000, 0x382F0000, 0x382F4000, 0x382F8000, 0x382FC000,
  0x38300000, 0x38304000, 0x38308000, 0x3830C000, 0x38310000, 0x38314000, 0x38318000, 0x3831C000, 0x38320000, 0x38324000, 0x38328000, 0x3832C000, 0x38330000, 0x38334000, 0x38338000, 0x3833C000,
  0x38340000, 0x38344000, 0x38348000, 0x3834C000, 0x38350000, 0x38354000, 0x38358000, 0x3835C000, 0x38360000, 0x38364000, 0x38368000, 0x3836C000, 0x38370000, 0x38374000, 0x38378000, 0x3837C000,
  0x38380000, 0x38384000, 0x38388000, 0x3838C000, 0x38390000, 0x38394000, 0x38398000, 0x3839C000, 0x383A0000, 0x383A4000, 0x383A8000, 0x383AC000, 0x383B0000, 0x383B4000, 0x383B8000, 0x383BC000,
  0x383C0000, 0x383C4000, 0x383C8000, 0x383CC000, 0x383D0000, 0x383D4000, 0x383D8000, 0x383DC000, 0x383E0000, 0x383E4000, 0x383E8000, 0x383EC000, 0x383F0000, 0x383F4000, 0x383F8000, 0x383FC000,
  0x38400000, 0x38404000, 0x38408000, 0x3840C000, 0x38410000, 0x38414000, 0x38418000, 0x3841C000, 0x38420000, 0x38424000, 0x38428000, 0x3842C000, 0x38430000, 0x38434000, 0x38438000, 0x3843C000,
  0x38440000, 0x38444000, 0x38448000, 0x3844C000, 0x38450000, 0x38454000, 0x38458000, 0x3845C000, 0x38460000, 0x38464000, 0x38468000, 0x3846C000, 0x38470000, 0x38474000, 0x38478000, 0x3847C000,
  0x38480000, 0x38484000, 0x38488000, 0x3848C000, 0x38490000, 0x38494000, 0x38498000, 0x3849C000, 0x384A0000, 0x384A4000, 0x384A8000, 0x384AC000, 0x384B0000, 0x384B4000, 0x384B8000, 0x384BC000,
  0x384C0000, 0x384C4000, 0x384C8000, 0x384CC000, 0x384D0000, 0x384D4000, 0x384D8000, 0x384DC000, 0x384E0000, 0x384E4000, 0x384E8000, 0x384EC000, 0x384F0000, 0x384F4000, 0x384F8000, 0x384FC000,
  0x38500000, 0x38504000, 0x38508000, 0x3850C000, 0x38510000, 0x38514000, 0x38518000, 0x3851C000, 0x38520000, 0x38524000, 0x38528000, 0x3852C000, 0x38530000, 0x38534000, 0x38538000, 0x3853C000,
  0x38540000, 0x38544000, 0x38548000, 0x3854C000, 0x38550000, 0x38554000, 0x38558000, 0x3855C000, 0x38560000, 0x38564000, 0x38568000, 0x3856C000, 0x38570000, 0x38574000, 0x38578000, 0x3857C000,
  0x38580000, 0x38584000, 0x38588000, 0x3858C000, 0x38590000, 0x38594000, 0x38598000, 0x3859C000, 0x385A0000, 0x385A4000, 0x385A8000, 0x385AC000, 0x385B0000, 0x385B4000, 0x385B8000, 0x385BC000,
  0x385C0000, 0x385C4000, 0x385C8000, 0x385CC000, 0x385D0000, 0x385D4000, 0x385D8000, 0x385DC000, 0x385E0000, 0x385E4000, 0x385E8000, 0x385EC000, 0x385F0000, 0x385F4000, 0x385F8000, 0x385FC000,
  0x38600000, 0x38604000, 0x38608000, 0x3860C000, 0x38610000, 0x38614000, 0x38618000, 0x3861C000, 0x38620000, 0x38624000, 0x38628000, 0x3862C000, 0x38630000, 0x38634000, 0x38638000, 0x3863C000,
  0x38640000, 0x38644000, 0x38648000, 0x3864C000, 0x38650000, 0x38654000, 0x38658000, 0x3865C000, 0x38660000, 0x38664000, 0x38668000, 0x3866C000, 0x38670000, 0x38674000, 0x38678000, 0x3867C000,
  0x38680000, 0x38684000, 0x38688000, 0x3868C000, 0x38690000, 0x38694000, 0x38698000, 0x3869C000, 0x386A0000, 0x386A4000, 0x386A8000, 0x386AC000, 0x386B0000, 0x386B4000, 0x386B8000, 0x386BC000,
  0x386C0000, 0x386C4000, 0x386C8000, 0x386CC000, 0x386D0000, 0x386D4000, 0x386D8000, 0x386DC000, 0x386E0000, 0x386E4000, 0x386E8000, 0x386EC000, 0x386F0000, 0x386F4000, 0x386F8000, 0x386FC000,
  0x38700000, 0x38704000, 0x38708000, 0x3870C000, 0x38710000, 0x38714000, 0x38718000, 0x3871C000, 0x38720000, 0x38724000, 0x38728000, 0x3872C000, 0x38730000, 0x38734000, 0x38738000, 0x3873C000,
  0x38740000, 0x38744000, 0x38748000, 0x3874C000, 0x38750000, 0x38754000, 0x38758000, 0x3875C000, 0x38760000, 0x38764000, 0x38768000, 0x3876C000, 0x38770000, 0x38774000, 0x38778000, 0x3877C000,
  0x38780000, 0x38784000, 0x38788000, 0x3878C000, 0x38790000, 0x38794000, 0x38798000, 0x3879C000, 0x387A0000, 0x387A4000, 0x387A8000, 0x387AC000, 0x387B0000, 0x387B4000, 0x387B8000, 0x387BC000,
  0x387C0000, 0x387C4000, 0x387C8000, 0x387CC000, 0x387D0000, 0x387D4000, 0x387D8000, 0x387DC000, 0x387E0000, 0x387E4000, 0x387E8000, 0x387EC000, 0x387F0000, 0x387F4000, 0x387F8000, 0x387FC000,
  0x38000000, 0x38002000, 0x38004000, 0x38006000, 0x38008000, 0x3800A000, 0x3800C000, 0x3800E000, 0x38010000, 0x38012000, 0x38014000, 0x38016000, 0x38018000, 0x3801A000, 0x3801C000, 0x3801E000,
  0x38020000, 0x38022000, 0x38024000, 0x38026000, 0x38028000, 0x3802A000, 0x3802C000, 0x3802E000, 0x38030000, 0x38032000, 0x38034000, 0x38036000, 0x38038000, 0x3803A000, 0x3803C000, 0x3803E000,
  0x38040000, 0x38042000, 0x38044000, 0x38046000, 0x38048000, 0x3804A000, 0x3804C000, 0x3804E000, 0x38050000, 0x38052000, 0x38054000, 0x38056000, 0x38058000, 0x3805A000, 0x3805C000, 0x3805E000,
  0x38060000, 0x38062000, 0x38064000, 0x38066000, 0x38068000, 0x3806A000, 0x3806C000, 0x3806E000, 0x38070000, 0x38072000, 0x38074000, 0x38076000, 0x38078000, 0x3807A000, 0x3807C000, 0x3807E000,
  0x38080000, 0x38082000, 0x38084000, 0x38086000, 0x38088000, 0x3808A000, 0x3808C000, 0x3808E000, 0x38090000, 0x38092000, 0x38094000, 0x38096000, 0x38098000, 0x3809A000, 0x3809C000, 0x3809E000,
  0x380A0000, 0x380A2000, 0x380A4000, 0x380A6000, 0x380A8000, 0x380AA000, 0x380AC000, 0x380AE000, 0x380B0000, 0x380B2000, 0x380B4000, 0x380B6000, 0x380B8000, 0x380BA000, 0x380BC000, 0x380BE000,
  0x380C0000, 0x380C2000, 0x380C4000, 0x380C6000, 0x380C8000, 0x380CA000, 0x380CC000, 0x380CE000, 0x380D0000, 0x380D2000, 0x380D4000, 0x380D6000, 0x380D8000, 0x380DA000, 0x380DC000, 0x380DE000,
  0x380E0000, 0x380E2000, 0x380E4000, 0x380E6000, 0x380E8000, 0x380EA000, 0x380EC000, 0x380EE000, 0x380F0000, 0x380F2000, 0x380F4000, 0x380F6000, 0x380F8000, 0x380FA000, 0x380FC000, 0x380FE000,
  0x38100000, 0x38102000, 0x38104000, 0x38106000, 0x38108000, 0x3810A000, 0x3810C000, 0x3810E000, 0x38110000, 0x38112000, 0x38114000, 0x38116000, 0x38118000, 0x3811A000, 0x3811C000, 0x3811E000,
  0x38120000, 0x38122000, 0x38124000, 0x38126000, 0x38128000, 0x3812A000, 0x3812C000, 0x3812E000, 0x38130000, 0x38132000, 0x38134000, 0x38136000, 0x38138000, 0x3813A000, 0x3813C000, 0x3813E000,
  0x38140000, 0x38142000, 0x38144000, 0x38146000, 0x38148000, 0x3814A000, 0x3814C000, 0x3814E000, 0x38150000, 0x38152000, 0x38154000, 0x38156000, 0x38158000, 0x3815A000, 0x3815C000, 0x3815E000,
  0x38160000, 0x38162000, 0x38164000, 0x38166000, 0x38168000, 0x3816A000, 0x3816C000, 0x3816E000, 0x38170000, 0x38172000, 0x38174000, 0x38176000, 0x38178000, 0x3817A000, 0x3817C000, 0x3817E000,
  0x38180000, 0x38182000, 0x38184000, 0x38186000, 0x38188000, 0x3818A000, 0x3818C000, 0x3818E000, 0x38190000, 0x38192000, 0x38194000, 0x38196000, 0x38198000, 0x3819A000, 0x3819C000, 0x3819E000,
  0x381A0000, 0x381A2000, 0x381A4000, 0x381A6000, 0x381A8000, 0x381AA000, 0x381AC000, 0x381AE000, 0x381B0000, 0x381B2000, 0x381B4000, 0x381B6000, 0x381B8000, 0x381BA000, 0x381BC000, 0x381BE000,
  0x381C0000, 0x381C2000, 0x381C4000, 0x381C6000, 0x381C8000, 0x381CA000, 0x381CC000, 0x381CE000, 0x381D0000, 0x381D2000, 0x381D4000, 0x381D6000, 0x381D8000, 0x381DA000, 0x381DC000, 0x381DE000,
  0x381E0000, 0x381E2000, 0x381E4000, 0x381E6000, 0x381E8000, 0x381EA000, 0x381EC000, 0x381EE000, 0x381F0000, 0x381F2000, 0x381F4000, 0x381F6000, 0x381F8000, 0x381FA000, 0x381FC000, 0x381FE000,
  0x38200000, 0x38202000, 0x38204000, 0x38206000, 0x38208000, 0x3820A000, 0x3820C000, 0x3820E000, 0x38210000, 0x38212000, 0x38214000, 0x38216000, 0x38218000, 0x3821A000, 0x3821C000, 0x3821E000,
  0x38220000, 0x38222000, 0x38224000, 0x38226000, 0x38228000, 0x3822A000, 0x3822C000, 0x3822E000, 0x38230000, 0x38232000, 0x38234000, 0x38236000, 0x38238000, 0x3823A000, 0x3823C000, 0x3823E000,
  0x38240000, 0x38242000, 0x38244000, 0x38246000, 0x38248000, 0x3824A000, 0x3824C000, 0x3824E000, 0x38250000, 0x38252000, 0x38254000, 0x38256000, 0x38258000, 0x3825A000, 0x3825C000, 0x3825E000,
  0x38260000, 0x38262000, 0x38264000, 0x38266000, 0x38268000, 0x3826A000, 0x3826C000, 0x3826E000, 0x38270000, 0x38272000, 0x38274000, 0x38276000, 0x38278000, 0x3827A000, 0x3827C000, 0x3827E000,
  0x38280000, 0x38282000, 0x38284000, 0x38286000, 0x38288000, 0x3828A000, 0x3828C000, 0x3828E000, 0x38290000, 0x38292000, 0x38294000, 0x38296000, 0x38298000, 0x3829A000, 0x3829C000, 0x3829E000,
  0x382A0000, 0x382A2000, 0x382A4000, 0x382A6000, 0x382A8000, 0x382AA000, 0x382AC000, 0x382AE000, 0x382B0000, 0x382B2000, 0x382B4000, 0x382B6000, 0x382B8000, 0x382BA000, 0x382BC000, 0x382BE000,
  0x382C0000, 0x382C2000, 0x382C4000, 0x382C6000, 0x382C8000, 0x382CA000, 0x382CC000, 0x382CE000, 0x382D0000, 0x382D2000, 0x382D4000, 0x382D6000, 0x382D8000, 0x382DA000, 0x382DC000, 0x382DE000,
  0x382E0000, 0x382E2000, 0x382E4000, 0x382E6000, 0x382E8000, 0x382EA000, 0x382EC000, 0x382EE000, 0x382F0000, 0x382F2000, 0x382F4000, 0x382F6000, 0x382F8000, 0x382FA000, 0x382FC000, 0x382FE000,
  0x38300000, 0x38302000, 0x38304000, 0x38306000, 0x38308000, 0x3830A000, 0x3830C000, 0x3830E000, 0x38310000, 0x38312000, 0x38314000, 0x38316000, 0x38318000, 0x3831A000, 0x3831C000, 0x3831E000,
  0x38320000, 0x38322000, 0x38324000, 0x38326000, 0x38328000, 0x3832A000, 0x3832C000, 0x3832E000, 0x38330000, 0x38332000, 0x38334000, 0x38336000, 0x38338000, 0x3833A000, 0x3833C000, 0x3833E000,
  0x38340000, 0x38342000, 0x38344000, 0x38346000, 0x38348000, 0x3834A000, 0x3834C000, 0x3834E000, 0x38350000, 0x38352000, 0x38354000, 0x38356000, 0x38358000, 0x3835A000, 0x3835C000, 0x3835E000,
  0x38360000, 0x38362000, 0x38364000, 0x38366000, 0x38368000, 0x3836A000, 0x3836C000, 0x3836E000, 0x38370000, 0x38372000, 0x38374000, 0x38376000, 0x38378000, 0x3837A000, 0x3837C000, 0x3837E000,
  0x38380000, 0x38382000, 0x38384000, 0x38386000, 0x38388000, 0x3838A000, 0x3838C000, 0x3838E000, 0x38390000, 0x38392000, 0x38394000, 0x38396000, 0x38398000, 0x3839A000, 0x3839C000, 0x3839E000,
  0x383A0000, 0x383A2000, 0x383A4000, 0x383A6000, 0x383A8000, 0x383AA000, 0x383AC000, 0x383AE000, 0x383B0000, 0x383B2000, 0x383B4000, 0x383B6000, 0x383B8000, 0x383BA000, 0x383BC000, 0x383BE000,
  0x383C0000, 0x383C2000, 0x383C4000, 0x383C6000, 0x383C8000, 0x383CA000, 0x383CC000, 0x383CE000, 0x383D0000, 0x383D2000, 0x383D4000, 0x383D6000, 0x383D8000, 0x383DA000, 0x383DC000, 0x383DE000,
  0x383E0000, 0x383E2000, 0x383E4000, 0x383E6000, 0x383E8000, 0x383EA000, 0x383EC000, 0x383EE000, 0x383F0000, 0x383F2000, 0x383F4000, 0x383F6000, 0x383F8000, 0x383FA000, 0x383FC000, 0x383FE000,
  0x38400000, 0x38402000, 0x38404000, 0x38406000, 0x38408000, 0x3840A000, 0x3840C000, 0x3840E000, 0x38410000, 0x38412000, 0x38414000, 0x38416000, 0x38418000, 0x3841A000, 0x3841C000, 0x3841E000,
  0x38420000, 0x38422000, 0x38424000, 0x38426000, 0x38428000, 0x3842A000, 0x3842C000, 0x3842E000, 0x38430000, 0x38432000, 0x38434000, 0x38436000, 0x38438000, 0x3843A000, 0x3843C000, 0x3843E000,
  0x38440000, 0x38442000, 0x38444000, 0x38446000, 0x38448000, 0x3844A000, 0x3844C000, 0x3844E000, 0x38450000, 0x38452000, 0x38454000, 0x38456000, 0x38458000, 0x3845A000, 0x3845C000, 0x3845E000,
  0x38460000, 0x38462000, 0x38464000, 0x38466000, 0x38468000, 0x3846A000, 0x3846C000, 0x3846E000, 0x38470000, 0x38472000, 0x38474000, 0x38476000, 0x38478000, 0x3847A000, 0x3847C000, 0x3847E000,
  0x38480000, 0x38482000, 0x38484000, 0x38486000, 0x38488000, 0x3848A000, 0x3848C000, 0x3848E000, 0x38490000, 0x38492000, 0x38494000, 0x38496000, 0x38498000, 0x3849A000, 0x3849C000, 0x3849E000,
  0x384A0000, 0x384A2000, 0x384A4000, 0x384A6000, 0x384A8000, 0x384AA000, 0x384AC000, 0x384AE000, 0x384B0000, 0x384B2000, 0x384B4000, 0x384B6000, 0x384B8000, 0x384BA000, 0x384BC000, 0x384BE000,
  0x384C0000, 0x384C2000, 0x384C4000, 0x384C6000, 0x384C8000, 0x384CA000, 0x384CC000, 0x384CE000, 0x384D0000, 0x384D2000, 0x384D4000, 0x384D6000, 0x384D8000, 0x384DA000, 0x384DC000, 0x384DE000,
  0x384E0000, 0x384E2000, 0x384E4000, 0x384E6000, 0x384E8000, 0x384EA000, 0x384EC000, 0x384EE000, 0x384F0000, 0x384F2000, 0x384F4000, 0x384F6000, 0x384F8000, 0x384FA000, 0x384FC000, 0x384FE000,
  0x38500000, 0x38502000, 0x38504000, 0x38506000, 0x38508000, 0x3850A000, 0x3850C000, 0x3850E000, 0x38510000, 0x38512000, 0x38514000, 0x38516000, 0x38518000, 0x3851A000, 0x3851C000, 0x3851E000,
  0x38520000, 0x38522000, 0x38524000, 0x38526000, 0x38528000, 0x3852A000, 0x3852C000, 0x3852E000, 0x38530000, 0x38532000, 0x38534000, 0x38536000, 0x38538000, 0x3853A000, 0x3853C000, 0x3853E000,
  0x38540000, 0x38542000, 0x38544000, 0x38546000, 0x38548000, 0x3854A000, 0x3854C000, 0x3854E000, 0x38550000, 0x38552000, 0x38554000, 0x38556000, 0x38558000, 0x3855A000, 0x3855C000, 0x3855E000,
  0x38560000, 0x38562000, 0x38564000, 0x38566000, 0x38568000, 0x3856A000, 0x3856C000, 0x3856E000, 0x38570000, 0x38572000, 0x38574000, 0x38576000, 0x38578000, 0x3857A000, 0x3857C000, 0x3857E000,
  0x38580000, 0x38582000, 0x38584000, 0x38586000, 0x38588000, 0x3858A000, 0x3858C000, 0x3858E000, 0x38590000, 0x38592000, 0x38594000, 0x38596000, 0x38598000, 0x3859A000, 0x3859C000, 0x3859E000,
  0x385A0000, 0x385A2000, 0x385A4000, 0x385A6000, 0x385A8000, 0x385AA000, 0x385AC000, 0x385AE000, 0x385B0000, 0x385B2000, 0x385B4000, 0x385B6000, 0x385B8000, 0x385BA000, 0x385BC000, 0x385BE000,
  0x385C0000, 0x385C2000, 0x385C4000, 0x385C6000, 0x385C8000, 0x385CA000, 0x385CC000, 0x385CE000, 0x385D0000, 0x385D2000, 0x385D4000, 0x385D6000, 0x385D8000, 0x385DA000, 0x385DC000, 0x385DE000,
  0x385E0000, 0x385E2000, 0x385E4000, 0x385E6000, 0x385E8000, 0x385EA000, 0x385EC000, 0x385EE000, 0x385F0000, 0x385F2000, 0x385F4000, 0x385F6000, 0x385F8000, 0x385FA000, 0x385FC000, 0x385FE000,
  0x38600000, 0x38602000, 0x38604000, 0x38606000, 0x38608000, 0x3860A000, 0x3860C000, 0x3860E000, 0x38610000, 0x38612000, 0x38614000, 0x38616000, 0x38618000, 0x3861A000, 0x3861C000, 0x3861E000,
  0x38620000, 0x38622000, 0x38624000, 0x38626000, 0x38628000, 0x3862A000, 0x3862C000, 0x3862E000, 0x38630000, 0x38632000, 0x38634000, 0x38636000, 0x38638000, 0x3863A000, 0x3863C000, 0x3863E000,
  0x38640000, 0x38642000, 0x38644000, 0x38646000, 0x38648000, 0x3864A000, 0x3864C000, 0x3864E000, 0x38650000, 0x38652000, 0x38654000, 0x38656000, 0x38658000, 0x3865A000, 0x3865C000, 0x3865E000,
  0x38660000, 0x38662000, 0x38664000, 0x38666000, 0x38668000, 0x3866A000, 0x3866C000, 0x3866E000, 0x38670000, 0x38672000, 0x38674000, 0x38676000, 0x38678000, 0x3867A000, 0x3867C000, 0x3867E000,
  0x38680000, 0x38682000, 0x38684000, 0x38686000, 0x38688000, 0x3868A000, 0x3868C000, 0x3868E000, 0x38690000, 0x38692000, 0x38694000, 0x38696000, 0x38698000, 0x3869A000, 0x3869C000, 0x3869E000,
  0x386A0000, 0x386A2000, 0x386A4000, 0x386A6000, 0x386A8000, 0x386AA000, 0x386AC000, 0x386AE000, 0x386B0000, 0x386B2000, 0x386B4000, 0x386B6000, 0x386B8000, 0x386BA000, 0x386BC000, 0x386BE000,
  0x386C0000, 0x386C2000, 0x386C4000, 0x386C6000, 0x386C8000, 0x386CA000, 0x386CC000, 0x386CE000, 0x386D0000, 0x386D2000, 0x386D4000, 0x386D6000, 0x386D8000, 0x386DA000, 0x386DC000, 0x386DE000,
  0x386E0000, 0x386E2000, 0x386E4000, 0x386E6000, 0x386E8000, 0x386EA000, 0x386EC000, 0x386EE000, 0x386F0000, 0x386F2000, 0x386F4000, 0x386F6000, 0x386F8000, 0x386FA000, 0x386FC000, 0x386FE000,
  0x38700000, 0x38702000, 0x38704000, 0x38706000, 0x38708000, 0x3870A000, 0x3870C000, 0x3870E000, 0x38710000, 0x38712000, 0x38714000, 0x38716000, 0x38718000, 0x3871A000, 0x3871C000, 0x3871E000,
  0x38720000, 0x38722000, 0x38724000, 0x38726000, 0x38728000, 0x3872A000, 0x3872C000, 0x3872E000, 0x38730000, 0x38732000, 0x38734000, 0x38736000, 0x38738000, 0x3873A000, 0x3873C000, 0x3873E000,
  0x38740000, 0x38742000, 0x38744000, 0x38746000, 0x38748000, 0x3874A000, 0x3874C000, 0x3874E000, 0x38750000, 0x38752000, 0x38754000, 0x38756000, 0x38758000, 0x3875A000, 0x3875C000, 0x3875E000,
  0x38760000, 0x38762000, 0x38764000, 0x38766000, 0x38768000, 0x3876A000, 0x3876C000, 0x3876E000, 0x38770000, 0x38772000, 0x38774000, 0x38776000, 0x38778000, 0x3877A000, 0x3877C000, 0x3877E000,
  0x38780000, 0x38782000, 0x38784000, 0x38786000, 0x38788000, 0x3878A000, 0x3878C000, 0x3878E000, 0x38790000, 0x38792000, 0x38794000, 0x38796000, 0x38798000, 0x3879A000, 0x3879C000, 0x3879E000,
  0x387A0000, 0x387A2000, 0x387A4000, 0x387A6000, 0x387A8000, 0x387AA000, 0x387AC000, 0x387AE000, 0x387B0000, 0x387B2000, 0x387B4000, 0x387B6000, 0x387B8000, 0x387BA000, 0x387BC000, 0x387BE000,
  0x387C0000, 0x387C2000, 0x387C4000, 0x387C6000, 0x387C8000, 0x387CA000, 0x387CC000, 0x387CE000, 0x387D0000, 0x387D2000, 0x387D4000, 0x387D6000, 0x387D8000, 0x387DA000, 0x387DC000, 0x387DE000,
  0x387E0000, 0x387E2000, 0x387E4000, 0x387E6000, 0x387E8000, 0x387EA000, 0x387EC000, 0x387EE000, 0x387F0000, 0x387F2000, 0x387F4000, 0x387F6000, 0x387F8000, 0x387FA000, 0x387FC000, 0x387FE000 };
__constant static const uint32_t exponent_table[64] = {
  0x00000000, 0x00800000, 0x01000000, 0x01800000, 0x02000000, 0x02800000, 0x03000000, 0x03800000, 0x04000000, 0x04800000, 0x05000000, 0x05800000, 0x06000000, 0x06800000, 0x07000000, 0x07800000,
  0x08000000, 0x08800000, 0x09000000, 0x09800000, 0x0A000000, 0x0A800000, 0x0B000000, 0x0B800000, 0x0C000000, 0x0C800000, 0x0D000000, 0x0D800000, 0x0E000000, 0x0E800000, 0x0F000000, 0x47800000,
  0x80000000, 0x80800000, 0x81000000, 0x81800000, 0x82000000, 0x82800000, 0x83000000, 0x83800000, 0x84000000, 0x84800000, 0x85000000, 0x85800000, 0x86000000, 0x86800000, 0x87000000, 0x87800000,
  0x88000000, 0x88800000, 0x89000000, 0x89800000, 0x8A000000, 0x8A800000, 0x8B000000, 0x8B800000, 0x8C000000, 0x8C800000, 0x8D000000, 0x8D800000, 0x8E000000, 0x8E800000, 0x8F000000, 0xC7800000 };
__constant static const unsigned short offset_table[64] = {
  0, 1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024,
  0, 1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024, 1024 };

static uint16_t float2halfbits(float value) {
  union { float x; uint32_t y; } u;
  u.x = value;
  uint32_t bits = u.y;

  uint16_t hbits = base_table[bits>>23] + (uint16_t)((bits&0x7FFFFF)>>shift_table[bits>>23]);;

  return hbits;
}

static float halfbits2float(uint16_t value) {
  uint32_t bits = mantissa_table[offset_table[value>>10]+(value&0x3FF)] + exponent_table[value>>10];

  union { uint32_t x; float y; } u;
  u.x = bits;
  return u.y;
}

// End of half.h.
// Start of timing.h.

// The function get_wall_time() returns the wall time in microseconds
// (with an unspecified offset).

#ifdef _WIN32

#include <windows.h>

static int64_t get_wall_time(void) {
  LARGE_INTEGER time,freq;
  assert(QueryPerformanceFrequency(&freq));
  assert(QueryPerformanceCounter(&time));
  return ((double)time.QuadPart / freq.QuadPart) * 1000000;
}

#else
// Assuming POSIX

#include <time.h>
#include <sys/time.h>

static int64_t get_wall_time(void) {
  struct timeval time;
  assert(gettimeofday(&time,NULL) == 0);
  return time.tv_sec * 1000000 + time.tv_usec;
}

static int64_t get_wall_time_ns(void) {
  struct timespec time;
  assert(clock_gettime(CLOCK_REALTIME, &time) == 0);
  return time.tv_sec * 1000000000 + time.tv_nsec;
}

#endif

// End of timing.h.
#include <getopt.h>
#include <ctype.h>
#include <inttypes.h>
#include <unistd.h>
// Start of values.h.

//// Text I/O

typedef int (*writer)(FILE*, const void*);
typedef int (*bin_reader)(void*);
typedef int (*str_reader)(const char *, void*);

struct array_reader {
  char* elems;
  int64_t n_elems_space;
  int64_t elem_size;
  int64_t n_elems_used;
  int64_t *shape;
  str_reader elem_reader;
};

static void skipspaces(FILE *f) {
  int c;
  do {
    c = getc(f);
  } while (isspace(c));

  if (c != EOF) {
    ungetc(c, f);
  }
}

static int constituent(char c) {
  return isalnum(c) || c == '.' || c == '-' || c == '+' || c == '_';
}

// Produces an empty token only on EOF.
static void next_token(FILE *f, char *buf, int bufsize) {
 start:
  skipspaces(f);

  int i = 0;
  while (i < bufsize) {
    int c = getc(f);
    buf[i] = (char)c;

    if (c == EOF) {
      buf[i] = 0;
      return;
    } else if (c == '-' && i == 1 && buf[0] == '-') {
      // Line comment, so skip to end of line and start over.
      for (; c != '\n' && c != EOF; c = getc(f));
      goto start;
    } else if (!constituent((char)c)) {
      if (i == 0) {
        // We permit single-character tokens that are not
        // constituents; this lets things like ']' and ',' be
        // tokens.
        buf[i+1] = 0;
        return;
      } else {
        ungetc(c, f);
        buf[i] = 0;
        return;
      }
    }

    i++;
  }

  buf[bufsize-1] = 0;
}

static int next_token_is(FILE *f, char *buf, int bufsize, const char* expected) {
  next_token(f, buf, bufsize);
  return strcmp(buf, expected) == 0;
}

static void remove_underscores(char *buf) {
  char *w = buf;

  for (char *r = buf; *r; r++) {
    if (*r != '_') {
      *w++ = *r;
    }
  }

  *w++ = 0;
}

static int read_str_elem(char *buf, struct array_reader *reader) {
  int ret;
  if (reader->n_elems_used == reader->n_elems_space) {
    reader->n_elems_space *= 2;
    reader->elems = (char*) realloc(reader->elems,
                                    (size_t)(reader->n_elems_space * reader->elem_size));
  }

  ret = reader->elem_reader(buf, reader->elems + reader->n_elems_used * reader->elem_size);

  if (ret == 0) {
    reader->n_elems_used++;
  }

  return ret;
}

static int read_str_array_elems(FILE *f,
                                char *buf, int bufsize,
                                struct array_reader *reader, int64_t dims) {
  int ret;
  int first = 1;
  char *knows_dimsize = (char*) calloc((size_t)dims, sizeof(char));
  int cur_dim = (int)dims-1;
  int64_t *elems_read_in_dim = (int64_t*) calloc((size_t)dims, sizeof(int64_t));

  while (1) {
    next_token(f, buf, bufsize);

    if (strcmp(buf, "]") == 0) {
      if (knows_dimsize[cur_dim]) {
        if (reader->shape[cur_dim] != elems_read_in_dim[cur_dim]) {
          ret = 1;
          break;
        }
      } else {
        knows_dimsize[cur_dim] = 1;
        reader->shape[cur_dim] = elems_read_in_dim[cur_dim];
      }
      if (cur_dim == 0) {
        ret = 0;
        break;
      } else {
        cur_dim--;
        elems_read_in_dim[cur_dim]++;
      }
    } else if (strcmp(buf, ",") == 0) {
      next_token(f, buf, bufsize);
      if (strcmp(buf, "[") == 0) {
        if (cur_dim == dims - 1) {
          ret = 1;
          break;
        }
        first = 1;
        cur_dim++;
        elems_read_in_dim[cur_dim] = 0;
      } else if (cur_dim == dims - 1) {
        ret = read_str_elem(buf, reader);
        if (ret != 0) {
          break;
        }
        elems_read_in_dim[cur_dim]++;
      } else {
        ret = 1;
        break;
      }
    } else if (strlen(buf) == 0) {
      // EOF
      ret = 1;
      break;
    } else if (first) {
      if (strcmp(buf, "[") == 0) {
        if (cur_dim == dims - 1) {
          ret = 1;
          break;
        }
        cur_dim++;
        elems_read_in_dim[cur_dim] = 0;
      } else {
        ret = read_str_elem(buf, reader);
        if (ret != 0) {
          break;
        }
        elems_read_in_dim[cur_dim]++;
        first = 0;
      }
    } else {
      ret = 1;
      break;
    }
  }

  free(knows_dimsize);
  free(elems_read_in_dim);
  return ret;
}

static int read_str_empty_array(FILE *f, char *buf, int bufsize,
                                const char *type_name, int64_t *shape, int64_t dims) {
  if (strlen(buf) == 0) {
    // EOF
    return 1;
  }

  if (strcmp(buf, "empty") != 0) {
    return 1;
  }

  if (!next_token_is(f, buf, bufsize, "(")) {
    return 1;
  }

  for (int i = 0; i < dims; i++) {
    if (!next_token_is(f, buf, bufsize, "[")) {
      return 1;
    }

    next_token(f, buf, bufsize);

    if (sscanf(buf, "%"SCNu64, (uint64_t*)&shape[i]) != 1) {
      return 1;
    }

    if (!next_token_is(f, buf, bufsize, "]")) {
      return 1;
    }
  }

  if (!next_token_is(f, buf, bufsize, type_name)) {
    return 1;
  }


  if (!next_token_is(f, buf, bufsize, ")")) {
    return 1;
  }

  // Check whether the array really is empty.
  for (int i = 0; i < dims; i++) {
    if (shape[i] == 0) {
      return 0;
    }
  }

  // Not an empty array!
  return 1;
}

static int read_str_array(FILE *f,
                          int64_t elem_size, str_reader elem_reader,
                          const char *type_name,
                          void **data, int64_t *shape, int64_t dims) {
  int ret;
  struct array_reader reader;
  char buf[100];

  int dims_seen;
  for (dims_seen = 0; dims_seen < dims; dims_seen++) {
    if (!next_token_is(f, buf, sizeof(buf), "[")) {
      break;
    }
  }

  if (dims_seen == 0) {
    return read_str_empty_array(f, buf, sizeof(buf), type_name, shape, dims);
  }

  if (dims_seen != dims) {
    return 1;
  }

  reader.shape = shape;
  reader.n_elems_used = 0;
  reader.elem_size = elem_size;
  reader.n_elems_space = 16;
  reader.elems = (char*) realloc(*data, (size_t)(elem_size*reader.n_elems_space));
  reader.elem_reader = elem_reader;

  ret = read_str_array_elems(f, buf, sizeof(buf), &reader, dims);

  *data = reader.elems;

  return ret;
}

#define READ_STR(MACRO, PTR, SUFFIX)                                   \
  remove_underscores(buf);                                              \
  int j;                                                                \
  if (sscanf(buf, "%"MACRO"%n", (PTR*)dest, &j) == 1) {                 \
    return !(strcmp(buf+j, "") == 0 || strcmp(buf+j, SUFFIX) == 0);     \
  } else {                                                              \
    return 1;                                                           \
  }

static int read_str_i8(char *buf, void* dest) {
  // Some platforms (WINDOWS) does not support scanf %hhd or its
  // cousin, %SCNi8.  Read into int first to avoid corrupting
  // memory.
  //
  // https://gcc.gnu.org/bugzilla/show_bug.cgi?id=63417
  remove_underscores(buf);
  int j, x;
  if (sscanf(buf, "%i%n", &x, &j) == 1) {
    *(int8_t*)dest = (int8_t)x;
    return !(strcmp(buf+j, "") == 0 || strcmp(buf+j, "i8") == 0);
  } else {
    return 1;
  }
}

static int read_str_u8(char *buf, void* dest) {
  // Some platforms (WINDOWS) does not support scanf %hhd or its
  // cousin, %SCNu8.  Read into int first to avoid corrupting
  // memory.
  //
  // https://gcc.gnu.org/bugzilla/show_bug.cgi?id=63417
  remove_underscores(buf);
  int j, x;
  if (sscanf(buf, "%i%n", &x, &j) == 1) {
    *(uint8_t*)dest = (uint8_t)x;
    return !(strcmp(buf+j, "") == 0 || strcmp(buf+j, "u8") == 0);
  } else {
    return 1;
  }
}

static int read_str_i16(char *buf, void* dest) {
  READ_STR(SCNi16, int16_t, "i16");
}

static int read_str_u16(char *buf, void* dest) {
  READ_STR(SCNi16, int16_t, "u16");
}

static int read_str_i32(char *buf, void* dest) {
  READ_STR(SCNi32, int32_t, "i32");
}

static int read_str_u32(char *buf, void* dest) {
  READ_STR(SCNi32, int32_t, "u32");
}

static int read_str_i64(char *buf, void* dest) {
  READ_STR(SCNi64, int64_t, "i64");
}

static int read_str_u64(char *buf, void* dest) {
  // FIXME: This is not correct, as SCNu64 only permits decimal
  // literals.  However, SCNi64 does not handle very large numbers
  // correctly (it's really for signed numbers, so that's fair).
  READ_STR(SCNu64, uint64_t, "u64");
}

static int read_str_f16(char *buf, void* dest) {
  remove_underscores(buf);
  if (strcmp(buf, "f16.nan") == 0) {
    *(uint16_t*)dest = float2halfbits(NAN);
    return 0;
  } else if (strcmp(buf, "f16.inf") == 0) {
    *(uint16_t*)dest = float2halfbits(INFINITY);
    return 0;
  } else if (strcmp(buf, "-f16.inf") == 0) {
    *(uint16_t*)dest = float2halfbits(-INFINITY);
    return 0;
  } else {
    int j;
    float x;
    if (sscanf(buf, "%f%n", &x, &j) == 1) {
      if (strcmp(buf+j, "") == 0 || strcmp(buf+j, "f16") == 0) {
        *(uint16_t*)dest = float2halfbits(x);
        return 0;
      }
    }
    return 1;
  }
}

static int read_str_f32(char *buf, void* dest) {
  remove_underscores(buf);
  if (strcmp(buf, "f32.nan") == 0) {
    *(float*)dest = (float)NAN;
    return 0;
  } else if (strcmp(buf, "f32.inf") == 0) {
    *(float*)dest = (float)INFINITY;
    return 0;
  } else if (strcmp(buf, "-f32.inf") == 0) {
    *(float*)dest = (float)-INFINITY;
    return 0;
  } else {
    READ_STR("f", float, "f32");
  }
}

static int read_str_f64(char *buf, void* dest) {
  remove_underscores(buf);
  if (strcmp(buf, "f64.nan") == 0) {
    *(double*)dest = (double)NAN;
    return 0;
  } else if (strcmp(buf, "f64.inf") == 0) {
    *(double*)dest = (double)INFINITY;
    return 0;
  } else if (strcmp(buf, "-f64.inf") == 0) {
    *(double*)dest = (double)-INFINITY;
    return 0;
  } else {
    READ_STR("lf", double, "f64");
  }
}

static int read_str_bool(char *buf, void* dest) {
  if (strcmp(buf, "true") == 0) {
    *(char*)dest = 1;
    return 0;
  } else if (strcmp(buf, "false") == 0) {
    *(char*)dest = 0;
    return 0;
  } else {
    return 1;
  }
}

static int write_str_i8(FILE *out, int8_t *src) {
  return fprintf(out, "%hhdi8", *src);
}

static int write_str_u8(FILE *out, uint8_t *src) {
  return fprintf(out, "%hhuu8", *src);
}

static int write_str_i16(FILE *out, int16_t *src) {
  return fprintf(out, "%hdi16", *src);
}

static int write_str_u16(FILE *out, uint16_t *src) {
  return fprintf(out, "%huu16", *src);
}

static int write_str_i32(FILE *out, int32_t *src) {
  return fprintf(out, "%di32", *src);
}

static int write_str_u32(FILE *out, uint32_t *src) {
  return fprintf(out, "%uu32", *src);
}

static int write_str_i64(FILE *out, int64_t *src) {
  return fprintf(out, "%"PRIi64"i64", *src);
}

static int write_str_u64(FILE *out, uint64_t *src) {
  return fprintf(out, "%"PRIu64"u64", *src);
}

static int write_str_f16(FILE *out, uint16_t *src) {
  float x = halfbits2float(*src);
  if (isnan(x)) {
    return fprintf(out, "f16.nan");
  } else if (isinf(x) && x >= 0) {
    return fprintf(out, "f16.inf");
  } else if (isinf(x)) {
    return fprintf(out, "-f16.inf");
  } else {
    return fprintf(out, "%.6ff16", x);
  }
}

static int write_str_f32(FILE *out, float *src) {
  float x = *src;
  if (isnan(x)) {
    return fprintf(out, "f32.nan");
  } else if (isinf(x) && x >= 0) {
    return fprintf(out, "f32.inf");
  } else if (isinf(x)) {
    return fprintf(out, "-f32.inf");
  } else {
    return fprintf(out, "%.6ff32", x);
  }
}

static int write_str_f64(FILE *out, double *src) {
  double x = *src;
  if (isnan(x)) {
    return fprintf(out, "f64.nan");
  } else if (isinf(x) && x >= 0) {
    return fprintf(out, "f64.inf");
  } else if (isinf(x)) {
    return fprintf(out, "-f64.inf");
  } else {
    return fprintf(out, "%.6ff64", *src);
  }
}

static int write_str_bool(FILE *out, void *src) {
  return fprintf(out, *(char*)src ? "true" : "false");
}

//// Binary I/O

#define BINARY_FORMAT_VERSION 2
#define IS_BIG_ENDIAN (!*(unsigned char *)&(uint16_t){1})

static void flip_bytes(size_t elem_size, unsigned char *elem) {
  for (size_t j=0; j<elem_size/2; j++) {
    unsigned char head = elem[j];
    size_t tail_index = elem_size-1-j;
    elem[j] = elem[tail_index];
    elem[tail_index] = head;
  }
}

// On Windows we need to explicitly set the file mode to not mangle
// newline characters.  On *nix there is no difference.
#ifdef _WIN32
#include <io.h>
#include <fcntl.h>
static void set_binary_mode(FILE *f) {
  setmode(fileno(f), O_BINARY);
}
#else
static void set_binary_mode(FILE *f) {
  (void)f;
}
#endif

static int read_byte(FILE *f, void* dest) {
  size_t num_elems_read = fread(dest, 1, 1, f);
  return num_elems_read == 1 ? 0 : 1;
}

//// Types

struct primtype_info_t {
  const char binname[4]; // Used for parsing binary data.
  const char* type_name; // Same name as in Futhark.
  const int64_t size; // in bytes
  const writer write_str; // Write in text format.
  const str_reader read_str; // Read in text format.
};

static const struct primtype_info_t i8_info =
  {.binname = "  i8", .type_name = "i8",   .size = 1,
   .write_str = (writer)write_str_i8, .read_str = (str_reader)read_str_i8};
static const struct primtype_info_t i16_info =
  {.binname = " i16", .type_name = "i16",  .size = 2,
   .write_str = (writer)write_str_i16, .read_str = (str_reader)read_str_i16};
static const struct primtype_info_t i32_info =
  {.binname = " i32", .type_name = "i32",  .size = 4,
   .write_str = (writer)write_str_i32, .read_str = (str_reader)read_str_i32};
static const struct primtype_info_t i64_info =
  {.binname = " i64", .type_name = "i64",  .size = 8,
   .write_str = (writer)write_str_i64, .read_str = (str_reader)read_str_i64};
static const struct primtype_info_t u8_info =
  {.binname = "  u8", .type_name = "u8",   .size = 1,
   .write_str = (writer)write_str_u8, .read_str = (str_reader)read_str_u8};
static const struct primtype_info_t u16_info =
  {.binname = " u16", .type_name = "u16",  .size = 2,
   .write_str = (writer)write_str_u16, .read_str = (str_reader)read_str_u16};
static const struct primtype_info_t u32_info =
  {.binname = " u32", .type_name = "u32",  .size = 4,
   .write_str = (writer)write_str_u32, .read_str = (str_reader)read_str_u32};
static const struct primtype_info_t u64_info =
  {.binname = " u64", .type_name = "u64",  .size = 8,
   .write_str = (writer)write_str_u64, .read_str = (str_reader)read_str_u64};
static const struct primtype_info_t f16_info =
  {.binname = " f16", .type_name = "f16",  .size = 2,
   .write_str = (writer)write_str_f16, .read_str = (str_reader)read_str_f16};
static const struct primtype_info_t f32_info =
  {.binname = " f32", .type_name = "f32",  .size = 4,
   .write_str = (writer)write_str_f32, .read_str = (str_reader)read_str_f32};
static const struct primtype_info_t f64_info =
  {.binname = " f64", .type_name = "f64",  .size = 8,
   .write_str = (writer)write_str_f64, .read_str = (str_reader)read_str_f64};
static const struct primtype_info_t bool_info =
  {.binname = "bool", .type_name = "bool", .size = 1,
   .write_str = (writer)write_str_bool, .read_str = (str_reader)read_str_bool};

static const struct primtype_info_t* primtypes[] = {
  &i8_info, &i16_info, &i32_info, &i64_info,
  &u8_info, &u16_info, &u32_info, &u64_info,
  &f16_info, &f32_info, &f64_info,
  &bool_info,
  NULL // NULL-terminated
};

// General value interface.  All endian business taken care of at
// lower layers.

static int read_is_binary(FILE *f) {
  skipspaces(f);
  int c = getc(f);
  if (c == 'b') {
    int8_t bin_version;
    int ret = read_byte(f, &bin_version);

    if (ret != 0) { futhark_panic(1, "binary-input: could not read version.\n"); }

    if (bin_version != BINARY_FORMAT_VERSION) {
      futhark_panic(1, "binary-input: File uses version %i, but I only understand version %i.\n",
            bin_version, BINARY_FORMAT_VERSION);
    }

    return 1;
  }
  ungetc(c, f);
  return 0;
}

static const struct primtype_info_t* read_bin_read_type_enum(FILE *f) {
  char read_binname[4];

  int num_matched = fscanf(f, "%4c", read_binname);
  if (num_matched != 1) { futhark_panic(1, "binary-input: Couldn't read element type.\n"); }

  const struct primtype_info_t **type = primtypes;

  for (; *type != NULL; type++) {
    // I compare the 4 characters manually instead of using strncmp because
    // this allows any value to be used, also NULL bytes
    if (memcmp(read_binname, (*type)->binname, 4) == 0) {
      return *type;
    }
  }
  futhark_panic(1, "binary-input: Did not recognize the type '%s'.\n", read_binname);
  return NULL;
}

static void read_bin_ensure_scalar(FILE *f, const struct primtype_info_t *expected_type) {
  int8_t bin_dims;
  int ret = read_byte(f, &bin_dims);
  if (ret != 0) { futhark_panic(1, "binary-input: Couldn't get dims.\n"); }

  if (bin_dims != 0) {
    futhark_panic(1, "binary-input: Expected scalar (0 dimensions), but got array with %i dimensions.\n",
          bin_dims);
  }

  const struct primtype_info_t *bin_type = read_bin_read_type_enum(f);
  if (bin_type != expected_type) {
    futhark_panic(1, "binary-input: Expected scalar of type %s but got scalar of type %s.\n",
          expected_type->type_name,
          bin_type->type_name);
  }
}

//// High-level interface

static int read_bin_array(FILE *f,
                          const struct primtype_info_t *expected_type, void **data, int64_t *shape, int64_t dims) {
  int ret;

  int8_t bin_dims;
  ret = read_byte(f, &bin_dims);
  if (ret != 0) { futhark_panic(1, "binary-input: Couldn't get dims.\n"); }

  if (bin_dims != dims) {
    futhark_panic(1, "binary-input: Expected %i dimensions, but got array with %i dimensions.\n",
          dims, bin_dims);
  }

  const struct primtype_info_t *bin_primtype = read_bin_read_type_enum(f);
  if (expected_type != bin_primtype) {
    futhark_panic(1, "binary-input: Expected %iD-array with element type '%s' but got %iD-array with element type '%s'.\n",
          dims, expected_type->type_name, dims, bin_primtype->type_name);
  }

  int64_t elem_count = 1;
  for (int i=0; i<dims; i++) {
    int64_t bin_shape;
    ret = (int)fread(&bin_shape, sizeof(bin_shape), 1, f);
    if (ret != 1) {
      futhark_panic(1, "binary-input: Couldn't read size for dimension %i of array.\n", i);
    }
    if (IS_BIG_ENDIAN) {
      flip_bytes(sizeof(bin_shape), (unsigned char*) &bin_shape);
    }
    elem_count *= bin_shape;
    shape[i] = bin_shape;
  }

  int64_t elem_size = expected_type->size;
  void* tmp = realloc(*data, (size_t)(elem_count * elem_size));
  if (tmp == NULL) {
    futhark_panic(1, "binary-input: Failed to allocate array of size %i.\n",
          elem_count * elem_size);
  }
  *data = tmp;

  int64_t num_elems_read = (int64_t)fread(*data, (size_t)elem_size, (size_t)elem_count, f);
  if (num_elems_read != elem_count) {
    futhark_panic(1, "binary-input: tried to read %i elements of an array, but only got %i elements.\n",
          elem_count, num_elems_read);
  }

  // If we're on big endian platform we must change all multibyte elements
  // from using little endian to big endian
  if (IS_BIG_ENDIAN && elem_size != 1) {
    flip_bytes((size_t)elem_size, (unsigned char*) *data);
  }

  return 0;
}

static int read_array(FILE *f, const struct primtype_info_t *expected_type, void **data, int64_t *shape, int64_t dims) {
  if (!read_is_binary(f)) {
    return read_str_array(f, expected_type->size, (str_reader)expected_type->read_str, expected_type->type_name, data, shape, dims);
  } else {
    return read_bin_array(f, expected_type, data, shape, dims);
  }
}

static int end_of_input(FILE *f) {
  skipspaces(f);
  char token[2];
  next_token(f, token, sizeof(token));
  if (strcmp(token, "") == 0) {
    return 0;
  } else {
    return 1;
  }
}

static int write_str_array(FILE *out,
                           const struct primtype_info_t *elem_type,
                           const unsigned char *data,
                           const int64_t *shape,
                           int8_t rank) {
  if (rank==0) {
    elem_type->write_str(out, (const void*)data);
  } else {
    int64_t len = (int64_t)shape[0];
    int64_t slice_size = 1;

    int64_t elem_size = elem_type->size;
    for (int8_t i = 1; i < rank; i++) {
      slice_size *= shape[i];
    }

    if (len*slice_size == 0) {
      fprintf(out, "empty(");
      for (int64_t i = 0; i < rank; i++) {
        fprintf(out, "[%"PRIi64"]", shape[i]);
      }
      fprintf(out, "%s", elem_type->type_name);
      fprintf(out, ")");
    } else if (rank==1) {
      fputc('[', out);
      for (int64_t i = 0; i < len; i++) {
        elem_type->write_str(out, (const void*) (data + i * elem_size));
        if (i != len-1) {
          fprintf(out, ", ");
        }
      }
      fputc(']', out);
    } else {
      fputc('[', out);
      for (int64_t i = 0; i < len; i++) {
        write_str_array(out, elem_type, data + i * slice_size * elem_size, shape+1, rank-1);
        if (i != len-1) {
          fprintf(out, ", ");
        }
      }
      fputc(']', out);
    }
  }
  return 0;
}

static int write_bin_array(FILE *out,
                           const struct primtype_info_t *elem_type,
                           const unsigned char *data,
                           const int64_t *shape,
                           int8_t rank) {
  int64_t num_elems = 1;
  for (int64_t i = 0; i < rank; i++) {
    num_elems *= shape[i];
  }

  fputc('b', out);
  fputc((char)BINARY_FORMAT_VERSION, out);
  fwrite(&rank, sizeof(int8_t), 1, out);
  fwrite(elem_type->binname, 4, 1, out);
  if (shape != NULL) {
    fwrite(shape, sizeof(int64_t), (size_t)rank, out);
  }

  if (IS_BIG_ENDIAN) {
    for (int64_t i = 0; i < num_elems; i++) {
      const unsigned char *elem = data+i*elem_type->size;
      for (int64_t j = 0; j < elem_type->size; j++) {
        fwrite(&elem[elem_type->size-j], 1, 1, out);
      }
    }
  } else {
    fwrite(data, (size_t)elem_type->size, (size_t)num_elems, out);
  }

  return 0;
}

static int write_array(FILE *out, int write_binary,
                       const struct primtype_info_t *elem_type,
                       const void *data,
                       const int64_t *shape,
                       const int8_t rank) {
  if (write_binary) {
    return write_bin_array(out, elem_type, data, shape, rank);
  } else {
    return write_str_array(out, elem_type, data, shape, rank);
  }
}

static int read_scalar(FILE *f,
                       const struct primtype_info_t *expected_type, void *dest) {
  if (!read_is_binary(f)) {
    char buf[100];
    next_token(f, buf, sizeof(buf));
    return expected_type->read_str(buf, dest);
  } else {
    read_bin_ensure_scalar(f, expected_type);
    size_t elem_size = (size_t)expected_type->size;
    size_t num_elems_read = fread(dest, elem_size, 1, f);
    if (IS_BIG_ENDIAN) {
      flip_bytes(elem_size, (unsigned char*) dest);
    }
    return num_elems_read == 1 ? 0 : 1;
  }
}

static int write_scalar(FILE *out, int write_binary, const struct primtype_info_t *type, void *src) {
  if (write_binary) {
    return write_bin_array(out, type, src, NULL, 0);
  } else {
    return type->write_str(out, src);
  }
}

// End of values.h.

static int binary_output = 0;
static int print_result = 1;
static FILE *runtime_file;
static int perform_warmup = 0;
static int num_runs = 1;
static const char *entry_point = "main";
// Start of tuning.h.

static char* load_tuning_file(const char *fname,
                              void *cfg,
                              int (*set_tuning_param)(void*, const char*, size_t)) {
  const int max_line_len = 1024;
  char* line = (char*) malloc(max_line_len);

  FILE *f = fopen(fname, "r");

  if (f == NULL) {
    snprintf(line, max_line_len, "Cannot open file: %s", strerror(errno));
    return line;
  }

  int lineno = 0;
  while (fgets(line, max_line_len, f) != NULL) {
    lineno++;
    char *eql = strstr(line, "=");
    if (eql) {
      *eql = 0;
      int value = atoi(eql+1);
      if (set_tuning_param(cfg, line, (size_t)value) != 0) {
        char* err = (char*) malloc(max_line_len + 50);
        snprintf(err, max_line_len + 50, "Unknown name '%s' on line %d.", line, lineno);
        free(line);
        return err;
      }
    } else {
      snprintf(line, max_line_len, "Invalid line %d (must be of form 'name=int').",
               lineno);
      return line;
    }
  }

  free(line);

  return NULL;
}

// End of tuning.h.

int parse_options(struct futhark_context_config *cfg, int argc,
                  char *const argv[])
{
    int ch;
    static struct option long_options[] = {{"write-runtime-to",
                                            required_argument, NULL, 1},
                                           {"runs", required_argument, NULL, 2},
                                           {"debugging", no_argument, NULL, 3},
                                           {"log", no_argument, NULL, 4},
                                           {"entry-point", required_argument,
                                            NULL, 5}, {"binary-output",
                                                       no_argument, NULL, 6},
                                           {"no-print-result", no_argument,
                                            NULL, 7}, {"help", no_argument,
                                                       NULL, 8},
                                           {"print-params", no_argument, NULL,
                                            9}, {"param", required_argument,
                                                 NULL, 10}, {"tuning",
                                                             required_argument,
                                                             NULL, 11}, {0, 0,
                                                                         0, 0}};
    static char *option_descriptions =
                "  -t/--write-runtime-to FILE Print the time taken to execute the program to the indicated file, an integral number of microseconds.\n  -r/--runs INT              Perform NUM runs of the program.\n  -D/--debugging             Perform possibly expensive internal correctness checks and verbose logging.\n  -L/--log                   Print various low-overhead logging information to stderr while running.\n  -e/--entry-point NAME      The entry point to run. Defaults to main.\n  -b/--binary-output         Print the program result in the binary output format.\n  -n/--no-print-result       Do not print the program result.\n  -h/--help                  Print help information and exit.\n  --print-params             Print all tuning parameters that can be set with --param or --tuning.\n  --param ASSIGNMENT         Set a tuning parameter to the given value.\n  --tuning FILE              Read size=value assignments from the given file.\n";
    
    while ((ch = getopt_long(argc, argv, ":t:r:DLe:bnh", long_options, NULL)) !=
           -1) {
        if (ch == 1 || ch == 't') {
            runtime_file = fopen(optarg, "w");
            if (runtime_file == NULL)
                futhark_panic(1, "Cannot open %s: %s\n", optarg,
                              strerror(errno));
        }
        if (ch == 2 || ch == 'r') {
            num_runs = atoi(optarg);
            perform_warmup = 1;
            if (num_runs <= 0)
                futhark_panic(1, "Need a positive number of runs, not %s\n",
                              optarg);
        }
        if (ch == 3 || ch == 'D')
            futhark_context_config_set_debugging(cfg, 1);
        if (ch == 4 || ch == 'L')
            futhark_context_config_set_logging(cfg, 1);
        if (ch == 5 || ch == 'e') {
            if (entry_point != NULL)
                entry_point = optarg;
        }
        if (ch == 6 || ch == 'b')
            binary_output = 1;
        if (ch == 7 || ch == 'n')
            print_result = 0;
        if (ch == 8 || ch == 'h') {
            printf("Usage: %s [OPTION]...\nOptions:\n\n%s\nFor more information, consult the Futhark User's Guide or the man pages.\n",
                   fut_progname, option_descriptions);
            exit(0);
        }
        if (ch == 9) {
            int n = futhark_get_tuning_param_count();
            
            for (int i = 0; i < n; i++)
                printf("%s (%s)\n", futhark_get_tuning_param_name(i),
                       futhark_get_tuning_param_class(i));
            exit(0);
        }
        if (ch == 10) {
            char *name = optarg;
            char *equals = strstr(optarg, "=");
            char *value_str = equals != NULL ? equals + 1 : optarg;
            int value = atoi(value_str);
            
            if (equals != NULL) {
                *equals = 0;
                if (futhark_context_config_set_tuning_param(cfg, name,
                                                            (size_t) value) !=
                    0)
                    futhark_panic(1, "Unknown size: %s\n", name);
            } else
                futhark_panic(1, "Invalid argument for size option: %s\n",
                              optarg);
        }
        if (ch == 11) {
            char *ret = load_tuning_file(optarg, cfg, (int (*)(void *, const
                                                               char *,
                                                               size_t)) futhark_context_config_set_tuning_param);
            
            if (ret != NULL)
                futhark_panic(1, "When loading tuning from '%s': %s\n", optarg,
                              ret);
        }
        if (ch == ':')
            futhark_panic(-1, "Missing argument for option %s\n", argv[optind -
                                                                       1]);
        if (ch == '?') {
            fprintf(stderr, "Usage: %s [OPTIONS]...\nOptions:\n\n%s\n",
                    fut_progname,
                    "  -t/--write-runtime-to FILE Print the time taken to execute the program to the indicated file, an integral number of microseconds.\n  -r/--runs INT              Perform NUM runs of the program.\n  -D/--debugging             Perform possibly expensive internal correctness checks and verbose logging.\n  -L/--log                   Print various low-overhead logging information to stderr while running.\n  -e/--entry-point NAME      The entry point to run. Defaults to main.\n  -b/--binary-output         Print the program result in the binary output format.\n  -n/--no-print-result       Do not print the program result.\n  -h/--help                  Print help information and exit.\n  --print-params             Print all tuning parameters that can be set with --param or --tuning.\n  --param ASSIGNMENT         Set a tuning parameter to the given value.\n  --tuning FILE              Read size=value assignments from the given file.\n");
            futhark_panic(1, "Unknown option: %s\n", argv[optind - 1]);
        }
    }
    return optind;
}
static void futrts_cli_entry_main(struct futhark_context *ctx)
{
    int64_t t_start, t_end;
    int time_runs = 0, profile_run = 0;
    
    // We do not want to profile all the initialisation.
    futhark_context_pause_profiling(ctx);
    // Declare and read input.
    set_binary_mode(stdin);
    
    struct futhark_f32_3d * read_value_0;
    int64_t read_shape_0[3];
    float *read_arr_0 = NULL;
    
    errno = 0;
    if (read_array(stdin, &f32_info, (void **) &read_arr_0, read_shape_0, 3) !=
        0)
        futhark_panic(1, "Cannot read input #%d of type %s (errno: %s).\n", 0,
                      "[][][]f32", strerror(errno));
    
    struct futhark_f32_3d * read_value_1;
    int64_t read_shape_1[3];
    float *read_arr_1 = NULL;
    
    errno = 0;
    if (read_array(stdin, &f32_info, (void **) &read_arr_1, read_shape_1, 3) !=
        0)
        futhark_panic(1, "Cannot read input #%d of type %s (errno: %s).\n", 1,
                      "[][][]f32", strerror(errno));
    
    struct futhark_f32_3d * read_value_2;
    int64_t read_shape_2[3];
    float *read_arr_2 = NULL;
    
    errno = 0;
    if (read_array(stdin, &f32_info, (void **) &read_arr_2, read_shape_2, 3) !=
        0)
        futhark_panic(1, "Cannot read input #%d of type %s (errno: %s).\n", 2,
                      "[][][]f32", strerror(errno));
    
    struct futhark_f32_3d * read_value_3;
    int64_t read_shape_3[3];
    float *read_arr_3 = NULL;
    
    errno = 0;
    if (read_array(stdin, &f32_info, (void **) &read_arr_3, read_shape_3, 3) !=
        0)
        futhark_panic(1, "Cannot read input #%d of type %s (errno: %s).\n", 3,
                      "[][][]f32", strerror(errno));
    
    struct futhark_f32_3d * read_value_4;
    int64_t read_shape_4[3];
    float *read_arr_4 = NULL;
    
    errno = 0;
    if (read_array(stdin, &f32_info, (void **) &read_arr_4, read_shape_4, 3) !=
        0)
        futhark_panic(1, "Cannot read input #%d of type %s (errno: %s).\n", 4,
                      "[][][]f32", strerror(errno));
    
    struct futhark_f32_3d * read_value_5;
    int64_t read_shape_5[3];
    float *read_arr_5 = NULL;
    
    errno = 0;
    if (read_array(stdin, &f32_info, (void **) &read_arr_5, read_shape_5, 3) !=
        0)
        futhark_panic(1, "Cannot read input #%d of type %s (errno: %s).\n", 5,
                      "[][][]f32", strerror(errno));
    
    struct futhark_f32_3d * read_value_6;
    int64_t read_shape_6[3];
    float *read_arr_6 = NULL;
    
    errno = 0;
    if (read_array(stdin, &f32_info, (void **) &read_arr_6, read_shape_6, 3) !=
        0)
        futhark_panic(1, "Cannot read input #%d of type %s (errno: %s).\n", 6,
                      "[][][]f32", strerror(errno));
    
    struct futhark_f32_3d * read_value_7;
    int64_t read_shape_7[3];
    float *read_arr_7 = NULL;
    
    errno = 0;
    if (read_array(stdin, &f32_info, (void **) &read_arr_7, read_shape_7, 3) !=
        0)
        futhark_panic(1, "Cannot read input #%d of type %s (errno: %s).\n", 7,
                      "[][][]f32", strerror(errno));
    
    struct futhark_f32_3d * read_value_8;
    int64_t read_shape_8[3];
    float *read_arr_8 = NULL;
    
    errno = 0;
    if (read_array(stdin, &f32_info, (void **) &read_arr_8, read_shape_8, 3) !=
        0)
        futhark_panic(1, "Cannot read input #%d of type %s (errno: %s).\n", 8,
                      "[][][]f32", strerror(errno));
    
    struct futhark_f32_3d * read_value_9;
    int64_t read_shape_9[3];
    float *read_arr_9 = NULL;
    
    errno = 0;
    if (read_array(stdin, &f32_info, (void **) &read_arr_9, read_shape_9, 3) !=
        0)
        futhark_panic(1, "Cannot read input #%d of type %s (errno: %s).\n", 9,
                      "[][][]f32", strerror(errno));
    
    struct futhark_f32_3d * read_value_10;
    int64_t read_shape_10[3];
    float *read_arr_10 = NULL;
    
    errno = 0;
    if (read_array(stdin, &f32_info, (void **) &read_arr_10, read_shape_10,
                   3) != 0)
        futhark_panic(1, "Cannot read input #%d of type %s (errno: %s).\n", 10,
                      "[][][]f32", strerror(errno));
    
    struct futhark_f32_3d * read_value_11;
    int64_t read_shape_11[3];
    float *read_arr_11 = NULL;
    
    errno = 0;
    if (read_array(stdin, &f32_info, (void **) &read_arr_11, read_shape_11,
                   3) != 0)
        futhark_panic(1, "Cannot read input #%d of type %s (errno: %s).\n", 11,
                      "[][][]f32", strerror(errno));
    
    struct futhark_f32_1d * read_value_12;
    int64_t read_shape_12[1];
    float *read_arr_12 = NULL;
    
    errno = 0;
    if (read_array(stdin, &f32_info, (void **) &read_arr_12, read_shape_12,
                   1) != 0)
        futhark_panic(1, "Cannot read input #%d of type %s (errno: %s).\n", 12,
                      "[]f32", strerror(errno));
    
    struct futhark_f32_1d * read_value_13;
    int64_t read_shape_13[1];
    float *read_arr_13 = NULL;
    
    errno = 0;
    if (read_array(stdin, &f32_info, (void **) &read_arr_13, read_shape_13,
                   1) != 0)
        futhark_panic(1, "Cannot read input #%d of type %s (errno: %s).\n", 13,
                      "[]f32", strerror(errno));
    
    struct futhark_f32_1d * read_value_14;
    int64_t read_shape_14[1];
    float *read_arr_14 = NULL;
    
    errno = 0;
    if (read_array(stdin, &f32_info, (void **) &read_arr_14, read_shape_14,
                   1) != 0)
        futhark_panic(1, "Cannot read input #%d of type %s (errno: %s).\n", 14,
                      "[]f32", strerror(errno));
    
    struct futhark_f32_1d * read_value_15;
    int64_t read_shape_15[1];
    float *read_arr_15 = NULL;
    
    errno = 0;
    if (read_array(stdin, &f32_info, (void **) &read_arr_15, read_shape_15,
                   1) != 0)
        futhark_panic(1, "Cannot read input #%d of type %s (errno: %s).\n", 15,
                      "[]f32", strerror(errno));
    
    struct futhark_f32_1d * read_value_16;
    int64_t read_shape_16[1];
    float *read_arr_16 = NULL;
    
    errno = 0;
    if (read_array(stdin, &f32_info, (void **) &read_arr_16, read_shape_16,
                   1) != 0)
        futhark_panic(1, "Cannot read input #%d of type %s (errno: %s).\n", 16,
                      "[]f32", strerror(errno));
    
    struct futhark_f32_1d * read_value_17;
    int64_t read_shape_17[1];
    float *read_arr_17 = NULL;
    
    errno = 0;
    if (read_array(stdin, &f32_info, (void **) &read_arr_17, read_shape_17,
                   1) != 0)
        futhark_panic(1, "Cannot read input #%d of type %s (errno: %s).\n", 17,
                      "[]f32", strerror(errno));
    
    struct futhark_f32_1d * read_value_18;
    int64_t read_shape_18[1];
    float *read_arr_18 = NULL;
    
    errno = 0;
    if (read_array(stdin, &f32_info, (void **) &read_arr_18, read_shape_18,
                   1) != 0)
        futhark_panic(1, "Cannot read input #%d of type %s (errno: %s).\n", 18,
                      "[]f32", strerror(errno));
    
    struct futhark_f32_1d * read_value_19;
    int64_t read_shape_19[1];
    float *read_arr_19 = NULL;
    
    errno = 0;
    if (read_array(stdin, &f32_info, (void **) &read_arr_19, read_shape_19,
                   1) != 0)
        futhark_panic(1, "Cannot read input #%d of type %s (errno: %s).\n", 19,
                      "[]f32", strerror(errno));
    
    struct futhark_i32_2d * read_value_20;
    int64_t read_shape_20[2];
    int32_t *read_arr_20 = NULL;
    
    errno = 0;
    if (read_array(stdin, &i32_info, (void **) &read_arr_20, read_shape_20,
                   2) != 0)
        futhark_panic(1, "Cannot read input #%d of type %s (errno: %s).\n", 20,
                      "[][]i32", strerror(errno));
    
    struct futhark_f32_3d * read_value_21;
    int64_t read_shape_21[3];
    float *read_arr_21 = NULL;
    
    errno = 0;
    if (read_array(stdin, &f32_info, (void **) &read_arr_21, read_shape_21,
                   3) != 0)
        futhark_panic(1, "Cannot read input #%d of type %s (errno: %s).\n", 21,
                      "[][][]f32", strerror(errno));
    
    struct futhark_f32_3d * read_value_22;
    int64_t read_shape_22[3];
    float *read_arr_22 = NULL;
    
    errno = 0;
    if (read_array(stdin, &f32_info, (void **) &read_arr_22, read_shape_22,
                   3) != 0)
        futhark_panic(1, "Cannot read input #%d of type %s (errno: %s).\n", 22,
                      "[][][]f32", strerror(errno));
    
    struct futhark_f32_3d * read_value_23;
    int64_t read_shape_23[3];
    float *read_arr_23 = NULL;
    
    errno = 0;
    if (read_array(stdin, &f32_info, (void **) &read_arr_23, read_shape_23,
                   3) != 0)
        futhark_panic(1, "Cannot read input #%d of type %s (errno: %s).\n", 23,
                      "[][][]f32", strerror(errno));
    
    struct futhark_f32_2d * read_value_24;
    int64_t read_shape_24[2];
    float *read_arr_24 = NULL;
    
    errno = 0;
    if (read_array(stdin, &f32_info, (void **) &read_arr_24, read_shape_24,
                   2) != 0)
        futhark_panic(1, "Cannot read input #%d of type %s (errno: %s).\n", 24,
                      "[][]f32", strerror(errno));
    if (end_of_input(stdin) != 0)
        futhark_panic(1,
                      "Expected EOF on stdin after reading input for \"%s\".\n",
                      "main");
    
    struct futhark_f32_3d * result_0;
    struct futhark_f32_3d * result_1;
    struct futhark_f32_3d * result_2;
    struct futhark_f32_3d * result_3;
    struct futhark_f32_3d * result_4;
    struct futhark_f32_3d * result_5;
    struct futhark_f32_2d * result_6;
    
    if (perform_warmup) {
        int r;
        
        assert((read_value_0 = futhark_new_f32_3d(ctx, read_arr_0,
                                                  read_shape_0[0],
                                                  read_shape_0[1],
                                                  read_shape_0[2])) != NULL);
        assert((read_value_1 = futhark_new_f32_3d(ctx, read_arr_1,
                                                  read_shape_1[0],
                                                  read_shape_1[1],
                                                  read_shape_1[2])) != NULL);
        assert((read_value_2 = futhark_new_f32_3d(ctx, read_arr_2,
                                                  read_shape_2[0],
                                                  read_shape_2[1],
                                                  read_shape_2[2])) != NULL);
        assert((read_value_3 = futhark_new_f32_3d(ctx, read_arr_3,
                                                  read_shape_3[0],
                                                  read_shape_3[1],
                                                  read_shape_3[2])) != NULL);
        assert((read_value_4 = futhark_new_f32_3d(ctx, read_arr_4,
                                                  read_shape_4[0],
                                                  read_shape_4[1],
                                                  read_shape_4[2])) != NULL);
        assert((read_value_5 = futhark_new_f32_3d(ctx, read_arr_5,
                                                  read_shape_5[0],
                                                  read_shape_5[1],
                                                  read_shape_5[2])) != NULL);
        assert((read_value_6 = futhark_new_f32_3d(ctx, read_arr_6,
                                                  read_shape_6[0],
                                                  read_shape_6[1],
                                                  read_shape_6[2])) != NULL);
        assert((read_value_7 = futhark_new_f32_3d(ctx, read_arr_7,
                                                  read_shape_7[0],
                                                  read_shape_7[1],
                                                  read_shape_7[2])) != NULL);
        assert((read_value_8 = futhark_new_f32_3d(ctx, read_arr_8,
                                                  read_shape_8[0],
                                                  read_shape_8[1],
                                                  read_shape_8[2])) != NULL);
        assert((read_value_9 = futhark_new_f32_3d(ctx, read_arr_9,
                                                  read_shape_9[0],
                                                  read_shape_9[1],
                                                  read_shape_9[2])) != NULL);
        assert((read_value_10 = futhark_new_f32_3d(ctx, read_arr_10,
                                                   read_shape_10[0],
                                                   read_shape_10[1],
                                                   read_shape_10[2])) != NULL);
        assert((read_value_11 = futhark_new_f32_3d(ctx, read_arr_11,
                                                   read_shape_11[0],
                                                   read_shape_11[1],
                                                   read_shape_11[2])) != NULL);
        assert((read_value_12 = futhark_new_f32_1d(ctx, read_arr_12,
                                                   read_shape_12[0])) != NULL);
        assert((read_value_13 = futhark_new_f32_1d(ctx, read_arr_13,
                                                   read_shape_13[0])) != NULL);
        assert((read_value_14 = futhark_new_f32_1d(ctx, read_arr_14,
                                                   read_shape_14[0])) != NULL);
        assert((read_value_15 = futhark_new_f32_1d(ctx, read_arr_15,
                                                   read_shape_15[0])) != NULL);
        assert((read_value_16 = futhark_new_f32_1d(ctx, read_arr_16,
                                                   read_shape_16[0])) != NULL);
        assert((read_value_17 = futhark_new_f32_1d(ctx, read_arr_17,
                                                   read_shape_17[0])) != NULL);
        assert((read_value_18 = futhark_new_f32_1d(ctx, read_arr_18,
                                                   read_shape_18[0])) != NULL);
        assert((read_value_19 = futhark_new_f32_1d(ctx, read_arr_19,
                                                   read_shape_19[0])) != NULL);
        assert((read_value_20 = futhark_new_i32_2d(ctx, read_arr_20,
                                                   read_shape_20[0],
                                                   read_shape_20[1])) != NULL);
        assert((read_value_21 = futhark_new_f32_3d(ctx, read_arr_21,
                                                   read_shape_21[0],
                                                   read_shape_21[1],
                                                   read_shape_21[2])) != NULL);
        assert((read_value_22 = futhark_new_f32_3d(ctx, read_arr_22,
                                                   read_shape_22[0],
                                                   read_shape_22[1],
                                                   read_shape_22[2])) != NULL);
        assert((read_value_23 = futhark_new_f32_3d(ctx, read_arr_23,
                                                   read_shape_23[0],
                                                   read_shape_23[1],
                                                   read_shape_23[2])) != NULL);
        assert((read_value_24 = futhark_new_f32_2d(ctx, read_arr_24,
                                                   read_shape_24[0],
                                                   read_shape_24[1])) != NULL);
        if (futhark_context_sync(ctx) != 0)
            futhark_panic(1, "%s", futhark_context_get_error(ctx));
        ;
        // Only profile last run.
        if (profile_run)
            futhark_context_unpause_profiling(ctx);
        t_start = get_wall_time();
        r = futhark_entry_main(ctx, &result_0, &result_1, &result_2, &result_3,
                               &result_4, &result_5, &result_6, read_value_0,
                               read_value_1, read_value_2, read_value_3,
                               read_value_4, read_value_5, read_value_6,
                               read_value_7, read_value_8, read_value_9,
                               read_value_10, read_value_11, read_value_12,
                               read_value_13, read_value_14, read_value_15,
                               read_value_16, read_value_17, read_value_18,
                               read_value_19, read_value_20, read_value_21,
                               read_value_22, read_value_23, read_value_24);
        if (r != 0)
            futhark_panic(1, "%s", futhark_context_get_error(ctx));
        if (futhark_context_sync(ctx) != 0)
            futhark_panic(1, "%s", futhark_context_get_error(ctx));
        ;
        if (profile_run)
            futhark_context_pause_profiling(ctx);
        t_end = get_wall_time();
        
        long elapsed_usec = t_end - t_start;
        
        if (time_runs && runtime_file != NULL) {
            fprintf(runtime_file, "%lld\n", (long long) elapsed_usec);
            fflush(runtime_file);
        }
        assert(futhark_free_f32_3d(ctx, read_value_0) == 0);
        assert(futhark_free_f32_3d(ctx, read_value_1) == 0);
        assert(futhark_free_f32_3d(ctx, read_value_2) == 0);
        assert(futhark_free_f32_3d(ctx, read_value_3) == 0);
        assert(futhark_free_f32_3d(ctx, read_value_4) == 0);
        assert(futhark_free_f32_3d(ctx, read_value_5) == 0);
        assert(futhark_free_f32_3d(ctx, read_value_6) == 0);
        assert(futhark_free_f32_3d(ctx, read_value_7) == 0);
        assert(futhark_free_f32_3d(ctx, read_value_8) == 0);
        assert(futhark_free_f32_3d(ctx, read_value_9) == 0);
        assert(futhark_free_f32_3d(ctx, read_value_10) == 0);
        assert(futhark_free_f32_3d(ctx, read_value_11) == 0);
        assert(futhark_free_f32_1d(ctx, read_value_12) == 0);
        assert(futhark_free_f32_1d(ctx, read_value_13) == 0);
        assert(futhark_free_f32_1d(ctx, read_value_14) == 0);
        assert(futhark_free_f32_1d(ctx, read_value_15) == 0);
        assert(futhark_free_f32_1d(ctx, read_value_16) == 0);
        assert(futhark_free_f32_1d(ctx, read_value_17) == 0);
        assert(futhark_free_f32_1d(ctx, read_value_18) == 0);
        assert(futhark_free_f32_1d(ctx, read_value_19) == 0);
        assert(futhark_free_i32_2d(ctx, read_value_20) == 0);
        assert(futhark_free_f32_3d(ctx, read_value_21) == 0);
        assert(futhark_free_f32_3d(ctx, read_value_22) == 0);
        assert(futhark_free_f32_3d(ctx, read_value_23) == 0);
        assert(futhark_free_f32_2d(ctx, read_value_24) == 0);
        assert(futhark_free_f32_3d(ctx, result_0) == 0);
        assert(futhark_free_f32_3d(ctx, result_1) == 0);
        assert(futhark_free_f32_3d(ctx, result_2) == 0);
        assert(futhark_free_f32_3d(ctx, result_3) == 0);
        assert(futhark_free_f32_3d(ctx, result_4) == 0);
        assert(futhark_free_f32_3d(ctx, result_5) == 0);
        assert(futhark_free_f32_2d(ctx, result_6) == 0);
    }
    time_runs = 1;
    // Proper run.
    for (int run = 0; run < num_runs; run++) {
        // Only profile last run.
        profile_run = run == num_runs - 1;
        
        int r;
        
        assert((read_value_0 = futhark_new_f32_3d(ctx, read_arr_0,
                                                  read_shape_0[0],
                                                  read_shape_0[1],
                                                  read_shape_0[2])) != NULL);
        assert((read_value_1 = futhark_new_f32_3d(ctx, read_arr_1,
                                                  read_shape_1[0],
                                                  read_shape_1[1],
                                                  read_shape_1[2])) != NULL);
        assert((read_value_2 = futhark_new_f32_3d(ctx, read_arr_2,
                                                  read_shape_2[0],
                                                  read_shape_2[1],
                                                  read_shape_2[2])) != NULL);
        assert((read_value_3 = futhark_new_f32_3d(ctx, read_arr_3,
                                                  read_shape_3[0],
                                                  read_shape_3[1],
                                                  read_shape_3[2])) != NULL);
        assert((read_value_4 = futhark_new_f32_3d(ctx, read_arr_4,
                                                  read_shape_4[0],
                                                  read_shape_4[1],
                                                  read_shape_4[2])) != NULL);
        assert((read_value_5 = futhark_new_f32_3d(ctx, read_arr_5,
                                                  read_shape_5[0],
                                                  read_shape_5[1],
                                                  read_shape_5[2])) != NULL);
        assert((read_value_6 = futhark_new_f32_3d(ctx, read_arr_6,
                                                  read_shape_6[0],
                                                  read_shape_6[1],
                                                  read_shape_6[2])) != NULL);
        assert((read_value_7 = futhark_new_f32_3d(ctx, read_arr_7,
                                                  read_shape_7[0],
                                                  read_shape_7[1],
                                                  read_shape_7[2])) != NULL);
        assert((read_value_8 = futhark_new_f32_3d(ctx, read_arr_8,
                                                  read_shape_8[0],
                                                  read_shape_8[1],
                                                  read_shape_8[2])) != NULL);
        assert((read_value_9 = futhark_new_f32_3d(ctx, read_arr_9,
                                                  read_shape_9[0],
                                                  read_shape_9[1],
                                                  read_shape_9[2])) != NULL);
        assert((read_value_10 = futhark_new_f32_3d(ctx, read_arr_10,
                                                   read_shape_10[0],
                                                   read_shape_10[1],
                                                   read_shape_10[2])) != NULL);
        assert((read_value_11 = futhark_new_f32_3d(ctx, read_arr_11,
                                                   read_shape_11[0],
                                                   read_shape_11[1],
                                                   read_shape_11[2])) != NULL);
        assert((read_value_12 = futhark_new_f32_1d(ctx, read_arr_12,
                                                   read_shape_12[0])) != NULL);
        assert((read_value_13 = futhark_new_f32_1d(ctx, read_arr_13,
                                                   read_shape_13[0])) != NULL);
        assert((read_value_14 = futhark_new_f32_1d(ctx, read_arr_14,
                                                   read_shape_14[0])) != NULL);
        assert((read_value_15 = futhark_new_f32_1d(ctx, read_arr_15,
                                                   read_shape_15[0])) != NULL);
        assert((read_value_16 = futhark_new_f32_1d(ctx, read_arr_16,
                                                   read_shape_16[0])) != NULL);
        assert((read_value_17 = futhark_new_f32_1d(ctx, read_arr_17,
                                                   read_shape_17[0])) != NULL);
        assert((read_value_18 = futhark_new_f32_1d(ctx, read_arr_18,
                                                   read_shape_18[0])) != NULL);
        assert((read_value_19 = futhark_new_f32_1d(ctx, read_arr_19,
                                                   read_shape_19[0])) != NULL);
        assert((read_value_20 = futhark_new_i32_2d(ctx, read_arr_20,
                                                   read_shape_20[0],
                                                   read_shape_20[1])) != NULL);
        assert((read_value_21 = futhark_new_f32_3d(ctx, read_arr_21,
                                                   read_shape_21[0],
                                                   read_shape_21[1],
                                                   read_shape_21[2])) != NULL);
        assert((read_value_22 = futhark_new_f32_3d(ctx, read_arr_22,
                                                   read_shape_22[0],
                                                   read_shape_22[1],
                                                   read_shape_22[2])) != NULL);
        assert((read_value_23 = futhark_new_f32_3d(ctx, read_arr_23,
                                                   read_shape_23[0],
                                                   read_shape_23[1],
                                                   read_shape_23[2])) != NULL);
        assert((read_value_24 = futhark_new_f32_2d(ctx, read_arr_24,
                                                   read_shape_24[0],
                                                   read_shape_24[1])) != NULL);
        if (futhark_context_sync(ctx) != 0)
            futhark_panic(1, "%s", futhark_context_get_error(ctx));
        ;
        // Only profile last run.
        if (profile_run)
            futhark_context_unpause_profiling(ctx);
        t_start = get_wall_time();
        r = futhark_entry_main(ctx, &result_0, &result_1, &result_2, &result_3,
                               &result_4, &result_5, &result_6, read_value_0,
                               read_value_1, read_value_2, read_value_3,
                               read_value_4, read_value_5, read_value_6,
                               read_value_7, read_value_8, read_value_9,
                               read_value_10, read_value_11, read_value_12,
                               read_value_13, read_value_14, read_value_15,
                               read_value_16, read_value_17, read_value_18,
                               read_value_19, read_value_20, read_value_21,
                               read_value_22, read_value_23, read_value_24);
        if (r != 0)
            futhark_panic(1, "%s", futhark_context_get_error(ctx));
        if (futhark_context_sync(ctx) != 0)
            futhark_panic(1, "%s", futhark_context_get_error(ctx));
        ;
        if (profile_run)
            futhark_context_pause_profiling(ctx);
        t_end = get_wall_time();
        
        long elapsed_usec = t_end - t_start;
        
        if (time_runs && runtime_file != NULL) {
            fprintf(runtime_file, "%lld\n", (long long) elapsed_usec);
            fflush(runtime_file);
        }
        assert(futhark_free_f32_3d(ctx, read_value_0) == 0);
        assert(futhark_free_f32_3d(ctx, read_value_1) == 0);
        assert(futhark_free_f32_3d(ctx, read_value_2) == 0);
        assert(futhark_free_f32_3d(ctx, read_value_3) == 0);
        assert(futhark_free_f32_3d(ctx, read_value_4) == 0);
        assert(futhark_free_f32_3d(ctx, read_value_5) == 0);
        assert(futhark_free_f32_3d(ctx, read_value_6) == 0);
        assert(futhark_free_f32_3d(ctx, read_value_7) == 0);
        assert(futhark_free_f32_3d(ctx, read_value_8) == 0);
        assert(futhark_free_f32_3d(ctx, read_value_9) == 0);
        assert(futhark_free_f32_3d(ctx, read_value_10) == 0);
        assert(futhark_free_f32_3d(ctx, read_value_11) == 0);
        assert(futhark_free_f32_1d(ctx, read_value_12) == 0);
        assert(futhark_free_f32_1d(ctx, read_value_13) == 0);
        assert(futhark_free_f32_1d(ctx, read_value_14) == 0);
        assert(futhark_free_f32_1d(ctx, read_value_15) == 0);
        assert(futhark_free_f32_1d(ctx, read_value_16) == 0);
        assert(futhark_free_f32_1d(ctx, read_value_17) == 0);
        assert(futhark_free_f32_1d(ctx, read_value_18) == 0);
        assert(futhark_free_f32_1d(ctx, read_value_19) == 0);
        assert(futhark_free_i32_2d(ctx, read_value_20) == 0);
        assert(futhark_free_f32_3d(ctx, read_value_21) == 0);
        assert(futhark_free_f32_3d(ctx, read_value_22) == 0);
        assert(futhark_free_f32_3d(ctx, read_value_23) == 0);
        assert(futhark_free_f32_2d(ctx, read_value_24) == 0);
        if (run < num_runs - 1) {
            assert(futhark_free_f32_3d(ctx, result_0) == 0);
            assert(futhark_free_f32_3d(ctx, result_1) == 0);
            assert(futhark_free_f32_3d(ctx, result_2) == 0);
            assert(futhark_free_f32_3d(ctx, result_3) == 0);
            assert(futhark_free_f32_3d(ctx, result_4) == 0);
            assert(futhark_free_f32_3d(ctx, result_5) == 0);
            assert(futhark_free_f32_2d(ctx, result_6) == 0);
        }
    }
    free(read_arr_0);
    free(read_arr_1);
    free(read_arr_2);
    free(read_arr_3);
    free(read_arr_4);
    free(read_arr_5);
    free(read_arr_6);
    free(read_arr_7);
    free(read_arr_8);
    free(read_arr_9);
    free(read_arr_10);
    free(read_arr_11);
    free(read_arr_12);
    free(read_arr_13);
    free(read_arr_14);
    free(read_arr_15);
    free(read_arr_16);
    free(read_arr_17);
    free(read_arr_18);
    free(read_arr_19);
    free(read_arr_20);
    free(read_arr_21);
    free(read_arr_22);
    free(read_arr_23);
    free(read_arr_24);
    if (print_result) {
        // Print the final result.
        if (binary_output)
            set_binary_mode(stdout);
        {
            float *arr = calloc(futhark_shape_f32_3d(ctx, result_0)[0] *
                                futhark_shape_f32_3d(ctx, result_0)[1] *
                                futhark_shape_f32_3d(ctx, result_0)[2],
                                f32_info.size);
            
            assert(arr != NULL);
            assert(futhark_values_f32_3d(ctx, result_0, arr) == 0);
            write_array(stdout, binary_output, &f32_info, arr,
                        futhark_shape_f32_3d(ctx, result_0), 3);
            free(arr);
        }
        printf("\n");
        {
            float *arr = calloc(futhark_shape_f32_3d(ctx, result_1)[0] *
                                futhark_shape_f32_3d(ctx, result_1)[1] *
                                futhark_shape_f32_3d(ctx, result_1)[2],
                                f32_info.size);
            
            assert(arr != NULL);
            assert(futhark_values_f32_3d(ctx, result_1, arr) == 0);
            write_array(stdout, binary_output, &f32_info, arr,
                        futhark_shape_f32_3d(ctx, result_1), 3);
            free(arr);
        }
        printf("\n");
        {
            float *arr = calloc(futhark_shape_f32_3d(ctx, result_2)[0] *
                                futhark_shape_f32_3d(ctx, result_2)[1] *
                                futhark_shape_f32_3d(ctx, result_2)[2],
                                f32_info.size);
            
            assert(arr != NULL);
            assert(futhark_values_f32_3d(ctx, result_2, arr) == 0);
            write_array(stdout, binary_output, &f32_info, arr,
                        futhark_shape_f32_3d(ctx, result_2), 3);
            free(arr);
        }
        printf("\n");
        {
            float *arr = calloc(futhark_shape_f32_3d(ctx, result_3)[0] *
                                futhark_shape_f32_3d(ctx, result_3)[1] *
                                futhark_shape_f32_3d(ctx, result_3)[2],
                                f32_info.size);
            
            assert(arr != NULL);
            assert(futhark_values_f32_3d(ctx, result_3, arr) == 0);
            write_array(stdout, binary_output, &f32_info, arr,
                        futhark_shape_f32_3d(ctx, result_3), 3);
            free(arr);
        }
        printf("\n");
        {
            float *arr = calloc(futhark_shape_f32_3d(ctx, result_4)[0] *
                                futhark_shape_f32_3d(ctx, result_4)[1] *
                                futhark_shape_f32_3d(ctx, result_4)[2],
                                f32_info.size);
            
            assert(arr != NULL);
            assert(futhark_values_f32_3d(ctx, result_4, arr) == 0);
            write_array(stdout, binary_output, &f32_info, arr,
                        futhark_shape_f32_3d(ctx, result_4), 3);
            free(arr);
        }
        printf("\n");
        {
            float *arr = calloc(futhark_shape_f32_3d(ctx, result_5)[0] *
                                futhark_shape_f32_3d(ctx, result_5)[1] *
                                futhark_shape_f32_3d(ctx, result_5)[2],
                                f32_info.size);
            
            assert(arr != NULL);
            assert(futhark_values_f32_3d(ctx, result_5, arr) == 0);
            write_array(stdout, binary_output, &f32_info, arr,
                        futhark_shape_f32_3d(ctx, result_5), 3);
            free(arr);
        }
        printf("\n");
        {
            float *arr = calloc(futhark_shape_f32_2d(ctx, result_6)[0] *
                                futhark_shape_f32_2d(ctx, result_6)[1],
                                f32_info.size);
            
            assert(arr != NULL);
            assert(futhark_values_f32_2d(ctx, result_6, arr) == 0);
            write_array(stdout, binary_output, &f32_info, arr,
                        futhark_shape_f32_2d(ctx, result_6), 2);
            free(arr);
        }
        printf("\n");
    }
    assert(futhark_free_f32_3d(ctx, result_0) == 0);
    assert(futhark_free_f32_3d(ctx, result_1) == 0);
    assert(futhark_free_f32_3d(ctx, result_2) == 0);
    assert(futhark_free_f32_3d(ctx, result_3) == 0);
    assert(futhark_free_f32_3d(ctx, result_4) == 0);
    assert(futhark_free_f32_3d(ctx, result_5) == 0);
    assert(futhark_free_f32_2d(ctx, result_6) == 0);
}
typedef void entry_point_fun(struct futhark_context *);
struct entry_point_entry {
    const char *name;
    entry_point_fun *fun;
};
int main(int argc, char **argv)
{
    fut_progname = argv[0];
    
    struct futhark_context_config *cfg = futhark_context_config_new();
    
    assert(cfg != NULL);
    
    int parsed_options = parse_options(cfg, argc, argv);
    
    argc -= parsed_options;
    argv += parsed_options;
    if (argc != 0)
        futhark_panic(1, "Excess non-option: %s\n", argv[0]);
    
    struct futhark_context *ctx = futhark_context_new(cfg);
    
    assert(ctx != NULL);
    
    char *error = futhark_context_get_error(ctx);
    
    if (error != NULL)
        futhark_panic(1, "%s", error);
    
    struct entry_point_entry entry_points[] = {{.name ="main", .fun =
                                                futrts_cli_entry_main}};
    
    if (entry_point != NULL) {
        int num_entry_points = sizeof(entry_points) / sizeof(entry_points[0]);
        entry_point_fun *entry_point_fun = NULL;
        
        for (int i = 0; i < num_entry_points; i++) {
            if (strcmp(entry_points[i].name, entry_point) == 0) {
                entry_point_fun = entry_points[i].fun;
                break;
            }
        }
        if (entry_point_fun == NULL) {
            fprintf(stderr,
                    "No entry point '%s'.  Select another with --entry-point.  Options are:\n",
                    entry_point);
            for (int i = 0; i < num_entry_points; i++)
                fprintf(stderr, "%s\n", entry_points[i].name);
            return 1;
        }
        if (isatty(fileno(stdin))) {
            fprintf(stderr, "Reading input from TTY.\n");
            fprintf(stderr,
                    "Send EOF (CTRL-d) after typing all input values.\n");
        }
        entry_point_fun(ctx);
        if (runtime_file != NULL)
            fclose(runtime_file);
        
        char *report = futhark_context_report(ctx);
        
        fputs(report, stderr);
        free(report);
    }
    futhark_context_free(ctx);
    futhark_context_config_free(cfg);
    return 0;
}

#ifdef _MSC_VER
#define inline __inline
#endif
#include <string.h>
#include <string.h>
#include <errno.h>
#include <assert.h>
#include <ctype.h>



// Start of lock.h.

// A very simple cross-platform implementation of locks.  Uses
// pthreads on Unix and some Windows thing there.  Futhark's
// host-level code is not multithreaded, but user code may be, so we
// need some mechanism for ensuring atomic access to API functions.
// This is that mechanism.  It is not exposed to user code at all, so
// we do not have to worry about name collisions.

#ifdef _WIN32

typedef HANDLE lock_t;

static void create_lock(lock_t *lock) {
  *lock = CreateMutex(NULL,  // Default security attributes.
                      FALSE, // Initially unlocked.
                      NULL); // Unnamed.
}

static void lock_lock(lock_t *lock) {
  assert(WaitForSingleObject(*lock, INFINITE) == WAIT_OBJECT_0);
}

static void lock_unlock(lock_t *lock) {
  assert(ReleaseMutex(*lock));
}

static void free_lock(lock_t *lock) {
  CloseHandle(*lock);
}

#else
// Assuming POSIX

#include <pthread.h>

typedef pthread_mutex_t lock_t;

static void create_lock(lock_t *lock) {
  int r = pthread_mutex_init(lock, NULL);
  assert(r == 0);
}

static void lock_lock(lock_t *lock) {
  int r = pthread_mutex_lock(lock);
  assert(r == 0);
}

static void lock_unlock(lock_t *lock) {
  int r = pthread_mutex_unlock(lock);
  assert(r == 0);
}

static void free_lock(lock_t *lock) {
  // Nothing to do for pthreads.
  (void)lock;
}

#endif

// End of lock.h.

#define FUTHARK_F64_ENABLED

// Start of scalar.h.

// Implementation of the primitive scalar operations.  Very
// repetitive.  This code is inserted directly into both CUDA and
// OpenCL programs, as well as the CPU code, so it has some #ifdefs to
// work everywhere.  Some operations are defined as macros because
// this allows us to use them as constant expressions in things like
// array sizes and static initialisers.

// Some of the #ifdefs are because OpenCL uses type-generic functions
// for some operations (e.g. sqrt), while C and CUDA sensibly use
// distinct functions for different precisions (e.g. sqrtf() and
// sqrt()).  This is quite annoying.  Due to C's unfortunate casting
// rules, it is also really easy to accidentally implement
// floating-point functions in the wrong precision, so be careful.

// Double-precision definitions are only included if the preprocessor
// macro FUTHARK_F64_ENABLED is set.

static inline uint8_t add8(uint8_t x, uint8_t y) {
  return x + y;
}

static inline uint16_t add16(uint16_t x, uint16_t y) {
  return x + y;
}

static inline uint32_t add32(uint32_t x, uint32_t y) {
  return x + y;
}

static inline uint64_t add64(uint64_t x, uint64_t y) {
  return x + y;
}

static inline uint8_t sub8(uint8_t x, uint8_t y) {
  return x - y;
}

static inline uint16_t sub16(uint16_t x, uint16_t y) {
  return x - y;
}

static inline uint32_t sub32(uint32_t x, uint32_t y) {
  return x - y;
}

static inline uint64_t sub64(uint64_t x, uint64_t y) {
  return x - y;
}

static inline uint8_t mul8(uint8_t x, uint8_t y) {
  return x * y;
}

static inline uint16_t mul16(uint16_t x, uint16_t y) {
  return x * y;
}

static inline uint32_t mul32(uint32_t x, uint32_t y) {
  return x * y;
}

static inline uint64_t mul64(uint64_t x, uint64_t y) {
  return x * y;
}

static inline uint8_t udiv8(uint8_t x, uint8_t y) {
  return x / y;
}

static inline uint16_t udiv16(uint16_t x, uint16_t y) {
  return x / y;
}

static inline uint32_t udiv32(uint32_t x, uint32_t y) {
  return x / y;
}

static inline uint64_t udiv64(uint64_t x, uint64_t y) {
  return x / y;
}

static inline uint8_t udiv_up8(uint8_t x, uint8_t y) {
  return (x + y - 1) / y;
}

static inline uint16_t udiv_up16(uint16_t x, uint16_t y) {
  return (x + y - 1) / y;
}

static inline uint32_t udiv_up32(uint32_t x, uint32_t y) {
  return (x + y - 1) / y;
}

static inline uint64_t udiv_up64(uint64_t x, uint64_t y) {
  return (x + y - 1) / y;
}

static inline uint8_t umod8(uint8_t x, uint8_t y) {
  return x % y;
}

static inline uint16_t umod16(uint16_t x, uint16_t y) {
  return x % y;
}

static inline uint32_t umod32(uint32_t x, uint32_t y) {
  return x % y;
}

static inline uint64_t umod64(uint64_t x, uint64_t y) {
  return x % y;
}

static inline uint8_t udiv_safe8(uint8_t x, uint8_t y) {
  return y == 0 ? 0 : x / y;
}

static inline uint16_t udiv_safe16(uint16_t x, uint16_t y) {
  return y == 0 ? 0 : x / y;
}

static inline uint32_t udiv_safe32(uint32_t x, uint32_t y) {
  return y == 0 ? 0 : x / y;
}

static inline uint64_t udiv_safe64(uint64_t x, uint64_t y) {
  return y == 0 ? 0 : x / y;
}

static inline uint8_t udiv_up_safe8(uint8_t x, uint8_t y) {
  return y == 0 ? 0 : (x + y - 1) / y;
}

static inline uint16_t udiv_up_safe16(uint16_t x, uint16_t y) {
  return y == 0 ? 0 : (x + y - 1) / y;
}

static inline uint32_t udiv_up_safe32(uint32_t x, uint32_t y) {
  return y == 0 ? 0 : (x + y - 1) / y;
}

static inline uint64_t udiv_up_safe64(uint64_t x, uint64_t y) {
  return y == 0 ? 0 : (x + y - 1) / y;
}

static inline uint8_t umod_safe8(uint8_t x, uint8_t y) {
  return y == 0 ? 0 : x % y;
}

static inline uint16_t umod_safe16(uint16_t x, uint16_t y) {
  return y == 0 ? 0 : x % y;
}

static inline uint32_t umod_safe32(uint32_t x, uint32_t y) {
  return y == 0 ? 0 : x % y;
}

static inline uint64_t umod_safe64(uint64_t x, uint64_t y) {
  return y == 0 ? 0 : x % y;
}

static inline int8_t sdiv8(int8_t x, int8_t y) {
  int8_t q = x / y;
  int8_t r = x % y;

  return q - ((r != 0 && r < 0 != y < 0) ? 1 : 0);
}

static inline int16_t sdiv16(int16_t x, int16_t y) {
  int16_t q = x / y;
  int16_t r = x % y;

  return q - ((r != 0 && r < 0 != y < 0) ? 1 : 0);
}

static inline int32_t sdiv32(int32_t x, int32_t y) {
  int32_t q = x / y;
  int32_t r = x % y;

  return q - ((r != 0 && r < 0 != y < 0) ? 1 : 0);
}

static inline int64_t sdiv64(int64_t x, int64_t y) {
  int64_t q = x / y;
  int64_t r = x % y;

  return q - ((r != 0 && r < 0 != y < 0) ? 1 : 0);
}

static inline int8_t sdiv_up8(int8_t x, int8_t y) {
  return sdiv8(x + y - 1, y);
}

static inline int16_t sdiv_up16(int16_t x, int16_t y) {
  return sdiv16(x + y - 1, y);
}

static inline int32_t sdiv_up32(int32_t x, int32_t y) {
  return sdiv32(x + y - 1, y);
}

static inline int64_t sdiv_up64(int64_t x, int64_t y) {
  return sdiv64(x + y - 1, y);
}

static inline int8_t smod8(int8_t x, int8_t y) {
  int8_t r = x % y;

  return r + (r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0) ? 0 : y);
}

static inline int16_t smod16(int16_t x, int16_t y) {
  int16_t r = x % y;

  return r + (r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0) ? 0 : y);
}

static inline int32_t smod32(int32_t x, int32_t y) {
  int32_t r = x % y;

  return r + (r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0) ? 0 : y);
}

static inline int64_t smod64(int64_t x, int64_t y) {
  int64_t r = x % y;

  return r + (r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0) ? 0 : y);
}

static inline int8_t sdiv_safe8(int8_t x, int8_t y) {
  return y == 0 ? 0 : sdiv8(x, y);
}

static inline int16_t sdiv_safe16(int16_t x, int16_t y) {
  return y == 0 ? 0 : sdiv16(x, y);
}

static inline int32_t sdiv_safe32(int32_t x, int32_t y) {
  return y == 0 ? 0 : sdiv32(x, y);
}

static inline int64_t sdiv_safe64(int64_t x, int64_t y) {
  return y == 0 ? 0 : sdiv64(x, y);
}

static inline int8_t sdiv_up_safe8(int8_t x, int8_t y) {
  return sdiv_safe8(x + y - 1, y);
}

static inline int16_t sdiv_up_safe16(int16_t x, int16_t y) {
  return sdiv_safe16(x + y - 1, y);
}

static inline int32_t sdiv_up_safe32(int32_t x, int32_t y) {
  return sdiv_safe32(x + y - 1, y);
}

static inline int64_t sdiv_up_safe64(int64_t x, int64_t y) {
  return sdiv_safe64(x + y - 1, y);
}

static inline int8_t smod_safe8(int8_t x, int8_t y) {
  return y == 0 ? 0 : smod8(x, y);
}

static inline int16_t smod_safe16(int16_t x, int16_t y) {
  return y == 0 ? 0 : smod16(x, y);
}

static inline int32_t smod_safe32(int32_t x, int32_t y) {
  return y == 0 ? 0 : smod32(x, y);
}

static inline int64_t smod_safe64(int64_t x, int64_t y) {
  return y == 0 ? 0 : smod64(x, y);
}

static inline int8_t squot8(int8_t x, int8_t y) {
  return x / y;
}

static inline int16_t squot16(int16_t x, int16_t y) {
  return x / y;
}

static inline int32_t squot32(int32_t x, int32_t y) {
  return x / y;
}

static inline int64_t squot64(int64_t x, int64_t y) {
  return x / y;
}

static inline int8_t srem8(int8_t x, int8_t y) {
  return x % y;
}

static inline int16_t srem16(int16_t x, int16_t y) {
  return x % y;
}

static inline int32_t srem32(int32_t x, int32_t y) {
  return x % y;
}

static inline int64_t srem64(int64_t x, int64_t y) {
  return x % y;
}

static inline int8_t squot_safe8(int8_t x, int8_t y) {
  return y == 0 ? 0 : x / y;
}

static inline int16_t squot_safe16(int16_t x, int16_t y) {
  return y == 0 ? 0 : x / y;
}

static inline int32_t squot_safe32(int32_t x, int32_t y) {
  return y == 0 ? 0 : x / y;
}

static inline int64_t squot_safe64(int64_t x, int64_t y) {
  return y == 0 ? 0 : x / y;
}

static inline int8_t srem_safe8(int8_t x, int8_t y) {
  return y == 0 ? 0 : x % y;
}

static inline int16_t srem_safe16(int16_t x, int16_t y) {
  return y == 0 ? 0 : x % y;
}

static inline int32_t srem_safe32(int32_t x, int32_t y) {
  return y == 0 ? 0 : x % y;
}

static inline int64_t srem_safe64(int64_t x, int64_t y) {
  return y == 0 ? 0 : x % y;
}

static inline int8_t smin8(int8_t x, int8_t y) {
  return x < y ? x : y;
}

static inline int16_t smin16(int16_t x, int16_t y) {
  return x < y ? x : y;
}

static inline int32_t smin32(int32_t x, int32_t y) {
  return x < y ? x : y;
}

static inline int64_t smin64(int64_t x, int64_t y) {
  return x < y ? x : y;
}

static inline uint8_t umin8(uint8_t x, uint8_t y) {
  return x < y ? x : y;
}

static inline uint16_t umin16(uint16_t x, uint16_t y) {
  return x < y ? x : y;
}

static inline uint32_t umin32(uint32_t x, uint32_t y) {
  return x < y ? x : y;
}

static inline uint64_t umin64(uint64_t x, uint64_t y) {
  return x < y ? x : y;
}

static inline int8_t smax8(int8_t x, int8_t y) {
  return x < y ? y : x;
}

static inline int16_t smax16(int16_t x, int16_t y) {
  return x < y ? y : x;
}

static inline int32_t smax32(int32_t x, int32_t y) {
  return x < y ? y : x;
}

static inline int64_t smax64(int64_t x, int64_t y) {
  return x < y ? y : x;
}

static inline uint8_t umax8(uint8_t x, uint8_t y) {
  return x < y ? y : x;
}

static inline uint16_t umax16(uint16_t x, uint16_t y) {
  return x < y ? y : x;
}

static inline uint32_t umax32(uint32_t x, uint32_t y) {
  return x < y ? y : x;
}

static inline uint64_t umax64(uint64_t x, uint64_t y) {
  return x < y ? y : x;
}

static inline uint8_t shl8(uint8_t x, uint8_t y) {
  return (uint8_t)(x << y);
}

static inline uint16_t shl16(uint16_t x, uint16_t y) {
  return (uint16_t)(x << y);
}

static inline uint32_t shl32(uint32_t x, uint32_t y) {
  return x << y;
}

static inline uint64_t shl64(uint64_t x, uint64_t y) {
  return x << y;
}

static inline uint8_t lshr8(uint8_t x, uint8_t y) {
  return x >> y;
}

static inline uint16_t lshr16(uint16_t x, uint16_t y) {
  return x >> y;
}

static inline uint32_t lshr32(uint32_t x, uint32_t y) {
  return x >> y;
}

static inline uint64_t lshr64(uint64_t x, uint64_t y) {
  return x >> y;
}

static inline int8_t ashr8(int8_t x, int8_t y) {
  return x >> y;
}

static inline int16_t ashr16(int16_t x, int16_t y) {
  return x >> y;
}

static inline int32_t ashr32(int32_t x, int32_t y) {
  return x >> y;
}

static inline int64_t ashr64(int64_t x, int64_t y) {
  return x >> y;
}

static inline uint8_t and8(uint8_t x, uint8_t y) {
  return x & y;
}

static inline uint16_t and16(uint16_t x, uint16_t y) {
  return x & y;
}

static inline uint32_t and32(uint32_t x, uint32_t y) {
  return x & y;
}

static inline uint64_t and64(uint64_t x, uint64_t y) {
  return x & y;
}

static inline uint8_t or8(uint8_t x, uint8_t y) {
  return x | y;
}

static inline uint16_t or16(uint16_t x, uint16_t y) {
  return x | y;
}

static inline uint32_t or32(uint32_t x, uint32_t y) {
  return x | y;
}

static inline uint64_t or64(uint64_t x, uint64_t y) {
  return x | y;
}

static inline uint8_t xor8(uint8_t x, uint8_t y) {
  return x ^ y;
}

static inline uint16_t xor16(uint16_t x, uint16_t y) {
  return x ^ y;
}

static inline uint32_t xor32(uint32_t x, uint32_t y) {
  return x ^ y;
}

static inline uint64_t xor64(uint64_t x, uint64_t y) {
  return x ^ y;
}

static inline bool ult8(uint8_t x, uint8_t y) {
  return x < y;
}

static inline bool ult16(uint16_t x, uint16_t y) {
  return x < y;
}

static inline bool ult32(uint32_t x, uint32_t y) {
  return x < y;
}

static inline bool ult64(uint64_t x, uint64_t y) {
  return x < y;
}

static inline bool ule8(uint8_t x, uint8_t y) {
  return x <= y;
}

static inline bool ule16(uint16_t x, uint16_t y) {
  return x <= y;
}

static inline bool ule32(uint32_t x, uint32_t y) {
  return x <= y;
}

static inline bool ule64(uint64_t x, uint64_t y) {
  return x <= y;
}

static inline bool slt8(int8_t x, int8_t y) {
  return x < y;
}

static inline bool slt16(int16_t x, int16_t y) {
  return x < y;
}

static inline bool slt32(int32_t x, int32_t y) {
  return x < y;
}

static inline bool slt64(int64_t x, int64_t y) {
  return x < y;
}

static inline bool sle8(int8_t x, int8_t y) {
  return x <= y;
}

static inline bool sle16(int16_t x, int16_t y) {
  return x <= y;
}

static inline bool sle32(int32_t x, int32_t y) {
  return x <= y;
}

static inline bool sle64(int64_t x, int64_t y) {
  return x <= y;
}

static inline uint8_t pow8(uint8_t x, uint8_t y) {
  uint8_t res = 1, rem = y;

  while (rem != 0) {
    if (rem & 1)
      res *= x;
    rem >>= 1;
    x *= x;
  }
  return res;
}

static inline uint16_t pow16(uint16_t x, uint16_t y) {
  uint16_t res = 1, rem = y;

  while (rem != 0) {
    if (rem & 1)
      res *= x;
    rem >>= 1;
    x *= x;
  }
  return res;
}

static inline uint32_t pow32(uint32_t x, uint32_t y) {
  uint32_t res = 1, rem = y;

  while (rem != 0) {
    if (rem & 1)
      res *= x;
    rem >>= 1;
    x *= x;
  }
  return res;
}

static inline uint64_t pow64(uint64_t x, uint64_t y) {
  uint64_t res = 1, rem = y;

  while (rem != 0) {
    if (rem & 1)
      res *= x;
    rem >>= 1;
    x *= x;
  }
  return res;
}

static inline bool itob_i8_bool(int8_t x) {
  return x;
}

static inline bool itob_i16_bool(int16_t x) {
  return x;
}

static inline bool itob_i32_bool(int32_t x) {
  return x;
}

static inline bool itob_i64_bool(int64_t x) {
  return x;
}

static inline int8_t btoi_bool_i8(bool x) {
  return x;
}

static inline int16_t btoi_bool_i16(bool x) {
  return x;
}

static inline int32_t btoi_bool_i32(bool x) {
  return x;
}

static inline int64_t btoi_bool_i64(bool x) {
  return x;
}

#define sext_i8_i8(x) ((int8_t) (int8_t) (x))
#define sext_i8_i16(x) ((int16_t) (int8_t) (x))
#define sext_i8_i32(x) ((int32_t) (int8_t) (x))
#define sext_i8_i64(x) ((int64_t) (int8_t) (x))
#define sext_i16_i8(x) ((int8_t) (int16_t) (x))
#define sext_i16_i16(x) ((int16_t) (int16_t) (x))
#define sext_i16_i32(x) ((int32_t) (int16_t) (x))
#define sext_i16_i64(x) ((int64_t) (int16_t) (x))
#define sext_i32_i8(x) ((int8_t) (int32_t) (x))
#define sext_i32_i16(x) ((int16_t) (int32_t) (x))
#define sext_i32_i32(x) ((int32_t) (int32_t) (x))
#define sext_i32_i64(x) ((int64_t) (int32_t) (x))
#define sext_i64_i8(x) ((int8_t) (int64_t) (x))
#define sext_i64_i16(x) ((int16_t) (int64_t) (x))
#define sext_i64_i32(x) ((int32_t) (int64_t) (x))
#define sext_i64_i64(x) ((int64_t) (int64_t) (x))
#define zext_i8_i8(x) ((int8_t) (uint8_t) (x))
#define zext_i8_i16(x) ((int16_t) (uint8_t) (x))
#define zext_i8_i32(x) ((int32_t) (uint8_t) (x))
#define zext_i8_i64(x) ((int64_t) (uint8_t) (x))
#define zext_i16_i8(x) ((int8_t) (uint16_t) (x))
#define zext_i16_i16(x) ((int16_t) (uint16_t) (x))
#define zext_i16_i32(x) ((int32_t) (uint16_t) (x))
#define zext_i16_i64(x) ((int64_t) (uint16_t) (x))
#define zext_i32_i8(x) ((int8_t) (uint32_t) (x))
#define zext_i32_i16(x) ((int16_t) (uint32_t) (x))
#define zext_i32_i32(x) ((int32_t) (uint32_t) (x))
#define zext_i32_i64(x) ((int64_t) (uint32_t) (x))
#define zext_i64_i8(x) ((int8_t) (uint64_t) (x))
#define zext_i64_i16(x) ((int16_t) (uint64_t) (x))
#define zext_i64_i32(x) ((int32_t) (uint64_t) (x))
#define zext_i64_i64(x) ((int64_t) (uint64_t) (x))

static int8_t abs8(int8_t x) {
  return (int8_t)abs(x);
}

static int16_t abs16(int16_t x) {
  return (int16_t)abs(x);
}

static int32_t abs32(int32_t x) {
  return abs(x);
}

static int64_t abs64(int64_t x) {
#if defined(__OPENCL_VERSION__)
  return abs(x);
#else
  return llabs(x);
#endif
}

#if defined(__OPENCL_VERSION__)
static int32_t futrts_popc8(int8_t x) {
  return popcount(x);
}

static int32_t futrts_popc16(int16_t x) {
  return popcount(x);
}

static int32_t futrts_popc32(int32_t x) {
  return popcount(x);
}

static int32_t futrts_popc64(int64_t x) {
  return popcount(x);
}
#elif defined(__CUDA_ARCH__)

static int32_t futrts_popc8(int8_t x) {
  return __popc(zext_i8_i32(x));
}

static int32_t futrts_popc16(int16_t x) {
  return __popc(zext_i16_i32(x));
}

static int32_t futrts_popc32(int32_t x) {
  return __popc(x);
}

static int32_t futrts_popc64(int64_t x) {
  return __popcll(x);
}

#else // Not OpenCL or CUDA, but plain C.

static int32_t futrts_popc8(uint8_t x) {
  int c = 0;
  for (; x; ++c) { x &= x - 1; }
  return c;
}

static int32_t futrts_popc16(uint16_t x) {
  int c = 0;
  for (; x; ++c) { x &= x - 1; }
  return c;
}

static int32_t futrts_popc32(uint32_t x) {
  int c = 0;
  for (; x; ++c) { x &= x - 1; }
  return c;
}

static int32_t futrts_popc64(uint64_t x) {
  int c = 0;
  for (; x; ++c) { x &= x - 1; }
  return c;
}
#endif

#if defined(__OPENCL_VERSION__)
static uint8_t futrts_mul_hi8(uint8_t a, uint8_t b) {
  return mul_hi(a, b);
}

static uint16_t futrts_mul_hi16(uint16_t a, uint16_t b) {
  return mul_hi(a, b);
}

static uint32_t futrts_mul_hi32(uint32_t a, uint32_t b) {
  return mul_hi(a, b);
}

static uint64_t futrts_mul_hi64(uint64_t a, uint64_t b) {
  return mul_hi(a, b);
}

#elif defined(__CUDA_ARCH__)

static uint8_t futrts_mul_hi8(uint8_t a, uint8_t b) {
  uint16_t aa = a;
  uint16_t bb = b;

  return aa * bb >> 8;
}

static uint16_t futrts_mul_hi16(uint16_t a, uint16_t b) {
  uint32_t aa = a;
  uint32_t bb = b;

  return aa * bb >> 16;
}

static uint32_t futrts_mul_hi32(uint32_t a, uint32_t b) {
  return mulhi(a, b);
}

static uint64_t futrts_mul_hi64(uint64_t a, uint64_t b) {
  return mul64hi(a, b);
}

#else // Not OpenCL or CUDA, but plain C.

static uint8_t futrts_mul_hi8(uint8_t a, uint8_t b) {
  uint16_t aa = a;
  uint16_t bb = b;

  return aa * bb >> 8;
}

static uint16_t futrts_mul_hi16(uint16_t a, uint16_t b) {
  uint32_t aa = a;
  uint32_t bb = b;

  return aa * bb >> 16;
}

static uint32_t futrts_mul_hi32(uint32_t a, uint32_t b) {
  uint64_t aa = a;
  uint64_t bb = b;

  return aa * bb >> 32;
}

static uint64_t futrts_mul_hi64(uint64_t a, uint64_t b) {
  __uint128_t aa = a;
  __uint128_t bb = b;

  return aa * bb >> 64;
}
#endif

#if defined(__OPENCL_VERSION__)
static uint8_t futrts_mad_hi8(uint8_t a, uint8_t b, uint8_t c) {
  return mad_hi(a, b, c);
}

static uint16_t futrts_mad_hi16(uint16_t a, uint16_t b, uint16_t c) {
  return mad_hi(a, b, c);
}

static uint32_t futrts_mad_hi32(uint32_t a, uint32_t b, uint32_t c) {
  return mad_hi(a, b, c);
}

static uint64_t futrts_mad_hi64(uint64_t a, uint64_t b, uint64_t c) {
  return mad_hi(a, b, c);
}

#else // Not OpenCL

static uint8_t futrts_mad_hi8(uint8_t a, uint8_t b, uint8_t c) {
  return futrts_mul_hi8(a, b) + c;
}

static uint16_t futrts_mad_hi16(uint16_t a, uint16_t b, uint16_t c) {
  return futrts_mul_hi16(a, b) + c;
}

static uint32_t futrts_mad_hi32(uint32_t a, uint32_t b, uint32_t c) {
  return futrts_mul_hi32(a, b) + c;
}

static uint64_t futrts_mad_hi64(uint64_t a, uint64_t b, uint64_t c) {
  return futrts_mul_hi64(a, b) + c;
}
#endif

#if defined(__OPENCL_VERSION__)
static int32_t futrts_clzz8(int8_t x) {
  return clz(x);
}

static int32_t futrts_clzz16(int16_t x) {
  return clz(x);
}

static int32_t futrts_clzz32(int32_t x) {
  return clz(x);
}

static int32_t futrts_clzz64(int64_t x) {
  return clz(x);
}

#elif defined(__CUDA_ARCH__)

static int32_t futrts_clzz8(int8_t x) {
  return __clz(zext_i8_i32(x)) - 24;
}

static int32_t futrts_clzz16(int16_t x) {
  return __clz(zext_i16_i32(x)) - 16;
}

static int32_t futrts_clzz32(int32_t x) {
  return __clz(x);
}

static int32_t futrts_clzz64(int64_t x) {
  return __clzll(x);
}

#else // Not OpenCL or CUDA, but plain C.

static int32_t futrts_clzz8(int8_t x) {
  return x == 0 ? 8 : __builtin_clz((uint32_t)zext_i8_i32(x)) - 24;
}

static int32_t futrts_clzz16(int16_t x) {
  return x == 0 ? 16 : __builtin_clz((uint32_t)zext_i16_i32(x)) - 16;
}

static int32_t futrts_clzz32(int32_t x) {
  return x == 0 ? 32 : __builtin_clz((uint32_t)x);
}

static int32_t futrts_clzz64(int64_t x) {
  return x == 0 ? 64 : __builtin_clzll((uint64_t)x);
}
#endif

#if defined(__OPENCL_VERSION__)
static int32_t futrts_ctzz8(int8_t x) {
  int i = 0;
  for (; i < 8 && (x & 1) == 0; i++, x >>= 1)
    ;
  return i;
}

static int32_t futrts_ctzz16(int16_t x) {
  int i = 0;
  for (; i < 16 && (x & 1) == 0; i++, x >>= 1)
    ;
  return i;
}

static int32_t futrts_ctzz32(int32_t x) {
  int i = 0;
  for (; i < 32 && (x & 1) == 0; i++, x >>= 1)
    ;
  return i;
}

static int32_t futrts_ctzz64(int64_t x) {
  int i = 0;
  for (; i < 64 && (x & 1) == 0; i++, x >>= 1)
    ;
  return i;
}

#elif defined(__CUDA_ARCH__)

static int32_t futrts_ctzz8(int8_t x) {
  int y = __ffs(x);
  return y == 0 ? 8 : y - 1;
}

static int32_t futrts_ctzz16(int16_t x) {
  int y = __ffs(x);
  return y == 0 ? 16 : y - 1;
}

static int32_t futrts_ctzz32(int32_t x) {
  int y = __ffs(x);
  return y == 0 ? 32 : y - 1;
}

static int32_t futrts_ctzz64(int64_t x) {
  int y = __ffsll(x);
  return y == 0 ? 64 : y - 1;
}

#else // Not OpenCL or CUDA, but plain C.

static int32_t futrts_ctzz8(int8_t x) {
  return x == 0 ? 8 : __builtin_ctz((uint32_t)x);
}

static int32_t futrts_ctzz16(int16_t x) {
  return x == 0 ? 16 : __builtin_ctz((uint32_t)x);
}

static int32_t futrts_ctzz32(int32_t x) {
  return x == 0 ? 32 : __builtin_ctz((uint32_t)x);
}

static int32_t futrts_ctzz64(int64_t x) {
  return x == 0 ? 64 : __builtin_ctzll((uint64_t)x);
}
#endif

static inline float fdiv32(float x, float y) {
  return x / y;
}

static inline float fadd32(float x, float y) {
  return x + y;
}

static inline float fsub32(float x, float y) {
  return x - y;
}

static inline float fmul32(float x, float y) {
  return x * y;
}

static inline bool cmplt32(float x, float y) {
  return x < y;
}

static inline bool cmple32(float x, float y) {
  return x <= y;
}

static inline float sitofp_i8_f32(int8_t x) {
  return (float) x;
}

static inline float sitofp_i16_f32(int16_t x) {
  return (float) x;
}

static inline float sitofp_i32_f32(int32_t x) {
  return (float) x;
}

static inline float sitofp_i64_f32(int64_t x) {
  return (float) x;
}

static inline float uitofp_i8_f32(uint8_t x) {
  return (float) x;
}

static inline float uitofp_i16_f32(uint16_t x) {
  return (float) x;
}

static inline float uitofp_i32_f32(uint32_t x) {
  return (float) x;
}

static inline float uitofp_i64_f32(uint64_t x) {
  return (float) x;
}

#ifdef __OPENCL_VERSION__
static inline float fabs32(float x) {
  return fabs(x);
}

static inline float fmax32(float x, float y) {
  return fmax(x, y);
}

static inline float fmin32(float x, float y) {
  return fmin(x, y);
}

static inline float fpow32(float x, float y) {
  return pow(x, y);
}

#else // Not OpenCL, but CUDA or plain C.

static inline float fabs32(float x) {
  return fabsf(x);
}

static inline float fmax32(float x, float y) {
  return fmaxf(x, y);
}

static inline float fmin32(float x, float y) {
  return fminf(x, y);
}

static inline float fpow32(float x, float y) {
  return powf(x, y);
}
#endif

static inline bool futrts_isnan32(float x) {
  return isnan(x);
}

static inline bool futrts_isinf32(float x) {
  return isinf(x);
}

static inline int8_t fptosi_f32_i8(float x) {
  if (futrts_isnan32(x) || futrts_isinf32(x)) {
    return 0;
  } else {
    return (int8_t) x;
  }
}

static inline int16_t fptosi_f32_i16(float x) {
  if (futrts_isnan32(x) || futrts_isinf32(x)) {
    return 0;
  } else {
    return (int16_t) x;
  }
}

static inline int32_t fptosi_f32_i32(float x) {
  if (futrts_isnan32(x) || futrts_isinf32(x)) {
    return 0;
  } else {
    return (int32_t) x;
  }
}

static inline int64_t fptosi_f32_i64(float x) {
  if (futrts_isnan32(x) || futrts_isinf32(x)) {
    return 0;
  } else {
    return (int64_t) x;
  };
}

static inline uint8_t fptoui_f32_i8(float x) {
  if (futrts_isnan32(x) || futrts_isinf32(x)) {
    return 0;
  } else {
    return (uint8_t) (int8_t) x;
  }
}

static inline uint16_t fptoui_f32_i16(float x) {
  if (futrts_isnan32(x) || futrts_isinf32(x)) {
    return 0;
  } else {
    return (uint16_t) (int16_t) x;
  }
}

static inline uint32_t fptoui_f32_i32(float x) {
  if (futrts_isnan32(x) || futrts_isinf32(x)) {
    return 0;
  } else {
    return (uint32_t) (int32_t) x;
  }
}

static inline uint64_t fptoui_f32_i64(float x) {
  if (futrts_isnan32(x) || futrts_isinf32(x)) {
    return 0;
  } else {
    return (uint64_t) (int64_t) x;
  }
}

static inline bool ftob_f32_bool(float x) {
  return x != 0;
}

static inline float btof_bool_f32(bool x) {
  return x ? 1 : 0;
}

#ifdef __OPENCL_VERSION__
static inline float futrts_log32(float x) {
  return log(x);
}

static inline float futrts_log2_32(float x) {
  return log2(x);
}

static inline float futrts_log10_32(float x) {
  return log10(x);
}

static inline float futrts_sqrt32(float x) {
  return sqrt(x);
}

static inline float futrts_exp32(float x) {
  return exp(x);
}

static inline float futrts_cos32(float x) {
  return cos(x);
}

static inline float futrts_sin32(float x) {
  return sin(x);
}

static inline float futrts_tan32(float x) {
  return tan(x);
}

static inline float futrts_acos32(float x) {
  return acos(x);
}

static inline float futrts_asin32(float x) {
  return asin(x);
}

static inline float futrts_atan32(float x) {
  return atan(x);
}

static inline float futrts_cosh32(float x) {
  return cosh(x);
}

static inline float futrts_sinh32(float x) {
  return sinh(x);
}

static inline float futrts_tanh32(float x) {
  return tanh(x);
}

static inline float futrts_acosh32(float x) {
  return acosh(x);
}

static inline float futrts_asinh32(float x) {
  return asinh(x);
}

static inline float futrts_atanh32(float x) {
  return atanh(x);
}

static inline float futrts_atan2_32(float x, float y) {
  return atan2(x, y);
}

static inline float futrts_hypot32(float x, float y) {
  return hypot(x, y);
}

static inline float futrts_gamma32(float x) {
  return tgamma(x);
}

static inline float futrts_lgamma32(float x) {
  return lgamma(x);
}

static inline float fmod32(float x, float y) {
  return fmod(x, y);
}

static inline float futrts_round32(float x) {
  return rint(x);
}

static inline float futrts_floor32(float x) {
  return floor(x);
}

static inline float futrts_ceil32(float x) {
  return ceil(x);
}

static inline float futrts_lerp32(float v0, float v1, float t) {
  return mix(v0, v1, t);
}

static inline float futrts_mad32(float a, float b, float c) {
  return mad(a, b, c);
}

static inline float futrts_fma32(float a, float b, float c) {
  return fma(a, b, c);
}

#else // Not OpenCL, but CUDA or plain C.

static inline float futrts_log32(float x) {
  return logf(x);
}

static inline float futrts_log2_32(float x) {
  return log2f(x);
}

static inline float futrts_log10_32(float x) {
  return log10f(x);
}

static inline float futrts_sqrt32(float x) {
  return sqrtf(x);
}

static inline float futrts_exp32(float x) {
  return expf(x);
}

static inline float futrts_cos32(float x) {
  return cosf(x);
}

static inline float futrts_sin32(float x) {
  return sinf(x);
}

static inline float futrts_tan32(float x) {
  return tanf(x);
}

static inline float futrts_acos32(float x) {
  return acosf(x);
}

static inline float futrts_asin32(float x) {
  return asinf(x);
}

static inline float futrts_atan32(float x) {
  return atanf(x);
}

static inline float futrts_cosh32(float x) {
  return coshf(x);
}

static inline float futrts_sinh32(float x) {
  return sinhf(x);
}

static inline float futrts_tanh32(float x) {
  return tanhf(x);
}

static inline float futrts_acosh32(float x) {
  return acoshf(x);
}

static inline float futrts_asinh32(float x) {
  return asinhf(x);
}

static inline float futrts_atanh32(float x) {
  return atanhf(x);
}

static inline float futrts_atan2_32(float x, float y) {
  return atan2f(x, y);
}

static inline float futrts_hypot32(float x, float y) {
  return hypotf(x, y);
}

static inline float futrts_gamma32(float x) {
  return tgammaf(x);
}

static inline float futrts_lgamma32(float x) {
  return lgammaf(x);
}

static inline float fmod32(float x, float y) {
  return fmodf(x, y);
}

static inline float futrts_round32(float x) {
  return rintf(x);
}

static inline float futrts_floor32(float x) {
  return floorf(x);
}

static inline float futrts_ceil32(float x) {
  return ceilf(x);
}

static inline float futrts_lerp32(float v0, float v1, float t) {
  return v0 + (v1 - v0) * t;
}

static inline float futrts_mad32(float a, float b, float c) {
  return a * b + c;
}

static inline float futrts_fma32(float a, float b, float c) {
  return fmaf(a, b, c);
}
#endif

static inline int32_t futrts_to_bits32(float x) {
  union {
    float f;
    int32_t t;
  } p;

  p.f = x;
  return p.t;
}

static inline float futrts_from_bits32(int32_t x) {
  union {
    int32_t f;
    float t;
  } p;

  p.f = x;
  return p.t;
}

static inline float fsignum32(float x) {
  return futrts_isnan32(x) ? x : (x > 0) - (x < 0);
}

#ifdef FUTHARK_F64_ENABLED

static inline double fdiv64(double x, double y) {
  return x / y;
}

static inline double fadd64(double x, double y) {
  return x + y;
}

static inline double fsub64(double x, double y) {
  return x - y;
}

static inline double fmul64(double x, double y) {
  return x * y;
}

static inline bool cmplt64(double x, double y) {
  return x < y;
}

static inline bool cmple64(double x, double y) {
  return x <= y;
}

static inline double sitofp_i8_f64(int8_t x) {
  return (double) x;
}

static inline double sitofp_i16_f64(int16_t x) {
  return (double) x;
}

static inline double sitofp_i32_f64(int32_t x) {
  return (double) x;
}

static inline double sitofp_i64_f64(int64_t x) {
  return (double) x;
}

static inline double uitofp_i8_f64(uint8_t x) {
  return (double) x;
}

static inline double uitofp_i16_f64(uint16_t x) {
  return (double) x;
}

static inline double uitofp_i32_f64(uint32_t x) {
  return (double) x;
}

static inline double uitofp_i64_f64(uint64_t x) {
  return (double) x;
}

static inline double fabs64(double x) {
  return fabs(x);
}

static inline double fmax64(double x, double y) {
  return fmax(x, y);
}

static inline double fmin64(double x, double y) {
  return fmin(x, y);
}

static inline double fpow64(double x, double y) {
  return pow(x, y);
}

static inline double futrts_log64(double x) {
  return log(x);
}

static inline double futrts_log2_64(double x) {
  return log2(x);
}

static inline double futrts_log10_64(double x) {
  return log10(x);
}

static inline double futrts_sqrt64(double x) {
  return sqrt(x);
}

static inline double futrts_exp64(double x) {
  return exp(x);
}

static inline double futrts_cos64(double x) {
  return cos(x);
}

static inline double futrts_sin64(double x) {
  return sin(x);
}

static inline double futrts_tan64(double x) {
  return tan(x);
}

static inline double futrts_acos64(double x) {
  return acos(x);
}

static inline double futrts_asin64(double x) {
  return asin(x);
}

static inline double futrts_atan64(double x) {
  return atan(x);
}

static inline double futrts_cosh64(double x) {
  return cosh(x);
}

static inline double futrts_sinh64(double x) {
  return sinh(x);
}

static inline double futrts_tanh64(double x) {
  return tanh(x);
}

static inline double futrts_acosh64(double x) {
  return acosh(x);
}

static inline double futrts_asinh64(double x) {
  return asinh(x);
}

static inline double futrts_atanh64(double x) {
  return atanh(x);
}

static inline double futrts_atan2_64(double x, double y) {
  return atan2(x, y);
}

static inline double futrts_hypot64(double x, double y) {
  return hypot(x, y);
}

static inline double futrts_gamma64(double x) {
  return tgamma(x);
}

static inline double futrts_lgamma64(double x) {
  return lgamma(x);
}

static inline double futrts_fma64(double a, double b, double c) {
  return fma(a, b, c);
}

static inline double futrts_round64(double x) {
  return rint(x);
}

static inline double futrts_ceil64(double x) {
  return ceil(x);
}

static inline double futrts_floor64(double x) {
  return floor(x);
}

static inline bool futrts_isnan64(double x) {
  return isnan(x);
}

static inline bool futrts_isinf64(double x) {
  return isinf(x);
}

static inline int8_t fptosi_f64_i8(double x) {
  if (futrts_isnan64(x) || futrts_isinf64(x)) {
    return 0;
  } else {
    return (int8_t) x;
  }
}

static inline int16_t fptosi_f64_i16(double x) {
  if (futrts_isnan64(x) || futrts_isinf64(x)) {
    return 0;
  } else {
    return (int16_t) x;
  }
}

static inline int32_t fptosi_f64_i32(double x) {
  if (futrts_isnan64(x) || futrts_isinf64(x)) {
    return 0;
  } else {
    return (int32_t) x;
  }
}

static inline int64_t fptosi_f64_i64(double x) {
  if (futrts_isnan64(x) || futrts_isinf64(x)) {
    return 0;
  } else {
    return (int64_t) x;
  }
}

static inline uint8_t fptoui_f64_i8(double x) {
  if (futrts_isnan64(x) || futrts_isinf64(x)) {
    return 0;
  } else {
    return (uint8_t) (int8_t) x;
  }
}

static inline uint16_t fptoui_f64_i16(double x) {
  if (futrts_isnan64(x) || futrts_isinf64(x)) {
    return 0;
  } else {
    return (uint16_t) (int16_t) x;
  }
}

static inline uint32_t fptoui_f64_i32(double x) {
  if (futrts_isnan64(x) || futrts_isinf64(x)) {
    return 0;
  } else {
    return (uint32_t) (int32_t) x;
  }
}

static inline uint64_t fptoui_f64_i64(double x) {
  if (futrts_isnan64(x) || futrts_isinf64(x)) {
    return 0;
  } else {
    return (uint64_t) (int64_t) x;
  }
}

static inline bool ftob_f64_bool(double x) {
  return x != 0;
}

static inline double btof_bool_f64(bool x) {
  return x ? 1 : 0;
}

static inline int64_t futrts_to_bits64(double x) {
  union {
    double f;
    int64_t t;
  } p;

  p.f = x;
  return p.t;
}

static inline double futrts_from_bits64(int64_t x) {
  union {
    int64_t f;
    double t;
  } p;

  p.f = x;
  return p.t;
}

static inline double fmod64(double x, double y) {
  return fmod(x, y);
}

static inline double fsignum64(double x) {
  return futrts_isnan64(x) ? x : (x > 0) - (x < 0);
}

static inline double futrts_lerp64(double v0, double v1, double t) {
#ifdef __OPENCL_VERSION__
  return mix(v0, v1, t);
#else
  return v0 + (v1 - v0) * t;
#endif
}

static inline double futrts_mad64(double a, double b, double c) {
#ifdef __OPENCL_VERSION__
  return mad(a, b, c);
#else
  return a * b + c;
#endif
}

static inline float fpconv_f32_f32(float x) {
  return (float) x;
}

static inline double fpconv_f32_f64(float x) {
  return (double) x;
}

static inline float fpconv_f64_f32(double x) {
  return (float) x;
}

static inline double fpconv_f64_f64(double x) {
  return (double) x;
}

#endif

// End of scalar.h.
// Start of scalar_f16.h.

// Half-precision is emulated if needed (e.g. in straight C) with the
// native type used if possible.  The emulation works by typedef'ing
// 'float' to 'f16', and then implementing all operations on single
// precision.  To cut down on duplication, we use the same code for
// those Futhark functions that require just operators or casts.  The
// in-memory representation for arrays will still be 16 bits even
// under emulation, so the compiler will have to be careful when
// generating reads or writes.

#if !defined(cl_khr_fp16) && !(defined(__CUDA_ARCH__) && __CUDA_ARCH__ >= 600)
#define EMULATE_F16
#endif

#if !defined(EMULATE_F16) && defined(__OPENCL_VERSION__)
#pragma OPENCL EXTENSION cl_khr_fp16 : enable
#endif

#ifdef EMULATE_F16

// Note that the half-precision storage format is still 16 bits - the
// compiler will have to be real careful!
typedef float f16;

#else

#ifdef __CUDA_ARCH__
#include <cuda_fp16.h>
#endif

typedef half f16;

#endif

// Some of these functions convert to single precision because half
// precision versions are not available.

static inline f16 fadd16(f16 x, f16 y) {
  return x + y;
}

static inline f16 fsub16(f16 x, f16 y) {
  return x - y;
}

static inline f16 fmul16(f16 x, f16 y) {
  return x * y;
}

static inline bool cmplt16(f16 x, f16 y) {
  return x < y;
}

static inline bool cmple16(f16 x, f16 y) {
  return x <= y;
}

static inline f16 sitofp_i8_f16(int8_t x) {
  return (f16) x;
}

static inline f16 sitofp_i16_f16(int16_t x) {
  return (f16) x;
}

static inline f16 sitofp_i32_f16(int32_t x) {
  return (f16) x;
}

static inline f16 sitofp_i64_f16(int64_t x) {
  return (f16) x;
}

static inline f16 uitofp_i8_f16(uint8_t x) {
  return (f16) x;
}

static inline f16 uitofp_i16_f16(uint16_t x) {
  return (f16) x;
}

static inline f16 uitofp_i32_f16(uint32_t x) {
  return (f16) x;
}

static inline f16 uitofp_i64_f16(uint64_t x) {
  return (f16) x;
}

static inline int8_t fptosi_f16_i8(f16 x) {
  return (int8_t) (float) x;
}

static inline int16_t fptosi_f16_i16(f16 x) {
  return (int16_t) x;
}

static inline int32_t fptosi_f16_i32(f16 x) {
  return (int32_t) x;
}

static inline int64_t fptosi_f16_i64(f16 x) {
  return (int64_t) x;
}

static inline uint8_t fptoui_f16_i8(f16 x) {
  return (uint8_t) (float) x;
}

static inline uint16_t fptoui_f16_i16(f16 x) {
  return (uint16_t) x;
}

static inline uint32_t fptoui_f16_i32(f16 x) {
  return (uint32_t) x;
}

static inline uint64_t fptoui_f16_i64(f16 x) {
  return (uint64_t) x;
}

static inline bool ftob_f16_bool(f16 x) {
  return x != (f16)0;
}

static inline f16 btof_bool_f16(bool x) {
  return x ? 1 : 0;
}

#ifndef EMULATE_F16

#ifdef __OPENCL_VERSION__
static inline f16 fabs16(f16 x) {
  return fabs(x);
}

static inline f16 fmax16(f16 x, f16 y) {
  return fmax(x, y);
}

static inline f16 fmin16(f16 x, f16 y) {
  return fmin(x, y);
}

static inline f16 fpow16(f16 x, f16 y) {
  return pow(x, y);
}

#else // Assuming CUDA.

static inline f16 fabs16(f16 x) {
  return fabsf(x);
}

static inline f16 fmax16(f16 x, f16 y) {
  return fmaxf(x, y);
}

static inline f16 fmin16(f16 x, f16 y) {
  return fminf(x, y);
}

static inline f16 fpow16(f16 x, f16 y) {
  return powf(x, y);
}
#endif

static inline bool futrts_isnan16(f16 x) {
  return isnan((float)x);
}

static inline bool futrts_isinf16(f16 x) {
  return isinf((float)x);
}

#ifdef __OPENCL_VERSION__
static inline f16 futrts_log16(f16 x) {
  return log(x);
}

static inline f16 futrts_log2_16(f16 x) {
  return log2(x);
}

static inline f16 futrts_log10_16(f16 x) {
  return log10(x);
}

static inline f16 futrts_sqrt16(f16 x) {
  return sqrt(x);
}

static inline f16 futrts_exp16(f16 x) {
  return exp(x);
}

static inline f16 futrts_cos16(f16 x) {
  return cos(x);
}

static inline f16 futrts_sin16(f16 x) {
  return sin(x);
}

static inline f16 futrts_tan16(f16 x) {
  return tan(x);
}

static inline f16 futrts_acos16(f16 x) {
  return acos(x);
}

static inline f16 futrts_asin16(f16 x) {
  return asin(x);
}

static inline f16 futrts_atan16(f16 x) {
  return atan(x);
}

static inline f16 futrts_cosh16(f16 x) {
  return cosh(x);
}

static inline f16 futrts_sinh16(f16 x) {
  return sinh(x);
}

static inline f16 futrts_tanh16(f16 x) {
  return tanh(x);
}

static inline f16 futrts_acosh16(f16 x) {
  return acosh(x);
}

static inline f16 futrts_asinh16(f16 x) {
  return asinh(x);
}

static inline f16 futrts_atanh16(f16 x) {
  return atanh(x);
}

static inline f16 futrts_atan2_16(f16 x, f16 y) {
  return atan2(x, y);
}

static inline f16 futrts_hypot16(f16 x, f16 y) {
  return hypot(x, y);
}

static inline f16 futrts_gamma16(f16 x) {
  return tgamma(x);
}

static inline f16 futrts_lgamma16(f16 x) {
  return lgamma(x);
}

static inline f16 fmod16(f16 x, f16 y) {
  return fmod(x, y);
}

static inline f16 futrts_round16(f16 x) {
  return rint(x);
}

static inline f16 futrts_floor16(f16 x) {
  return floor(x);
}

static inline f16 futrts_ceil16(f16 x) {
  return ceil(x);
}

static inline f16 futrts_lerp16(f16 v0, f16 v1, f16 t) {
  return mix(v0, v1, t);
}

static inline f16 futrts_mad16(f16 a, f16 b, f16 c) {
  return mad(a, b, c);
}

static inline f16 futrts_fma16(f16 a, f16 b, f16 c) {
  return fma(a, b, c);
}

#else // Assume CUDA.

static inline f16 futrts_log16(f16 x) {
  return hlog(x);
}

static inline f16 futrts_log2_16(f16 x) {
  return hlog2(x);
}

static inline f16 futrts_log10_16(f16 x) {
  return hlog10(x);
}

static inline f16 futrts_sqrt16(f16 x) {
  return hsqrt(x);
}

static inline f16 futrts_exp16(f16 x) {
  return hexp(x);
}

static inline f16 futrts_cos16(f16 x) {
  return hcos(x);
}

static inline f16 futrts_sin16(f16 x) {
  return hsin(x);
}

static inline f16 futrts_tan16(f16 x) {
  return tanf(x);
}

static inline f16 futrts_acos16(f16 x) {
  return acosf(x);
}

static inline f16 futrts_asin16(f16 x) {
  return asinf(x);
}

static inline f16 futrts_atan16(f16 x) {
  return atanf(x);
}

static inline f16 futrts_cosh16(f16 x) {
  return coshf(x);
}

static inline f16 futrts_sinh16(f16 x) {
  return sinhf(x);
}

static inline f16 futrts_tanh16(f16 x) {
  return tanhf(x);
}

static inline f16 futrts_acosh16(f16 x) {
  return acoshf(x);
}

static inline f16 futrts_asinh16(f16 x) {
  return asinhf(x);
}

static inline f16 futrts_atanh16(f16 x) {
  return atanhf(x);
}

static inline f16 futrts_atan2_16(f16 x, f16 y) {
  return atan2f(x, y);
}

static inline f16 futrts_hypot16(f16 x, f16 y) {
  return hypotf(x, y);
}

static inline f16 futrts_gamma16(f16 x) {
  return tgammaf(x);
}

static inline f16 futrts_lgamma16(f16 x) {
  return lgammaf(x);
}

static inline f16 fmod16(f16 x, f16 y) {
  return fmodf(x, y);
}

static inline f16 futrts_round16(f16 x) {
  return rintf(x);
}

static inline f16 futrts_floor16(f16 x) {
  return hfloor(x);
}

static inline f16 futrts_ceil16(f16 x) {
  return hceil(x);
}

static inline f16 futrts_lerp16(f16 v0, f16 v1, f16 t) {
  return v0 + (v1 - v0) * t;
}

static inline f16 futrts_mad16(f16 a, f16 b, f16 c) {
  return a * b + c;
}

static inline f16 futrts_fma16(f16 a, f16 b, f16 c) {
  return fmaf(a, b, c);
}

#endif

// The CUDA __half type cannot be put in unions for some reason, so we
// use bespoke conversion functions instead.
#ifdef __CUDA_ARCH__
static inline int16_t futrts_to_bits16(f16 x) {
  return __half_as_ushort(x);
}
static inline f16 futrts_from_bits16(int16_t x) {
  return __ushort_as_half(x);
}
#else
static inline int16_t futrts_to_bits16(f16 x) {
  union {
    f16 f;
    int16_t t;
  } p;

  p.f = x;
  return p.t;
}

static inline f16 futrts_from_bits16(int16_t x) {
  union {
    int16_t f;
    f16 t;
  } p;

  p.f = x;
  return p.t;
}
#endif

#else // No native f16 - emulate.

static inline f16 fabs16(f16 x) {
  return fabs32(x);
}

static inline f16 fmax16(f16 x, f16 y) {
  return fmax32(x, y);
}

static inline f16 fmin16(f16 x, f16 y) {
  return fmin32(x, y);
}

static inline f16 fpow16(f16 x, f16 y) {
  return fpow32(x, y);
}

static inline bool futrts_isnan16(f16 x) {
  return futrts_isnan32(x);
}

static inline bool futrts_isinf16(f16 x) {
  return futrts_isinf32(x);
}

static inline f16 futrts_log16(f16 x) {
  return futrts_log32(x);
}

static inline f16 futrts_log2_16(f16 x) {
  return futrts_log2_32(x);
}

static inline f16 futrts_log10_16(f16 x) {
  return futrts_log10_32(x);
}

static inline f16 futrts_sqrt16(f16 x) {
  return futrts_sqrt32(x);
}

static inline f16 futrts_exp16(f16 x) {
  return futrts_exp32(x);
}

static inline f16 futrts_cos16(f16 x) {
  return futrts_cos32(x);
}

static inline f16 futrts_sin16(f16 x) {
  return futrts_sin32(x);
}

static inline f16 futrts_tan16(f16 x) {
  return futrts_tan32(x);
}

static inline f16 futrts_acos16(f16 x) {
  return futrts_acos32(x);
}

static inline f16 futrts_asin16(f16 x) {
  return futrts_asin32(x);
}

static inline f16 futrts_atan16(f16 x) {
  return futrts_atan32(x);
}

static inline f16 futrts_cosh16(f16 x) {
  return futrts_cosh32(x);
}

static inline f16 futrts_sinh16(f16 x) {
  return futrts_sinh32(x);
}

static inline f16 futrts_tanh16(f16 x) {
  return futrts_tanh32(x);
}

static inline f16 futrts_acosh16(f16 x) {
  return futrts_acosh32(x);
}

static inline f16 futrts_asinh16(f16 x) {
  return futrts_asinh32(x);
}

static inline f16 futrts_atanh16(f16 x) {
  return futrts_atanh32(x);
}

static inline f16 futrts_atan2_16(f16 x, f16 y) {
  return futrts_atan2_32(x, y);
}

static inline f16 futrts_hypot16(f16 x, f16 y) {
  return futrts_hypot32(x, y);
}

static inline f16 futrts_gamma16(f16 x) {
  return futrts_gamma32(x);
}

static inline f16 futrts_lgamma16(f16 x) {
  return futrts_lgamma32(x);
}

static inline f16 fmod16(f16 x, f16 y) {
  return fmod32(x, y);
}

static inline f16 futrts_round16(f16 x) {
  return futrts_round32(x);
}

static inline f16 futrts_floor16(f16 x) {
  return futrts_floor32(x);
}

static inline f16 futrts_ceil16(f16 x) {
  return futrts_ceil32(x);
}

static inline f16 futrts_lerp16(f16 v0, f16 v1, f16 t) {
  return futrts_lerp32(v0, v1, t);
}

static inline f16 futrts_mad16(f16 a, f16 b, f16 c) {
  return futrts_mad32(a, b, c);
}

static inline f16 futrts_fma16(f16 a, f16 b, f16 c) {
  return futrts_fma32(a, b, c);
}

// Even when we are using an OpenCL that does not support cl_khr_fp16,
// it must still support vload_half for actually creating a
// half-precision number, which can then be efficiently converted to a
// float.  Similarly for vstore_half.
#ifdef __OPENCL_VERSION__

static inline int16_t futrts_to_bits16(f16 x) {
  int16_t y;
  // Violating strict aliasing here.
  vstore_half((float)x, 0, (half*)&y);
  return y;
}

static inline f16 futrts_from_bits16(int16_t x) {
  return (f16)vload_half(0, (half*)&x);
}

#else

static inline int16_t futrts_to_bits16(f16 x) {
  return (int16_t)float2halfbits(x);
}

static inline f16 futrts_from_bits16(int16_t x) {
  return halfbits2float((uint16_t)x);
}

static inline f16 fsignum16(f16 x) {
  return futrts_isnan16(x) ? x : (x > 0) - (x < 0);
}

#endif

#endif

static inline float fpconv_f16_f16(f16 x) {
  return x;
}

static inline float fpconv_f16_f32(f16 x) {
  return x;
}

static inline f16 fpconv_f32_f16(float x) {
  return x;
}

#ifdef FUTHARK_F64_ENABLED

static inline double fpconv_f16_f64(f16 x) {
  return (double) x;
}

static inline f16 fpconv_f64_f16(double x) {
  return (f16) x;
}

#endif


// End of scalar_f16.h.

static int init_constants(struct futhark_context *);
static int free_constants(struct futhark_context *);
struct memblock {
    int *references;
    unsigned char *mem;
    int64_t size;
    const char *desc;
};
struct futhark_context_config {
    int debugging;
    int in_use;
};
struct futhark_context_config *futhark_context_config_new(void)
{
    struct futhark_context_config *cfg =
                                  (struct futhark_context_config *) malloc(sizeof(struct futhark_context_config));
    
    if (cfg == NULL)
        return NULL;
    cfg->in_use = 0;
    cfg->debugging = 0;
    return cfg;
}
void futhark_context_config_free(struct futhark_context_config *cfg)
{
    assert(!cfg->in_use);
    free(cfg);
}
void futhark_context_config_set_debugging(struct futhark_context_config *cfg,
                                          int detail)
{
    cfg->debugging = detail;
}
void futhark_context_config_set_profiling(struct futhark_context_config *cfg,
                                          int flag)
{
    (void) cfg;
    (void) flag;
}
void futhark_context_config_set_logging(struct futhark_context_config *cfg,
                                        int detail)
{
    // Does nothing for this backend.
    (void) cfg;
    (void) detail;
}
struct futhark_context {
    struct futhark_context_config *cfg;
    int detail_memory;
    int debugging;
    int profiling;
    int logging;
    lock_t lock;
    char *error;
    FILE *log;
    int profiling_paused;
    int64_t peak_mem_usage_default;
    int64_t cur_mem_usage_default;
    struct {
        int dummy;
    } constants;
};
struct futhark_context *futhark_context_new(struct futhark_context_config *cfg)
{
    assert(!cfg->in_use);
    
    struct futhark_context *ctx =
                           (struct futhark_context *) malloc(sizeof(struct futhark_context));
    
    if (ctx == NULL)
        return NULL;
    ctx->cfg = cfg;
    ctx->cfg->in_use = 1;
    ctx->detail_memory = cfg->debugging;
    ctx->debugging = cfg->debugging;
    ctx->profiling = cfg->debugging;
    ctx->logging = cfg->debugging;
    ctx->error = NULL;
    ctx->log = stderr;
    create_lock(&ctx->lock);
    ctx->peak_mem_usage_default = 0;
    ctx->cur_mem_usage_default = 0;
    init_constants(ctx);
    return ctx;
}
void futhark_context_free(struct futhark_context *ctx)
{
    free_constants(ctx);
    free_lock(&ctx->lock);
    ctx->cfg->in_use = 0;
    free(ctx);
}
int futhark_context_sync(struct futhark_context *ctx)
{
    (void) ctx;
    return 0;
}
static const char *tuning_param_names[0];
static const char *tuning_param_vars[0];
static const char *tuning_param_classes[0];
int futhark_context_config_set_tuning_param(struct futhark_context_config *cfg,
                                            const char *param_name,
                                            size_t param_value)
{
    (void) cfg;
    (void) param_name;
    (void) param_value;
    return 1;
}
static int memblock_unref(struct futhark_context *ctx, struct memblock *block,
                          const char *desc)
{
    if (block->references != NULL) {
        *block->references -= 1;
        if (ctx->detail_memory)
            fprintf(ctx->log,
                    "Unreferencing block %s (allocated as %s) in %s: %d references remaining.\n",
                    desc, block->desc, "default space", *block->references);
        if (*block->references == 0) {
            ctx->cur_mem_usage_default -= block->size;
            free(block->mem);
            free(block->references);
            if (ctx->detail_memory)
                fprintf(ctx->log,
                        "%lld bytes freed (now allocated: %lld bytes)\n",
                        (long long) block->size,
                        (long long) ctx->cur_mem_usage_default);
        }
        block->references = NULL;
    }
    return 0;
}
static int memblock_alloc(struct futhark_context *ctx, struct memblock *block,
                          int64_t size, const char *desc)
{
    if (size < 0)
        futhark_panic(1,
                      "Negative allocation of %lld bytes attempted for %s in %s.\n",
                      (long long) size, desc, "default space",
                      ctx->cur_mem_usage_default);
    
    int ret = memblock_unref(ctx, block, desc);
    
    if (ret != FUTHARK_SUCCESS)
        return ret;
    if (ctx->detail_memory)
        fprintf(ctx->log,
                "Allocating %lld bytes for %s in %s (then allocated: %lld bytes)",
                (long long) size, desc, "default space",
                (long long) ctx->cur_mem_usage_default + size);
    if (ctx->cur_mem_usage_default > ctx->peak_mem_usage_default) {
        ctx->peak_mem_usage_default = ctx->cur_mem_usage_default;
        if (ctx->detail_memory)
            fprintf(ctx->log, " (new peak).\n");
    } else if (ctx->detail_memory)
        fprintf(ctx->log, ".\n");
    block->mem = (unsigned char *) malloc((size_t) size);
    if (ctx->error == NULL) {
        block->references = (int *) malloc(sizeof(int));
        *block->references = 1;
        block->size = size;
        block->desc = desc;
        ctx->cur_mem_usage_default += size;
        return FUTHARK_SUCCESS;
    } else {
        char *old_error = ctx->error;
        
        ctx->error =
            msgprintf("Failed to allocate memory in %s.\nAttempted allocation: %12lld bytes\nCurrently allocated:  %12lld bytes\n%s",
                      "default space", (long long) size,
                      (long long) ctx->cur_mem_usage_default, old_error);
        free(old_error);
        return FUTHARK_OUT_OF_MEMORY;
    }
}
static int memblock_set(struct futhark_context *ctx, struct memblock *lhs,
                        struct memblock *rhs, const char *lhs_desc)
{
    int ret = memblock_unref(ctx, lhs, lhs_desc);
    
    if (rhs->references != NULL)
        (*rhs->references)++;
    *lhs = *rhs;
    return ret;
}
int futhark_get_tuning_param_count(void)
{
    return sizeof(tuning_param_names) / sizeof(tuning_param_names[0]);
}
const char *futhark_get_tuning_param_name(int i)
{
    return tuning_param_names[i];
}
const char *futhark_get_tuning_param_class(int i)
{
    return tuning_param_classes[i];
}
char *futhark_context_report(struct futhark_context *ctx)
{
    if (futhark_context_sync(ctx) != 0)
        return NULL;
    
    struct str_builder builder;
    
    str_builder_init(&builder);
    if (ctx->detail_memory || ctx->profiling || ctx->logging) {
        { }
    }
    if (ctx->profiling) { }
    return builder.str;
}
char *futhark_context_get_error(struct futhark_context *ctx)
{
    char *error = ctx->error;
    
    ctx->error = NULL;
    return error;
}
void futhark_context_set_logging_file(struct futhark_context *ctx, FILE *f)
{
    ctx->log = f;
}
void futhark_context_pause_profiling(struct futhark_context *ctx)
{
    ctx->profiling_paused = 1;
}
void futhark_context_unpause_profiling(struct futhark_context *ctx)
{
    ctx->profiling_paused = 0;
}
int futhark_context_clear_caches(struct futhark_context *ctx)
{
    lock_lock(&ctx->lock);
    ctx->peak_mem_usage_default = 0;
    lock_unlock(&ctx->lock);
    return ctx->error != NULL;
}

static int futrts_entry_main(struct futhark_context *ctx,
                             struct memblock *mem_out_p_39807,
                             struct memblock *mem_out_p_39808,
                             struct memblock *mem_out_p_39809,
                             struct memblock *mem_out_p_39810,
                             struct memblock *mem_out_p_39811,
                             struct memblock *mem_out_p_39812,
                             struct memblock *mem_out_p_39813,
                             struct memblock tketau_mem_37734,
                             struct memblock tketaup1_mem_37735,
                             struct memblock tketaum1_mem_37736,
                             struct memblock dtketau_mem_37737,
                             struct memblock dtketaup1_mem_37738,
                             struct memblock dtketaum1_mem_37739,
                             struct memblock utau_mem_37740,
                             struct memblock vtau_mem_37741,
                             struct memblock wtau_mem_37742,
                             struct memblock maskU_mem_37743,
                             struct memblock maskV_mem_37744,
                             struct memblock maskW_mem_37745,
                             struct memblock dxt_mem_37746,
                             struct memblock dxu_mem_37747,
                             struct memblock dyt_mem_37748,
                             struct memblock dyu_mem_37749,
                             struct memblock dzzt_mem_37750,
                             struct memblock dzzw_mem_37751,
                             struct memblock cost_mem_37752,
                             struct memblock cosu_mem_37753,
                             struct memblock kbot_mem_37754,
                             struct memblock kappaM_mem_37755,
                             struct memblock mxl_mem_37756,
                             struct memblock forc_mem_37757,
                             struct memblock forc_tke_surface_mem_37758,
                             int64_t xdim_31402, int64_t ydim_31403,
                             int64_t zzdim_31404);

static int init_constants(struct futhark_context *ctx)
{
    (void) ctx;
    
    int err = 0;
    
    
  cleanup:
    return err;
}
static int free_constants(struct futhark_context *ctx)
{
    (void) ctx;
    return 0;
}
struct futhark_i32_2d {
    struct memblock mem;
    int64_t shape[2];
};
struct futhark_i32_2d *futhark_new_i32_2d(struct futhark_context *ctx, const
                                          int32_t *data, int64_t dim0,
                                          int64_t dim1)
{
    struct futhark_i32_2d *bad = NULL;
    struct futhark_i32_2d *arr =
                          (struct futhark_i32_2d *) malloc(sizeof(struct futhark_i32_2d));
    
    if (arr == NULL)
        return bad;
    lock_lock(&ctx->lock);
    arr->mem.references = NULL;
    if (memblock_alloc(ctx, &arr->mem, dim0 * dim1 * 4, "arr->mem"))
        return NULL;
    arr->shape[0] = dim0;
    arr->shape[1] = dim1;
    if ((size_t) (dim0 * dim1) * 4 > 0)
        memmove(arr->mem.mem + 0, data + 0, (size_t) (dim0 * dim1) * 4);
    lock_unlock(&ctx->lock);
    return arr;
}
struct futhark_i32_2d *futhark_new_raw_i32_2d(struct futhark_context *ctx, const
                                              unsigned char *data,
                                              int64_t offset, int64_t dim0,
                                              int64_t dim1)
{
    struct futhark_i32_2d *bad = NULL;
    struct futhark_i32_2d *arr =
                          (struct futhark_i32_2d *) malloc(sizeof(struct futhark_i32_2d));
    
    if (arr == NULL)
        return bad;
    lock_lock(&ctx->lock);
    arr->mem.references = NULL;
    if (memblock_alloc(ctx, &arr->mem, dim0 * dim1 * 4, "arr->mem"))
        return NULL;
    arr->shape[0] = dim0;
    arr->shape[1] = dim1;
    if ((size_t) (dim0 * dim1) * 4 > 0)
        memmove(arr->mem.mem + 0, data + offset, (size_t) (dim0 * dim1) * 4);
    lock_unlock(&ctx->lock);
    return arr;
}
int futhark_free_i32_2d(struct futhark_context *ctx, struct futhark_i32_2d *arr)
{
    lock_lock(&ctx->lock);
    if (memblock_unref(ctx, &arr->mem, "arr->mem") != 0)
        return 1;
    lock_unlock(&ctx->lock);
    free(arr);
    return 0;
}
int futhark_values_i32_2d(struct futhark_context *ctx,
                          struct futhark_i32_2d *arr, int32_t *data)
{
    lock_lock(&ctx->lock);
    if ((size_t) (arr->shape[0] * arr->shape[1]) * 4 > 0)
        memmove(data + 0, arr->mem.mem + 0, (size_t) (arr->shape[0] *
                                                      arr->shape[1]) * 4);
    lock_unlock(&ctx->lock);
    return 0;
}
unsigned char *futhark_values_raw_i32_2d(struct futhark_context *ctx,
                                         struct futhark_i32_2d *arr)
{
    (void) ctx;
    return arr->mem.mem;
}
const int64_t *futhark_shape_i32_2d(struct futhark_context *ctx,
                                    struct futhark_i32_2d *arr)
{
    (void) ctx;
    return arr->shape;
}
struct futhark_f32_1d {
    struct memblock mem;
    int64_t shape[1];
};
struct futhark_f32_1d *futhark_new_f32_1d(struct futhark_context *ctx, const
                                          float *data, int64_t dim0)
{
    struct futhark_f32_1d *bad = NULL;
    struct futhark_f32_1d *arr =
                          (struct futhark_f32_1d *) malloc(sizeof(struct futhark_f32_1d));
    
    if (arr == NULL)
        return bad;
    lock_lock(&ctx->lock);
    arr->mem.references = NULL;
    if (memblock_alloc(ctx, &arr->mem, dim0 * 4, "arr->mem"))
        return NULL;
    arr->shape[0] = dim0;
    if ((size_t) dim0 * 4 > 0)
        memmove(arr->mem.mem + 0, data + 0, (size_t) dim0 * 4);
    lock_unlock(&ctx->lock);
    return arr;
}
struct futhark_f32_1d *futhark_new_raw_f32_1d(struct futhark_context *ctx, const
                                              unsigned char *data,
                                              int64_t offset, int64_t dim0)
{
    struct futhark_f32_1d *bad = NULL;
    struct futhark_f32_1d *arr =
                          (struct futhark_f32_1d *) malloc(sizeof(struct futhark_f32_1d));
    
    if (arr == NULL)
        return bad;
    lock_lock(&ctx->lock);
    arr->mem.references = NULL;
    if (memblock_alloc(ctx, &arr->mem, dim0 * 4, "arr->mem"))
        return NULL;
    arr->shape[0] = dim0;
    if ((size_t) dim0 * 4 > 0)
        memmove(arr->mem.mem + 0, data + offset, (size_t) dim0 * 4);
    lock_unlock(&ctx->lock);
    return arr;
}
int futhark_free_f32_1d(struct futhark_context *ctx, struct futhark_f32_1d *arr)
{
    lock_lock(&ctx->lock);
    if (memblock_unref(ctx, &arr->mem, "arr->mem") != 0)
        return 1;
    lock_unlock(&ctx->lock);
    free(arr);
    return 0;
}
int futhark_values_f32_1d(struct futhark_context *ctx,
                          struct futhark_f32_1d *arr, float *data)
{
    lock_lock(&ctx->lock);
    if ((size_t) arr->shape[0] * 4 > 0)
        memmove(data + 0, arr->mem.mem + 0, (size_t) arr->shape[0] * 4);
    lock_unlock(&ctx->lock);
    return 0;
}
unsigned char *futhark_values_raw_f32_1d(struct futhark_context *ctx,
                                         struct futhark_f32_1d *arr)
{
    (void) ctx;
    return arr->mem.mem;
}
const int64_t *futhark_shape_f32_1d(struct futhark_context *ctx,
                                    struct futhark_f32_1d *arr)
{
    (void) ctx;
    return arr->shape;
}
struct futhark_f32_2d {
    struct memblock mem;
    int64_t shape[2];
};
struct futhark_f32_2d *futhark_new_f32_2d(struct futhark_context *ctx, const
                                          float *data, int64_t dim0,
                                          int64_t dim1)
{
    struct futhark_f32_2d *bad = NULL;
    struct futhark_f32_2d *arr =
                          (struct futhark_f32_2d *) malloc(sizeof(struct futhark_f32_2d));
    
    if (arr == NULL)
        return bad;
    lock_lock(&ctx->lock);
    arr->mem.references = NULL;
    if (memblock_alloc(ctx, &arr->mem, dim0 * dim1 * 4, "arr->mem"))
        return NULL;
    arr->shape[0] = dim0;
    arr->shape[1] = dim1;
    if ((size_t) (dim0 * dim1) * 4 > 0)
        memmove(arr->mem.mem + 0, data + 0, (size_t) (dim0 * dim1) * 4);
    lock_unlock(&ctx->lock);
    return arr;
}
struct futhark_f32_2d *futhark_new_raw_f32_2d(struct futhark_context *ctx, const
                                              unsigned char *data,
                                              int64_t offset, int64_t dim0,
                                              int64_t dim1)
{
    struct futhark_f32_2d *bad = NULL;
    struct futhark_f32_2d *arr =
                          (struct futhark_f32_2d *) malloc(sizeof(struct futhark_f32_2d));
    
    if (arr == NULL)
        return bad;
    lock_lock(&ctx->lock);
    arr->mem.references = NULL;
    if (memblock_alloc(ctx, &arr->mem, dim0 * dim1 * 4, "arr->mem"))
        return NULL;
    arr->shape[0] = dim0;
    arr->shape[1] = dim1;
    if ((size_t) (dim0 * dim1) * 4 > 0)
        memmove(arr->mem.mem + 0, data + offset, (size_t) (dim0 * dim1) * 4);
    lock_unlock(&ctx->lock);
    return arr;
}
int futhark_free_f32_2d(struct futhark_context *ctx, struct futhark_f32_2d *arr)
{
    lock_lock(&ctx->lock);
    if (memblock_unref(ctx, &arr->mem, "arr->mem") != 0)
        return 1;
    lock_unlock(&ctx->lock);
    free(arr);
    return 0;
}
int futhark_values_f32_2d(struct futhark_context *ctx,
                          struct futhark_f32_2d *arr, float *data)
{
    lock_lock(&ctx->lock);
    if ((size_t) (arr->shape[0] * arr->shape[1]) * 4 > 0)
        memmove(data + 0, arr->mem.mem + 0, (size_t) (arr->shape[0] *
                                                      arr->shape[1]) * 4);
    lock_unlock(&ctx->lock);
    return 0;
}
unsigned char *futhark_values_raw_f32_2d(struct futhark_context *ctx,
                                         struct futhark_f32_2d *arr)
{
    (void) ctx;
    return arr->mem.mem;
}
const int64_t *futhark_shape_f32_2d(struct futhark_context *ctx,
                                    struct futhark_f32_2d *arr)
{
    (void) ctx;
    return arr->shape;
}
struct futhark_f32_3d {
    struct memblock mem;
    int64_t shape[3];
};
struct futhark_f32_3d *futhark_new_f32_3d(struct futhark_context *ctx, const
                                          float *data, int64_t dim0,
                                          int64_t dim1, int64_t dim2)
{
    struct futhark_f32_3d *bad = NULL;
    struct futhark_f32_3d *arr =
                          (struct futhark_f32_3d *) malloc(sizeof(struct futhark_f32_3d));
    
    if (arr == NULL)
        return bad;
    lock_lock(&ctx->lock);
    arr->mem.references = NULL;
    if (memblock_alloc(ctx, &arr->mem, dim0 * dim1 * dim2 * 4, "arr->mem"))
        return NULL;
    arr->shape[0] = dim0;
    arr->shape[1] = dim1;
    arr->shape[2] = dim2;
    if ((size_t) (dim0 * dim1 * dim2) * 4 > 0)
        memmove(arr->mem.mem + 0, data + 0, (size_t) (dim0 * dim1 * dim2) * 4);
    lock_unlock(&ctx->lock);
    return arr;
}
struct futhark_f32_3d *futhark_new_raw_f32_3d(struct futhark_context *ctx, const
                                              unsigned char *data,
                                              int64_t offset, int64_t dim0,
                                              int64_t dim1, int64_t dim2)
{
    struct futhark_f32_3d *bad = NULL;
    struct futhark_f32_3d *arr =
                          (struct futhark_f32_3d *) malloc(sizeof(struct futhark_f32_3d));
    
    if (arr == NULL)
        return bad;
    lock_lock(&ctx->lock);
    arr->mem.references = NULL;
    if (memblock_alloc(ctx, &arr->mem, dim0 * dim1 * dim2 * 4, "arr->mem"))
        return NULL;
    arr->shape[0] = dim0;
    arr->shape[1] = dim1;
    arr->shape[2] = dim2;
    if ((size_t) (dim0 * dim1 * dim2) * 4 > 0)
        memmove(arr->mem.mem + 0, data + offset, (size_t) (dim0 * dim1 * dim2) *
                4);
    lock_unlock(&ctx->lock);
    return arr;
}
int futhark_free_f32_3d(struct futhark_context *ctx, struct futhark_f32_3d *arr)
{
    lock_lock(&ctx->lock);
    if (memblock_unref(ctx, &arr->mem, "arr->mem") != 0)
        return 1;
    lock_unlock(&ctx->lock);
    free(arr);
    return 0;
}
int futhark_values_f32_3d(struct futhark_context *ctx,
                          struct futhark_f32_3d *arr, float *data)
{
    lock_lock(&ctx->lock);
    if ((size_t) (arr->shape[0] * arr->shape[1] * arr->shape[2]) * 4 > 0)
        memmove(data + 0, arr->mem.mem + 0, (size_t) (arr->shape[0] *
                                                      arr->shape[1] *
                                                      arr->shape[2]) * 4);
    lock_unlock(&ctx->lock);
    return 0;
}
unsigned char *futhark_values_raw_f32_3d(struct futhark_context *ctx,
                                         struct futhark_f32_3d *arr)
{
    (void) ctx;
    return arr->mem.mem;
}
const int64_t *futhark_shape_f32_3d(struct futhark_context *ctx,
                                    struct futhark_f32_3d *arr)
{
    (void) ctx;
    return arr->shape;
}

static int futrts_entry_main(struct futhark_context *ctx,
                             struct memblock *mem_out_p_39807,
                             struct memblock *mem_out_p_39808,
                             struct memblock *mem_out_p_39809,
                             struct memblock *mem_out_p_39810,
                             struct memblock *mem_out_p_39811,
                             struct memblock *mem_out_p_39812,
                             struct memblock *mem_out_p_39813,
                             struct memblock tketau_mem_37734,
                             struct memblock tketaup1_mem_37735,
                             struct memblock tketaum1_mem_37736,
                             struct memblock dtketau_mem_37737,
                             struct memblock dtketaup1_mem_37738,
                             struct memblock dtketaum1_mem_37739,
                             struct memblock utau_mem_37740,
                             struct memblock vtau_mem_37741,
                             struct memblock wtau_mem_37742,
                             struct memblock maskU_mem_37743,
                             struct memblock maskV_mem_37744,
                             struct memblock maskW_mem_37745,
                             struct memblock dxt_mem_37746,
                             struct memblock dxu_mem_37747,
                             struct memblock dyt_mem_37748,
                             struct memblock dyu_mem_37749,
                             struct memblock dzzt_mem_37750,
                             struct memblock dzzw_mem_37751,
                             struct memblock cost_mem_37752,
                             struct memblock cosu_mem_37753,
                             struct memblock kbot_mem_37754,
                             struct memblock kappaM_mem_37755,
                             struct memblock mxl_mem_37756,
                             struct memblock forc_mem_37757,
                             struct memblock forc_tke_surface_mem_37758,
                             int64_t xdim_31402, int64_t ydim_31403,
                             int64_t zzdim_31404)
{
    (void) ctx;
    
    int err = 0;
    size_t mem_37763_cached_sizze_39814 = 0;
    unsigned char *mem_37763 = NULL;
    size_t mem_37768_cached_sizze_39815 = 0;
    unsigned char *mem_37768 = NULL;
    size_t mem_37773_cached_sizze_39816 = 0;
    unsigned char *mem_37773 = NULL;
    size_t mem_37778_cached_sizze_39817 = 0;
    unsigned char *mem_37778 = NULL;
    size_t mem_38173_cached_sizze_39818 = 0;
    unsigned char *mem_38173 = NULL;
    size_t mem_38188_cached_sizze_39819 = 0;
    unsigned char *mem_38188 = NULL;
    size_t mem_38205_cached_sizze_39820 = 0;
    unsigned char *mem_38205 = NULL;
    size_t mem_38210_cached_sizze_39821 = 0;
    unsigned char *mem_38210 = NULL;
    size_t mem_38215_cached_sizze_39822 = 0;
    unsigned char *mem_38215 = NULL;
    size_t mem_38220_cached_sizze_39823 = 0;
    unsigned char *mem_38220 = NULL;
    size_t mem_38280_cached_sizze_39824 = 0;
    unsigned char *mem_38280 = NULL;
    size_t mem_38327_cached_sizze_39825 = 0;
    unsigned char *mem_38327 = NULL;
    size_t mem_38374_cached_sizze_39826 = 0;
    unsigned char *mem_38374 = NULL;
    size_t mem_38421_cached_sizze_39827 = 0;
    unsigned char *mem_38421 = NULL;
    size_t mem_38482_cached_sizze_39828 = 0;
    unsigned char *mem_38482 = NULL;
    size_t mem_38485_cached_sizze_39829 = 0;
    unsigned char *mem_38485 = NULL;
    size_t mem_38488_cached_sizze_39830 = 0;
    unsigned char *mem_38488 = NULL;
    size_t mem_38491_cached_sizze_39831 = 0;
    unsigned char *mem_38491 = NULL;
    size_t mem_38542_cached_sizze_39832 = 0;
    unsigned char *mem_38542 = NULL;
    size_t mem_38545_cached_sizze_39833 = 0;
    unsigned char *mem_38545 = NULL;
    size_t mem_38548_cached_sizze_39834 = 0;
    unsigned char *mem_38548 = NULL;
    size_t mem_38551_cached_sizze_39835 = 0;
    unsigned char *mem_38551 = NULL;
    size_t mem_38602_cached_sizze_39836 = 0;
    unsigned char *mem_38602 = NULL;
    size_t mem_38617_cached_sizze_39837 = 0;
    unsigned char *mem_38617 = NULL;
    size_t mem_38620_cached_sizze_39838 = 0;
    unsigned char *mem_38620 = NULL;
    size_t mem_38647_cached_sizze_39839 = 0;
    unsigned char *mem_38647 = NULL;
    size_t mem_38650_cached_sizze_39840 = 0;
    unsigned char *mem_38650 = NULL;
    size_t mem_38677_cached_sizze_39841 = 0;
    unsigned char *mem_38677 = NULL;
    size_t mem_38692_cached_sizze_39842 = 0;
    unsigned char *mem_38692 = NULL;
    size_t mem_38695_cached_sizze_39843 = 0;
    unsigned char *mem_38695 = NULL;
    size_t mem_38722_cached_sizze_39844 = 0;
    unsigned char *mem_38722 = NULL;
    size_t mem_38725_cached_sizze_39845 = 0;
    unsigned char *mem_38725 = NULL;
    size_t mem_38752_cached_sizze_39846 = 0;
    unsigned char *mem_38752 = NULL;
    size_t mem_39090_cached_sizze_39847 = 0;
    unsigned char *mem_39090 = NULL;
    size_t mem_39193_cached_sizze_39848 = 0;
    unsigned char *mem_39193 = NULL;
    size_t mem_39296_cached_sizze_39849 = 0;
    unsigned char *mem_39296 = NULL;
    size_t mem_39398_cached_sizze_39850 = 0;
    unsigned char *mem_39398 = NULL;
    size_t mem_39403_cached_sizze_39851 = 0;
    unsigned char *mem_39403 = NULL;
    struct memblock mem_39560;
    
    mem_39560.references = NULL;
    
    struct memblock mem_39555;
    
    mem_39555.references = NULL;
    
    struct memblock mem_39549;
    
    mem_39549.references = NULL;
    
    struct memblock mem_out_39731;
    
    mem_out_39731.references = NULL;
    
    struct memblock mem_out_39730;
    
    mem_out_39730.references = NULL;
    
    struct memblock mem_out_39729;
    
    mem_out_39729.references = NULL;
    
    struct memblock mem_out_39728;
    
    mem_out_39728.references = NULL;
    
    struct memblock mem_out_39727;
    
    mem_out_39727.references = NULL;
    
    struct memblock mem_out_39726;
    
    mem_out_39726.references = NULL;
    
    struct memblock mem_out_39725;
    
    mem_out_39725.references = NULL;
    
    int64_t y_35716 = sub64(xdim_31402, (int64_t) 2);
    int64_t y_35717 = sub64(ydim_31403, (int64_t) 2);
    int64_t y_35718 = sub64(zzdim_31404, (int64_t) 1);
    int64_t binop_x_37759 = xdim_31402 * ydim_31403;
    int64_t binop_x_37760 = zzdim_31404 * binop_x_37759;
    int64_t binop_y_37761 = (int64_t) 4 * binop_x_37760;
    int64_t bytes_37762 = smax64((int64_t) 0, binop_y_37761);
    
    if (mem_37763_cached_sizze_39814 < bytes_37762) {
        err = lexical_realloc(&ctx->error, &mem_37763,
                              &mem_37763_cached_sizze_39814, bytes_37762);
        if (err != FUTHARK_SUCCESS)
            goto cleanup;
    }
    if (mem_37768_cached_sizze_39815 < bytes_37762) {
        err = lexical_realloc(&ctx->error, &mem_37768,
                              &mem_37768_cached_sizze_39815, bytes_37762);
        if (err != FUTHARK_SUCCESS)
            goto cleanup;
    }
    if (mem_37773_cached_sizze_39816 < bytes_37762) {
        err = lexical_realloc(&ctx->error, &mem_37773,
                              &mem_37773_cached_sizze_39816, bytes_37762);
        if (err != FUTHARK_SUCCESS)
            goto cleanup;
    }
    if (mem_37778_cached_sizze_39817 < bytes_37762) {
        err = lexical_realloc(&ctx->error, &mem_37778,
                              &mem_37778_cached_sizze_39817, bytes_37762);
        if (err != FUTHARK_SUCCESS)
            goto cleanup;
    }
    
    int64_t ctx_val_37824 = ydim_31403 * zzdim_31404;
    
    for (int64_t i_37166 = 0; i_37166 < xdim_31402; i_37166++) {
        bool cond_35721 = sle64((int64_t) 2, i_37166);
        bool cond_t_res_35722 = slt64(i_37166, y_35716);
        bool x_35723 = cond_35721 && cond_t_res_35722;
        
        for (int64_t i_37132 = 0; i_37132 < ydim_31403; i_37132++) {
            bool cond_t_res_35726 = sle64((int64_t) 2, i_37132);
            bool x_35727 = x_35723 && cond_t_res_35726;
            bool cond_t_res_35728 = slt64(i_37132, y_35717);
            bool x_35729 = x_35727 && cond_t_res_35728;
            
            for (int64_t i_37128 = 0; i_37128 < zzdim_31404; i_37128++) {
                bool cond_t_res_35732 = slt64(i_37128, y_35718);
                bool x_35733 = x_35729 && cond_t_res_35732;
                float defunc_0_f_res_35734;
                
                if (x_35733) {
                    int64_t i_35735 = add64((int64_t) 1, i_37128);
                    bool x_35736 = sle64((int64_t) 0, i_35735);
                    bool y_35737 = slt64(i_35735, zzdim_31404);
                    bool bounds_check_35738 = x_35736 && y_35737;
                    bool index_certs_35739;
                    
                    if (!bounds_check_35738) {
                        ctx->error =
                            msgprintf("Error: %s%lld%s%lld%s\n\nBacktrace:\n%s",
                                      "Index [", (long long) i_35735,
                                      "] out of bounds for array of shape [",
                                      (long long) zzdim_31404, "].",
                                      "-> #0  sample_programs/tke.fut:108:35-42\n   #1  /prelude/soacs.fut:59:3-10\n   #2  /prelude/array.fut:195:3-17\n   #3  /prelude/functional.fut:39:59-65\n   #4  /prelude/soacs.fut:59:3-10\n   #5  /prelude/array.fut:203:3-34\n   #6  /prelude/functional.fut:39:59-65\n   #7  /prelude/soacs.fut:59:3-10\n   #8  /prelude/array.fut:211:3-39\n   #9  sample_programs/tke.fut:106:17-111:17\n   #10 sample_programs/tke.fut:57:1-363:81\n");
                        err = FUTHARK_PROGRAM_ERROR;
                        goto cleanup;
                    }
                    
                    float y_35740;
                    
                    y_35740 = ((float *) dzzt_mem_37750.mem)[i_35735];
                    
                    float x_35741 = 1.0F / y_35740;
                    float x_35742 = 0.5F * x_35741;
                    float x_35755;
                    
                    x_35755 = ((float *) kappaM_mem_37755.mem)[i_37166 *
                                                               (zzdim_31404 *
                                                                ydim_31403) +
                                                               i_37132 *
                                                               zzdim_31404 +
                                                               i_37128];
                    
                    bool index_certs_35758;
                    
                    if (!bounds_check_35738) {
                        ctx->error =
                            msgprintf("Error: %s%lld%s%lld%s%lld%s%lld%s%lld%s%lld%s\n\nBacktrace:\n%s",
                                      "Index [", (long long) i_37166, ", ",
                                      (long long) i_37132, ", ",
                                      (long long) i_35735,
                                      "] out of bounds for array of shape [",
                                      (long long) xdim_31402, "][",
                                      (long long) ydim_31403, "][",
                                      (long long) zzdim_31404, "].",
                                      "-> #0  sample_programs/tke.fut:109:46-62\n   #1  /prelude/soacs.fut:59:3-10\n   #2  /prelude/array.fut:195:3-17\n   #3  /prelude/functional.fut:39:59-65\n   #4  /prelude/soacs.fut:59:3-10\n   #5  /prelude/array.fut:203:3-34\n   #6  /prelude/functional.fut:39:59-65\n   #7  /prelude/soacs.fut:59:3-10\n   #8  /prelude/array.fut:211:3-39\n   #9  sample_programs/tke.fut:106:17-111:17\n   #10 sample_programs/tke.fut:57:1-363:81\n");
                        err = FUTHARK_PROGRAM_ERROR;
                        goto cleanup;
                    }
                    
                    float y_35759;
                    
                    y_35759 = ((float *) kappaM_mem_37755.mem)[i_37166 *
                                                               (zzdim_31404 *
                                                                ydim_31403) +
                                                               i_37132 *
                                                               zzdim_31404 +
                                                               i_35735];
                    
                    float y_35760 = x_35755 + y_35759;
                    float defunc_0_f_res_t_res_35761 = x_35742 * y_35760;
                    
                    defunc_0_f_res_35734 = defunc_0_f_res_t_res_35761;
                } else {
                    defunc_0_f_res_35734 = 0.0F;
                }
                ((float *) mem_37763)[i_37166 * ctx_val_37824 + i_37132 *
                                      zzdim_31404 + i_37128] =
                    defunc_0_f_res_35734;
            }
        }
        
        bool cond_36502 = sle64((int64_t) 1, i_37166);
        bool x_36504 = cond_t_res_35722 && cond_36502;
        
        for (int64_t i_37140 = 0; i_37140 < ydim_31403; i_37140++) {
            bool cond_t_res_36507 = sle64((int64_t) 2, i_37140);
            bool x_36508 = x_36504 && cond_t_res_36507;
            bool cond_t_res_36509 = slt64(i_37140, y_35717);
            bool x_36510 = x_36508 && cond_t_res_36509;
            
            for (int64_t i_37136 = 0; i_37136 < zzdim_31404; i_37136++) {
                float defunc_0_f_res_36513;
                
                if (x_36510) {
                    float x_36518;
                    
                    x_36518 = ((float *) cost_mem_37752.mem)[i_37140];
                    
                    float y_36523;
                    
                    y_36523 = ((float *) dxt_mem_37746.mem)[i_37166];
                    
                    float dx_36524 = x_36518 * y_36523;
                    float velS_36531;
                    
                    velS_36531 = ((float *) utau_mem_37740.mem)[i_37166 *
                                                                (zzdim_31404 *
                                                                 ydim_31403) +
                                                                i_37140 *
                                                                zzdim_31404 +
                                                                i_37136];
                    
                    int64_t i_36532 = sub64(i_37166, (int64_t) 1);
                    bool x_36533 = sle64((int64_t) 0, i_36532);
                    bool y_36534 = slt64(i_36532, xdim_31402);
                    bool bounds_check_36535 = x_36533 && y_36534;
                    bool index_certs_36538;
                    
                    if (!bounds_check_36535) {
                        ctx->error =
                            msgprintf("Error: %s%lld%s%lld%s%lld%s%lld%s%lld%s%lld%s\n\nBacktrace:\n%s",
                                      "Index [", (long long) i_36532, ", ",
                                      (long long) i_37140, ", ",
                                      (long long) i_37136,
                                      "] out of bounds for array of shape [",
                                      (long long) xdim_31402, "][",
                                      (long long) ydim_31403, "][",
                                      (long long) zzdim_31404, "].",
                                      "-> #0  sample_programs/tke.fut:288:43-56\n   #1  /prelude/soacs.fut:59:3-10\n   #2  /prelude/array.fut:195:3-17\n   #3  /prelude/functional.fut:39:59-65\n   #4  /prelude/soacs.fut:59:3-10\n   #5  /prelude/array.fut:203:3-34\n   #6  /prelude/functional.fut:39:59-65\n   #7  /prelude/soacs.fut:59:3-10\n   #8  /prelude/array.fut:211:3-39\n   #9  sample_programs/tke.fut:282:21-301:21\n   #10 sample_programs/tke.fut:57:1-363:81\n");
                        err = FUTHARK_PROGRAM_ERROR;
                        goto cleanup;
                    }
                    
                    float maskWm1_36539;
                    
                    maskWm1_36539 = ((float *) maskW_mem_37745.mem)[i_36532 *
                                                                    (zzdim_31404 *
                                                                     ydim_31403) +
                                                                    i_37140 *
                                                                    zzdim_31404 +
                                                                    i_37136];
                    
                    float maskWs_36540;
                    
                    maskWs_36540 = ((float *) maskW_mem_37745.mem)[i_37166 *
                                                                   (zzdim_31404 *
                                                                    ydim_31403) +
                                                                   i_37140 *
                                                                   zzdim_31404 +
                                                                   i_37136];
                    
                    int64_t i_36541 = add64((int64_t) 1, i_37166);
                    bool x_36542 = sle64((int64_t) 0, i_36541);
                    bool y_36543 = slt64(i_36541, xdim_31402);
                    bool bounds_check_36544 = x_36542 && y_36543;
                    bool index_certs_36547;
                    
                    if (!bounds_check_36544) {
                        ctx->error =
                            msgprintf("Error: %s%lld%s%lld%s%lld%s%lld%s%lld%s%lld%s\n\nBacktrace:\n%s",
                                      "Index [", (long long) i_36541, ", ",
                                      (long long) i_37140, ", ",
                                      (long long) i_37136,
                                      "] out of bounds for array of shape [",
                                      (long long) xdim_31402, "][",
                                      (long long) ydim_31403, "][",
                                      (long long) zzdim_31404, "].",
                                      "-> #0  sample_programs/tke.fut:290:43-56\n   #1  /prelude/soacs.fut:59:3-10\n   #2  /prelude/array.fut:195:3-17\n   #3  /prelude/functional.fut:39:59-65\n   #4  /prelude/soacs.fut:59:3-10\n   #5  /prelude/array.fut:203:3-34\n   #6  /prelude/functional.fut:39:59-65\n   #7  /prelude/soacs.fut:59:3-10\n   #8  /prelude/array.fut:211:3-39\n   #9  sample_programs/tke.fut:282:21-301:21\n   #10 sample_programs/tke.fut:57:1-363:81\n");
                        err = FUTHARK_PROGRAM_ERROR;
                        goto cleanup;
                    }
                    
                    float maskWp1_36548;
                    
                    maskWp1_36548 = ((float *) maskW_mem_37745.mem)[i_36541 *
                                                                    (zzdim_31404 *
                                                                     ydim_31403) +
                                                                    i_37140 *
                                                                    zzdim_31404 +
                                                                    i_37136];
                    
                    int64_t i_36549 = add64((int64_t) 2, i_37166);
                    bool x_36550 = sle64((int64_t) 0, i_36549);
                    bool y_36551 = slt64(i_36549, xdim_31402);
                    bool bounds_check_36552 = x_36550 && y_36551;
                    bool index_certs_36555;
                    
                    if (!bounds_check_36552) {
                        ctx->error =
                            msgprintf("Error: %s%lld%s%lld%s%lld%s%lld%s%lld%s%lld%s\n\nBacktrace:\n%s",
                                      "Index [", (long long) i_36549, ", ",
                                      (long long) i_37140, ", ",
                                      (long long) i_37136,
                                      "] out of bounds for array of shape [",
                                      (long long) xdim_31402, "][",
                                      (long long) ydim_31403, "][",
                                      (long long) zzdim_31404, "].",
                                      "-> #0  sample_programs/tke.fut:291:43-56\n   #1  /prelude/soacs.fut:59:3-10\n   #2  /prelude/array.fut:195:3-17\n   #3  /prelude/functional.fut:39:59-65\n   #4  /prelude/soacs.fut:59:3-10\n   #5  /prelude/array.fut:203:3-34\n   #6  /prelude/functional.fut:39:59-65\n   #7  /prelude/soacs.fut:59:3-10\n   #8  /prelude/array.fut:211:3-39\n   #9  sample_programs/tke.fut:282:21-301:21\n   #10 sample_programs/tke.fut:57:1-363:81\n");
                        err = FUTHARK_PROGRAM_ERROR;
                        goto cleanup;
                    }
                    
                    float maskwp2_36556;
                    
                    maskwp2_36556 = ((float *) maskW_mem_37745.mem)[i_36549 *
                                                                    (zzdim_31404 *
                                                                     ydim_31403) +
                                                                    i_37140 *
                                                                    zzdim_31404 +
                                                                    i_37136];
                    
                    float varSM1_36557;
                    
                    varSM1_36557 = ((float *) tketau_mem_37734.mem)[i_36532 *
                                                                    (zzdim_31404 *
                                                                     ydim_31403) +
                                                                    i_37140 *
                                                                    zzdim_31404 +
                                                                    i_37136];
                    
                    float varS_36558;
                    
                    varS_36558 = ((float *) tketau_mem_37734.mem)[i_37166 *
                                                                  (zzdim_31404 *
                                                                   ydim_31403) +
                                                                  i_37140 *
                                                                  zzdim_31404 +
                                                                  i_37136];
                    
                    float varSP1_36559;
                    
                    varSP1_36559 = ((float *) tketau_mem_37734.mem)[i_36541 *
                                                                    (zzdim_31404 *
                                                                     ydim_31403) +
                                                                    i_37140 *
                                                                    zzdim_31404 +
                                                                    i_37136];
                    
                    float varSP2_36560;
                    
                    varSP2_36560 = ((float *) tketau_mem_37734.mem)[i_36549 *
                                                                    (zzdim_31404 *
                                                                     ydim_31403) +
                                                                    i_37140 *
                                                                    zzdim_31404 +
                                                                    i_37136];
                    
                    int64_t y_36561 = sub64(xdim_31402, (int64_t) 1);
                    bool cond_36562 = slt64(i_37166, y_36561);
                    float maskUtr_36563;
                    
                    if (cond_36562) {
                        float maskUtr_t_res_36564 = maskWs_36540 *
                              maskWp1_36548;
                        
                        maskUtr_36563 = maskUtr_t_res_36564;
                    } else {
                        maskUtr_36563 = 0.0F;
                    }
                    
                    float maskUtrP1_36565;
                    
                    if (cond_36562) {
                        float maskUtrP1_t_res_36566 = maskWp1_36548 *
                              maskwp2_36556;
                        
                        maskUtrP1_36565 = maskUtrP1_t_res_36566;
                    } else {
                        maskUtrP1_36565 = 0.0F;
                    }
                    
                    float maskUtrM1_36567;
                    
                    if (cond_36562) {
                        float maskUtrM1_t_res_36568 = maskWm1_36539 *
                              maskWs_36540;
                        
                        maskUtrM1_36567 = maskUtrM1_t_res_36568;
                    } else {
                        maskUtrM1_36567 = 0.0F;
                    }
                    
                    float real_abs_arg_37003 = velS_36531 / dx_36524;
                    float abs_res_37004 = (float) fabs(real_abs_arg_37003);
                    float x_37005 = varSP2_36560 - varSP1_36559;
                    float rjp_37006 = maskUtrP1_36565 * x_37005;
                    float x_37007 = varSP1_36559 - varS_36558;
                    float rj_37008 = maskUtr_36563 * x_37007;
                    float x_37009 = varS_36558 - varSM1_36557;
                    float rjm_37010 = maskUtrM1_36567 * x_37009;
                    float abs_res_37011 = (float) fabs(rj_37008);
                    bool cond_37012 = abs_res_37011 < 1.0e-20F;
                    float divisor_37013;
                    
                    if (cond_37012) {
                        divisor_37013 = 1.0e-20F;
                    } else {
                        divisor_37013 = rj_37008;
                    }
                    
                    bool cond_37014 = 0.0F < velS_36531;
                    float cr_37015;
                    
                    if (cond_37014) {
                        float cr_t_res_37016 = rjm_37010 / divisor_37013;
                        
                        cr_37015 = cr_t_res_37016;
                    } else {
                        float cr_f_res_37017 = rjp_37006 / divisor_37013;
                        
                        cr_37015 = cr_f_res_37017;
                    }
                    
                    float min_res_37018 = fmin32(2.0F, cr_37015);
                    float real_min_arg_37019 = 2.0F * cr_37015;
                    float min_res_37020 = fmin32(1.0F, real_min_arg_37019);
                    float max_res_37021 = fmax32(min_res_37018, min_res_37020);
                    float max_res_37022 = fmax32(0.0F, max_res_37021);
                    float y_37023 = varS_36558 + varSP1_36559;
                    float x_37024 = velS_36531 * y_37023;
                    float x_37025 = 0.5F * x_37024;
                    float abs_res_37026 = (float) fabs(velS_36531);
                    float x_37027 = 1.0F - max_res_37022;
                    float y_37028 = abs_res_37004 * max_res_37022;
                    float y_37029 = x_37027 + y_37028;
                    float x_37030 = abs_res_37026 * y_37029;
                    float x_37031 = rj_37008 * x_37030;
                    float y_37032 = 0.5F * x_37031;
                    float calcflux_res_37033 = x_37025 - y_37032;
                    
                    defunc_0_f_res_36513 = calcflux_res_37033;
                } else {
                    defunc_0_f_res_36513 = 0.0F;
                }
                ((float *) mem_37768)[i_37166 * ctx_val_37824 + i_37140 *
                                      zzdim_31404 + i_37136] =
                    defunc_0_f_res_36513;
            }
        }
        for (int64_t i_37148 = 0; i_37148 < ydim_31403; i_37148++) {
            bool cond_36576 = sle64((int64_t) 1, i_37148);
            bool cond_t_res_36577 = slt64(i_37148, y_35717);
            bool x_36578 = cond_36576 && cond_t_res_36577;
            bool x_36579 = cond_35721 && x_36578;
            bool x_36580 = cond_t_res_35722 && x_36579;
            
            for (int64_t i_37144 = 0; i_37144 < zzdim_31404; i_37144++) {
                float defunc_0_f_res_36583;
                
                if (x_36580) {
                    float x_36588;
                    
                    x_36588 = ((float *) cost_mem_37752.mem)[i_37148];
                    
                    float y_36589;
                    
                    y_36589 = ((float *) dyt_mem_37748.mem)[i_37148];
                    
                    float dx_36590 = x_36588 * y_36589;
                    float velS_36600;
                    
                    velS_36600 = ((float *) vtau_mem_37741.mem)[i_37166 *
                                                                (zzdim_31404 *
                                                                 ydim_31403) +
                                                                i_37148 *
                                                                zzdim_31404 +
                                                                i_37144];
                    
                    int64_t i_36601 = sub64(i_37148, (int64_t) 1);
                    bool x_36602 = sle64((int64_t) 0, i_36601);
                    bool y_36603 = slt64(i_36601, ydim_31403);
                    bool bounds_check_36604 = x_36602 && y_36603;
                    bool index_certs_36606;
                    
                    if (!bounds_check_36604) {
                        ctx->error =
                            msgprintf("Error: %s%lld%s%lld%s%lld%s%lld%s%lld%s%lld%s\n\nBacktrace:\n%s",
                                      "Index [", (long long) i_37166, ", ",
                                      (long long) i_36601, ", ",
                                      (long long) i_37144,
                                      "] out of bounds for array of shape [",
                                      (long long) xdim_31402, "][",
                                      (long long) ydim_31403, "][",
                                      (long long) zzdim_31404, "].",
                                      "-> #0  sample_programs/tke.fut:307:43-56\n   #1  /prelude/soacs.fut:59:3-10\n   #2  /prelude/array.fut:195:3-17\n   #3  /prelude/functional.fut:39:59-65\n   #4  /prelude/soacs.fut:59:3-10\n   #5  /prelude/array.fut:203:3-34\n   #6  /prelude/functional.fut:39:59-65\n   #7  /prelude/soacs.fut:59:3-10\n   #8  /prelude/array.fut:211:3-39\n   #9  sample_programs/tke.fut:302:22-320:21\n   #10 sample_programs/tke.fut:57:1-363:81\n");
                        err = FUTHARK_PROGRAM_ERROR;
                        goto cleanup;
                    }
                    
                    float maskWm1_36607;
                    
                    maskWm1_36607 = ((float *) maskW_mem_37745.mem)[i_37166 *
                                                                    (zzdim_31404 *
                                                                     ydim_31403) +
                                                                    i_36601 *
                                                                    zzdim_31404 +
                                                                    i_37144];
                    
                    float maskWs_36608;
                    
                    maskWs_36608 = ((float *) maskW_mem_37745.mem)[i_37166 *
                                                                   (zzdim_31404 *
                                                                    ydim_31403) +
                                                                   i_37148 *
                                                                   zzdim_31404 +
                                                                   i_37144];
                    
                    int64_t i_36609 = add64((int64_t) 1, i_37148);
                    bool x_36610 = sle64((int64_t) 0, i_36609);
                    bool y_36611 = slt64(i_36609, ydim_31403);
                    bool bounds_check_36612 = x_36610 && y_36611;
                    bool index_certs_36614;
                    
                    if (!bounds_check_36612) {
                        ctx->error =
                            msgprintf("Error: %s%lld%s%lld%s%lld%s%lld%s%lld%s%lld%s\n\nBacktrace:\n%s",
                                      "Index [", (long long) i_37166, ", ",
                                      (long long) i_36609, ", ",
                                      (long long) i_37144,
                                      "] out of bounds for array of shape [",
                                      (long long) xdim_31402, "][",
                                      (long long) ydim_31403, "][",
                                      (long long) zzdim_31404, "].",
                                      "-> #0  sample_programs/tke.fut:309:43-56\n   #1  /prelude/soacs.fut:59:3-10\n   #2  /prelude/array.fut:195:3-17\n   #3  /prelude/functional.fut:39:59-65\n   #4  /prelude/soacs.fut:59:3-10\n   #5  /prelude/array.fut:203:3-34\n   #6  /prelude/functional.fut:39:59-65\n   #7  /prelude/soacs.fut:59:3-10\n   #8  /prelude/array.fut:211:3-39\n   #9  sample_programs/tke.fut:302:22-320:21\n   #10 sample_programs/tke.fut:57:1-363:81\n");
                        err = FUTHARK_PROGRAM_ERROR;
                        goto cleanup;
                    }
                    
                    float maskWp1_36615;
                    
                    maskWp1_36615 = ((float *) maskW_mem_37745.mem)[i_37166 *
                                                                    (zzdim_31404 *
                                                                     ydim_31403) +
                                                                    i_36609 *
                                                                    zzdim_31404 +
                                                                    i_37144];
                    
                    int64_t i_36616 = add64((int64_t) 2, i_37148);
                    bool x_36617 = sle64((int64_t) 0, i_36616);
                    bool y_36618 = slt64(i_36616, ydim_31403);
                    bool bounds_check_36619 = x_36617 && y_36618;
                    bool index_certs_36621;
                    
                    if (!bounds_check_36619) {
                        ctx->error =
                            msgprintf("Error: %s%lld%s%lld%s%lld%s%lld%s%lld%s%lld%s\n\nBacktrace:\n%s",
                                      "Index [", (long long) i_37166, ", ",
                                      (long long) i_36616, ", ",
                                      (long long) i_37144,
                                      "] out of bounds for array of shape [",
                                      (long long) xdim_31402, "][",
                                      (long long) ydim_31403, "][",
                                      (long long) zzdim_31404, "].",
                                      "-> #0  sample_programs/tke.fut:310:43-56\n   #1  /prelude/soacs.fut:59:3-10\n   #2  /prelude/array.fut:195:3-17\n   #3  /prelude/functional.fut:39:59-65\n   #4  /prelude/soacs.fut:59:3-10\n   #5  /prelude/array.fut:203:3-34\n   #6  /prelude/functional.fut:39:59-65\n   #7  /prelude/soacs.fut:59:3-10\n   #8  /prelude/array.fut:211:3-39\n   #9  sample_programs/tke.fut:302:22-320:21\n   #10 sample_programs/tke.fut:57:1-363:81\n");
                        err = FUTHARK_PROGRAM_ERROR;
                        goto cleanup;
                    }
                    
                    float maskwp2_36622;
                    
                    maskwp2_36622 = ((float *) maskW_mem_37745.mem)[i_37166 *
                                                                    (zzdim_31404 *
                                                                     ydim_31403) +
                                                                    i_36616 *
                                                                    zzdim_31404 +
                                                                    i_37144];
                    
                    float varSM1_36623;
                    
                    varSM1_36623 = ((float *) tketau_mem_37734.mem)[i_37166 *
                                                                    (zzdim_31404 *
                                                                     ydim_31403) +
                                                                    i_36601 *
                                                                    zzdim_31404 +
                                                                    i_37144];
                    
                    float varS_36624;
                    
                    varS_36624 = ((float *) tketau_mem_37734.mem)[i_37166 *
                                                                  (zzdim_31404 *
                                                                   ydim_31403) +
                                                                  i_37148 *
                                                                  zzdim_31404 +
                                                                  i_37144];
                    
                    float varSP1_36625;
                    
                    varSP1_36625 = ((float *) tketau_mem_37734.mem)[i_37166 *
                                                                    (zzdim_31404 *
                                                                     ydim_31403) +
                                                                    i_36609 *
                                                                    zzdim_31404 +
                                                                    i_37144];
                    
                    float varSP2_36626;
                    
                    varSP2_36626 = ((float *) tketau_mem_37734.mem)[i_37166 *
                                                                    (zzdim_31404 *
                                                                     ydim_31403) +
                                                                    i_36616 *
                                                                    zzdim_31404 +
                                                                    i_37144];
                    
                    int64_t y_36627 = sub64(ydim_31403, (int64_t) 1);
                    bool cond_36628 = slt64(i_37148, y_36627);
                    float maskVtr_36629;
                    
                    if (cond_36628) {
                        float maskVtr_t_res_36630 = maskWs_36608 *
                              maskWp1_36615;
                        
                        maskVtr_36629 = maskVtr_t_res_36630;
                    } else {
                        maskVtr_36629 = 0.0F;
                    }
                    
                    float maskVtrP1_36631;
                    
                    if (cond_36628) {
                        float maskVtrP1_t_res_36632 = maskWp1_36615 *
                              maskwp2_36622;
                        
                        maskVtrP1_36631 = maskVtrP1_t_res_36632;
                    } else {
                        maskVtrP1_36631 = 0.0F;
                    }
                    
                    float maskVtrM1_36633;
                    
                    if (cond_36628) {
                        float maskVtrM1_t_res_36634 = maskWm1_36607 *
                              maskWs_36608;
                        
                        maskVtrM1_36633 = maskVtrM1_t_res_36634;
                    } else {
                        maskVtrM1_36633 = 0.0F;
                    }
                    
                    float calcflux_arg_36635;
                    
                    calcflux_arg_36635 =
                        ((float *) cosu_mem_37753.mem)[i_37148];
                    
                    float scaledVel_37045 = velS_36600 * calcflux_arg_36635;
                    float real_abs_arg_37047 = scaledVel_37045 / dx_36590;
                    float abs_res_37048 = (float) fabs(real_abs_arg_37047);
                    float x_37049 = varSP2_36626 - varSP1_36625;
                    float rjp_37050 = maskVtrP1_36631 * x_37049;
                    float x_37051 = varSP1_36625 - varS_36624;
                    float rj_37052 = maskVtr_36629 * x_37051;
                    float x_37053 = varS_36624 - varSM1_36623;
                    float rjm_37054 = maskVtrM1_36633 * x_37053;
                    float abs_res_37055 = (float) fabs(rj_37052);
                    bool cond_37056 = abs_res_37055 < 1.0e-20F;
                    float divisor_37057;
                    
                    if (cond_37056) {
                        divisor_37057 = 1.0e-20F;
                    } else {
                        divisor_37057 = rj_37052;
                    }
                    
                    bool cond_37058 = 0.0F < velS_36600;
                    float cr_37059;
                    
                    if (cond_37058) {
                        float cr_t_res_37060 = rjm_37054 / divisor_37057;
                        
                        cr_37059 = cr_t_res_37060;
                    } else {
                        float cr_f_res_37061 = rjp_37050 / divisor_37057;
                        
                        cr_37059 = cr_f_res_37061;
                    }
                    
                    float min_res_37062 = fmin32(2.0F, cr_37059);
                    float real_min_arg_37063 = 2.0F * cr_37059;
                    float min_res_37064 = fmin32(1.0F, real_min_arg_37063);
                    float max_res_37065 = fmax32(min_res_37062, min_res_37064);
                    float max_res_37066 = fmax32(0.0F, max_res_37065);
                    float y_37067 = varS_36624 + varSP1_36625;
                    float x_37068 = scaledVel_37045 * y_37067;
                    float x_37069 = 0.5F * x_37068;
                    float abs_res_37070 = (float) fabs(scaledVel_37045);
                    float x_37071 = 1.0F - max_res_37066;
                    float y_37072 = abs_res_37048 * max_res_37066;
                    float y_37073 = x_37071 + y_37072;
                    float x_37074 = abs_res_37070 * y_37073;
                    float x_37075 = rj_37052 * x_37074;
                    float y_37076 = 0.5F * x_37075;
                    float calcflux_res_37077 = x_37069 - y_37076;
                    
                    defunc_0_f_res_36583 = calcflux_res_37077;
                } else {
                    defunc_0_f_res_36583 = 0.0F;
                }
                ((float *) mem_37773)[i_37166 * ctx_val_37824 + i_37148 *
                                      zzdim_31404 + i_37144] =
                    defunc_0_f_res_36583;
            }
        }
        for (int64_t i_37156 = 0; i_37156 < ydim_31403; i_37156++) {
            bool cond_t_res_36643 = sle64((int64_t) 2, i_37156);
            bool cond_t_res_36644 = slt64(i_37156, y_35717);
            
            for (int64_t i_37152 = 0; i_37152 < zzdim_31404; i_37152++) {
                bool cond_36647 = slt64(i_37152, y_35718);
                bool x_36648 = cond_35721 && cond_36647;
                bool x_36649 = cond_t_res_35722 && x_36648;
                bool x_36650 = cond_t_res_36643 && x_36649;
                bool x_36651 = cond_t_res_36644 && x_36650;
                float defunc_0_f_res_36652;
                
                if (x_36651) {
                    float velS_36665;
                    
                    velS_36665 = ((float *) wtau_mem_37742.mem)[i_37166 *
                                                                (zzdim_31404 *
                                                                 ydim_31403) +
                                                                i_37156 *
                                                                zzdim_31404 +
                                                                i_37152];
                    
                    bool cond_36666 = i_37152 == (int64_t) 0;
                    bool cond_36667 = !cond_36666;
                    float varSM1_36668;
                    
                    if (cond_36667) {
                        int64_t i_36669 = sub64(i_37152, (int64_t) 1);
                        bool x_36670 = sle64((int64_t) 0, i_36669);
                        bool y_36671 = slt64(i_36669, zzdim_31404);
                        bool bounds_check_36672 = x_36670 && y_36671;
                        bool index_certs_36675;
                        
                        if (!bounds_check_36672) {
                            ctx->error =
                                msgprintf("Error: %s%lld%s%lld%s%lld%s%lld%s%lld%s%lld%s\n\nBacktrace:\n%s",
                                          "Index [", (long long) i_37166, ", ",
                                          (long long) i_37156, ", ",
                                          (long long) i_36669,
                                          "] out of bounds for array of shape [",
                                          (long long) xdim_31402, "][",
                                          (long long) ydim_31403, "][",
                                          (long long) zzdim_31404, "].",
                                          "-> #0  sample_programs/tke.fut:325:57-71\n   #1  /prelude/soacs.fut:59:3-10\n   #2  /prelude/array.fut:195:3-17\n   #3  /prelude/functional.fut:39:59-65\n   #4  /prelude/soacs.fut:59:3-10\n   #5  /prelude/array.fut:203:3-34\n   #6  /prelude/functional.fut:39:59-65\n   #7  /prelude/soacs.fut:59:3-10\n   #8  /prelude/array.fut:211:3-39\n   #9  sample_programs/tke.fut:321:20-339:21\n   #10 sample_programs/tke.fut:57:1-363:81\n");
                            err = FUTHARK_PROGRAM_ERROR;
                            goto cleanup;
                        }
                        
                        float varSM1_t_res_36676;
                        
                        varSM1_t_res_36676 =
                            ((float *) tketau_mem_37734.mem)[i_37166 *
                                                             (zzdim_31404 *
                                                              ydim_31403) +
                                                             i_37156 *
                                                             zzdim_31404 +
                                                             i_36669];
                        varSM1_36668 = varSM1_t_res_36676;
                    } else {
                        varSM1_36668 = 0.0F;
                    }
                    
                    float varS_36677;
                    
                    varS_36677 = ((float *) tketau_mem_37734.mem)[i_37166 *
                                                                  (zzdim_31404 *
                                                                   ydim_31403) +
                                                                  i_37156 *
                                                                  zzdim_31404 +
                                                                  i_37152];
                    
                    int64_t y_36678 = sub64(zzdim_31404, (int64_t) 2);
                    bool cond_36679 = slt64(i_37152, y_36678);
                    float varSP2_36680;
                    
                    if (cond_36679) {
                        int64_t i_36681 = add64((int64_t) 2, i_37152);
                        bool x_36682 = sle64((int64_t) 0, i_36681);
                        bool y_36683 = slt64(i_36681, zzdim_31404);
                        bool bounds_check_36684 = x_36682 && y_36683;
                        bool index_certs_36687;
                        
                        if (!bounds_check_36684) {
                            ctx->error =
                                msgprintf("Error: %s%lld%s%lld%s%lld%s%lld%s%lld%s%lld%s\n\nBacktrace:\n%s",
                                          "Index [", (long long) i_37166, ", ",
                                          (long long) i_37156, ", ",
                                          (long long) i_36681,
                                          "] out of bounds for array of shape [",
                                          (long long) xdim_31402, "][",
                                          (long long) ydim_31403, "][",
                                          (long long) zzdim_31404, "].",
                                          "-> #0  sample_programs/tke.fut:327:61-75\n   #1  /prelude/soacs.fut:59:3-10\n   #2  /prelude/array.fut:195:3-17\n   #3  /prelude/functional.fut:39:59-65\n   #4  /prelude/soacs.fut:59:3-10\n   #5  /prelude/array.fut:203:3-34\n   #6  /prelude/functional.fut:39:59-65\n   #7  /prelude/soacs.fut:59:3-10\n   #8  /prelude/array.fut:211:3-39\n   #9  sample_programs/tke.fut:321:20-339:21\n   #10 sample_programs/tke.fut:57:1-363:81\n");
                            err = FUTHARK_PROGRAM_ERROR;
                            goto cleanup;
                        }
                        
                        float varSP2_t_res_36688;
                        
                        varSP2_t_res_36688 =
                            ((float *) tketau_mem_37734.mem)[i_37166 *
                                                             (zzdim_31404 *
                                                              ydim_31403) +
                                                             i_37156 *
                                                             zzdim_31404 +
                                                             i_36681];
                        varSP2_36680 = varSP2_t_res_36688;
                    } else {
                        varSP2_36680 = 0.0F;
                    }
                    
                    int64_t i_36689 = add64((int64_t) 1, i_37152);
                    bool x_36690 = sle64((int64_t) 0, i_36689);
                    bool y_36691 = slt64(i_36689, zzdim_31404);
                    bool bounds_check_36692 = x_36690 && y_36691;
                    bool index_certs_36695;
                    
                    if (!bounds_check_36692) {
                        ctx->error =
                            msgprintf("Error: %s%lld%s%lld%s%lld%s%lld%s%lld%s%lld%s\n\nBacktrace:\n%s",
                                      "Index [", (long long) i_37166, ", ",
                                      (long long) i_37156, ", ",
                                      (long long) i_36689,
                                      "] out of bounds for array of shape [",
                                      (long long) xdim_31402, "][",
                                      (long long) ydim_31403, "][",
                                      (long long) zzdim_31404, "].",
                                      "-> #0  sample_programs/tke.fut:328:42-56\n   #1  /prelude/soacs.fut:59:3-10\n   #2  /prelude/array.fut:195:3-17\n   #3  /prelude/functional.fut:39:59-65\n   #4  /prelude/soacs.fut:59:3-10\n   #5  /prelude/array.fut:203:3-34\n   #6  /prelude/functional.fut:39:59-65\n   #7  /prelude/soacs.fut:59:3-10\n   #8  /prelude/array.fut:211:3-39\n   #9  sample_programs/tke.fut:321:20-339:21\n   #10 sample_programs/tke.fut:57:1-363:81\n");
                        err = FUTHARK_PROGRAM_ERROR;
                        goto cleanup;
                    }
                    
                    float varSP1_36696;
                    
                    varSP1_36696 = ((float *) tketau_mem_37734.mem)[i_37166 *
                                                                    (zzdim_31404 *
                                                                     ydim_31403) +
                                                                    i_37156 *
                                                                    zzdim_31404 +
                                                                    i_36689];
                    
                    float maskWm1_36697;
                    
                    if (cond_36667) {
                        int64_t i_36698 = sub64(i_37152, (int64_t) 1);
                        bool x_36699 = sle64((int64_t) 0, i_36698);
                        bool y_36700 = slt64(i_36698, zzdim_31404);
                        bool bounds_check_36701 = x_36699 && y_36700;
                        bool index_certs_36704;
                        
                        if (!bounds_check_36701) {
                            ctx->error =
                                msgprintf("Error: %s%lld%s%lld%s%lld%s%lld%s%lld%s%lld%s\n\nBacktrace:\n%s",
                                          "Index [", (long long) i_37166, ", ",
                                          (long long) i_37156, ", ",
                                          (long long) i_36698,
                                          "] out of bounds for array of shape [",
                                          (long long) xdim_31402, "][",
                                          (long long) ydim_31403, "][",
                                          (long long) zzdim_31404, "].",
                                          "-> #0  sample_programs/tke.fut:329:58-71\n   #1  /prelude/soacs.fut:59:3-10\n   #2  /prelude/array.fut:195:3-17\n   #3  /prelude/functional.fut:39:59-65\n   #4  /prelude/soacs.fut:59:3-10\n   #5  /prelude/array.fut:203:3-34\n   #6  /prelude/functional.fut:39:59-65\n   #7  /prelude/soacs.fut:59:3-10\n   #8  /prelude/array.fut:211:3-39\n   #9  sample_programs/tke.fut:321:20-339:21\n   #10 sample_programs/tke.fut:57:1-363:81\n");
                            err = FUTHARK_PROGRAM_ERROR;
                            goto cleanup;
                        }
                        
                        float maskWm1_t_res_36705;
                        
                        maskWm1_t_res_36705 =
                            ((float *) maskW_mem_37745.mem)[i_37166 *
                                                            (zzdim_31404 *
                                                             ydim_31403) +
                                                            i_37156 *
                                                            zzdim_31404 +
                                                            i_36698];
                        maskWm1_36697 = maskWm1_t_res_36705;
                    } else {
                        maskWm1_36697 = 0.0F;
                    }
                    
                    float maskWs_36706;
                    
                    maskWs_36706 = ((float *) maskW_mem_37745.mem)[i_37166 *
                                                                   (zzdim_31404 *
                                                                    ydim_31403) +
                                                                   i_37156 *
                                                                   zzdim_31404 +
                                                                   i_37152];
                    
                    float maskWp1_36707;
                    
                    maskWp1_36707 = ((float *) maskW_mem_37745.mem)[i_37166 *
                                                                    (zzdim_31404 *
                                                                     ydim_31403) +
                                                                    i_37156 *
                                                                    zzdim_31404 +
                                                                    i_36689];
                    
                    float maskwp2_36708;
                    
                    if (cond_36679) {
                        int64_t i_36709 = add64((int64_t) 2, i_37152);
                        bool x_36710 = sle64((int64_t) 0, i_36709);
                        bool y_36711 = slt64(i_36709, zzdim_31404);
                        bool bounds_check_36712 = x_36710 && y_36711;
                        bool index_certs_36715;
                        
                        if (!bounds_check_36712) {
                            ctx->error =
                                msgprintf("Error: %s%lld%s%lld%s%lld%s%lld%s%lld%s%lld%s\n\nBacktrace:\n%s",
                                          "Index [", (long long) i_37166, ", ",
                                          (long long) i_37156, ", ",
                                          (long long) i_36709,
                                          "] out of bounds for array of shape [",
                                          (long long) xdim_31402, "][",
                                          (long long) ydim_31403, "][",
                                          (long long) zzdim_31404, "].",
                                          "-> #0  sample_programs/tke.fut:332:62-75\n   #1  /prelude/soacs.fut:59:3-10\n   #2  /prelude/array.fut:195:3-17\n   #3  /prelude/functional.fut:39:59-65\n   #4  /prelude/soacs.fut:59:3-10\n   #5  /prelude/array.fut:203:3-34\n   #6  /prelude/functional.fut:39:59-65\n   #7  /prelude/soacs.fut:59:3-10\n   #8  /prelude/array.fut:211:3-39\n   #9  sample_programs/tke.fut:321:20-339:21\n   #10 sample_programs/tke.fut:57:1-363:81\n");
                            err = FUTHARK_PROGRAM_ERROR;
                            goto cleanup;
                        }
                        
                        float maskwp2_t_res_36716;
                        
                        maskwp2_t_res_36716 =
                            ((float *) maskW_mem_37745.mem)[i_37166 *
                                                            (zzdim_31404 *
                                                             ydim_31403) +
                                                            i_37156 *
                                                            zzdim_31404 +
                                                            i_36709];
                        maskwp2_36708 = maskwp2_t_res_36716;
                    } else {
                        maskwp2_36708 = 0.0F;
                    }
                    
                    float maskWtr_36717 = maskWs_36706 * maskWp1_36707;
                    float maskWtrP1_36718 = maskWp1_36707 * maskwp2_36708;
                    float maskWtrM1_36719 = maskWm1_36697 * maskWs_36706;
                    float dx_36721;
                    
                    dx_36721 = ((float *) dzzw_mem_37751.mem)[i_37152];
                    
                    float real_abs_arg_37091 = velS_36665 / dx_36721;
                    float abs_res_37092 = (float) fabs(real_abs_arg_37091);
                    float x_37093 = varSP2_36680 - varSP1_36696;
                    float rjp_37094 = maskWtrP1_36718 * x_37093;
                    float x_37095 = varSP1_36696 - varS_36677;
                    float rj_37096 = maskWtr_36717 * x_37095;
                    float x_37097 = varS_36677 - varSM1_36668;
                    float rjm_37098 = maskWtrM1_36719 * x_37097;
                    float abs_res_37099 = (float) fabs(rj_37096);
                    bool cond_37100 = abs_res_37099 < 1.0e-20F;
                    float divisor_37101;
                    
                    if (cond_37100) {
                        divisor_37101 = 1.0e-20F;
                    } else {
                        divisor_37101 = rj_37096;
                    }
                    
                    bool cond_37102 = 0.0F < velS_36665;
                    float cr_37103;
                    
                    if (cond_37102) {
                        float cr_t_res_37104 = rjm_37098 / divisor_37101;
                        
                        cr_37103 = cr_t_res_37104;
                    } else {
                        float cr_f_res_37105 = rjp_37094 / divisor_37101;
                        
                        cr_37103 = cr_f_res_37105;
                    }
                    
                    float min_res_37106 = fmin32(2.0F, cr_37103);
                    float real_min_arg_37107 = 2.0F * cr_37103;
                    float min_res_37108 = fmin32(1.0F, real_min_arg_37107);
                    float max_res_37109 = fmax32(min_res_37106, min_res_37108);
                    float max_res_37110 = fmax32(0.0F, max_res_37109);
                    float y_37111 = varS_36677 + varSP1_36696;
                    float x_37112 = velS_36665 * y_37111;
                    float x_37113 = 0.5F * x_37112;
                    float abs_res_37114 = (float) fabs(velS_36665);
                    float x_37115 = 1.0F - max_res_37110;
                    float y_37116 = abs_res_37092 * max_res_37110;
                    float y_37117 = x_37115 + y_37116;
                    float x_37118 = abs_res_37114 * y_37117;
                    float x_37119 = rj_37096 * x_37118;
                    float y_37120 = 0.5F * x_37119;
                    float calcflux_res_37121 = x_37113 - y_37120;
                    
                    defunc_0_f_res_36652 = calcflux_res_37121;
                } else {
                    defunc_0_f_res_36652 = 0.0F;
                }
                ((float *) mem_37778)[i_37166 * ctx_val_37824 + i_37156 *
                                      zzdim_31404 + i_37152] =
                    defunc_0_f_res_36652;
            }
        }
    }
    
    int64_t y_36392 = sub64(ydim_31403, (int64_t) 1);
    int64_t y_36352 = sub64(xdim_31402, (int64_t) 1);
    int32_t i64_res_35854 = sext_i64_i32(ydim_31403);
    int32_t y_35855 = sub32(i64_res_35854, 2);
    int32_t i64_res_35852 = sext_i64_i32(xdim_31402);
    int32_t y_35853 = sub32(i64_res_35852, 2);
    int64_t binop_y_38171 = (int64_t) 4 * zzdim_31404;
    int64_t bytes_38172 = smax64((int64_t) 0, binop_y_38171);
    
    if (mem_38173_cached_sizze_39818 < bytes_38172) {
        err = lexical_realloc(&ctx->error, &mem_38173,
                              &mem_38173_cached_sizze_39818, bytes_38172);
        if (err != FUTHARK_SUCCESS)
            goto cleanup;
    }
    for (int64_t i_37173 = 0; i_37173 < zzdim_31404; i_37173++) {
        int32_t defunc_0_f_res_35851 = sext_i64_i32(i_37173);
        
        ((int32_t *) mem_38173)[i_37173] = defunc_0_f_res_35851;
    }
    
    int64_t binop_y_38186 = (int64_t) 4 * ydim_31403;
    int64_t bytes_38187 = smax64((int64_t) 0, binop_y_38186);
    
    if (mem_38188_cached_sizze_39819 < bytes_38187) {
        err = lexical_realloc(&ctx->error, &mem_38188,
                              &mem_38188_cached_sizze_39819, bytes_38187);
        if (err != FUTHARK_SUCCESS)
            goto cleanup;
    }
    for (int64_t i_37177 = 0; i_37177 < ydim_31403; i_37177++) {
        int32_t defunc_0_f_res_35847 = sext_i64_i32(i_37177);
        
        ((int32_t *) mem_38188)[i_37177] = defunc_0_f_res_35847;
    }
    if (mem_38205_cached_sizze_39820 < bytes_37762) {
        err = lexical_realloc(&ctx->error, &mem_38205,
                              &mem_38205_cached_sizze_39820, bytes_37762);
        if (err != FUTHARK_SUCCESS)
            goto cleanup;
    }
    if (mem_38210_cached_sizze_39821 < bytes_37762) {
        err = lexical_realloc(&ctx->error, &mem_38210,
                              &mem_38210_cached_sizze_39821, bytes_37762);
        if (err != FUTHARK_SUCCESS)
            goto cleanup;
    }
    if (mem_38215_cached_sizze_39822 < bytes_37762) {
        err = lexical_realloc(&ctx->error, &mem_38215,
                              &mem_38215_cached_sizze_39822, bytes_37762);
        if (err != FUTHARK_SUCCESS)
            goto cleanup;
    }
    if (mem_38220_cached_sizze_39823 < bytes_37762) {
        err = lexical_realloc(&ctx->error, &mem_38220,
                              &mem_38220_cached_sizze_39823, bytes_37762);
        if (err != FUTHARK_SUCCESS)
            goto cleanup;
    }
    
    int64_t binop_x_38277 = ydim_31403 * zzdim_31404;
    int64_t binop_y_38278 = (int64_t) 4 * binop_x_38277;
    int64_t bytes_38279 = smax64((int64_t) 0, binop_y_38278);
    
    if (mem_38280_cached_sizze_39824 < bytes_38279) {
        err = lexical_realloc(&ctx->error, &mem_38280,
                              &mem_38280_cached_sizze_39824, bytes_38279);
        if (err != FUTHARK_SUCCESS)
            goto cleanup;
    }
    if (mem_38327_cached_sizze_39825 < bytes_38279) {
        err = lexical_realloc(&ctx->error, &mem_38327,
                              &mem_38327_cached_sizze_39825, bytes_38279);
        if (err != FUTHARK_SUCCESS)
            goto cleanup;
    }
    if (mem_38374_cached_sizze_39826 < bytes_38279) {
        err = lexical_realloc(&ctx->error, &mem_38374,
                              &mem_38374_cached_sizze_39826, bytes_38279);
        if (err != FUTHARK_SUCCESS)
            goto cleanup;
    }
    if (mem_38421_cached_sizze_39827 < bytes_38279) {
        err = lexical_realloc(&ctx->error, &mem_38421,
                              &mem_38421_cached_sizze_39827, bytes_38279);
        if (err != FUTHARK_SUCCESS)
            goto cleanup;
    }
    if (mem_38482_cached_sizze_39828 < bytes_38172) {
        err = lexical_realloc(&ctx->error, &mem_38482,
                              &mem_38482_cached_sizze_39828, bytes_38172);
        if (err != FUTHARK_SUCCESS)
            goto cleanup;
    }
    if (mem_38485_cached_sizze_39829 < bytes_38172) {
        err = lexical_realloc(&ctx->error, &mem_38485,
                              &mem_38485_cached_sizze_39829, bytes_38172);
        if (err != FUTHARK_SUCCESS)
            goto cleanup;
    }
    if (mem_38488_cached_sizze_39830 < bytes_38172) {
        err = lexical_realloc(&ctx->error, &mem_38488,
                              &mem_38488_cached_sizze_39830, bytes_38172);
        if (err != FUTHARK_SUCCESS)
            goto cleanup;
    }
    if (mem_38491_cached_sizze_39831 < bytes_38172) {
        err = lexical_realloc(&ctx->error, &mem_38491,
                              &mem_38491_cached_sizze_39831, bytes_38172);
        if (err != FUTHARK_SUCCESS)
            goto cleanup;
    }
    if (mem_38542_cached_sizze_39832 < bytes_38172) {
        err = lexical_realloc(&ctx->error, &mem_38542,
                              &mem_38542_cached_sizze_39832, bytes_38172);
        if (err != FUTHARK_SUCCESS)
            goto cleanup;
    }
    if (mem_38545_cached_sizze_39833 < bytes_38172) {
        err = lexical_realloc(&ctx->error, &mem_38545,
                              &mem_38545_cached_sizze_39833, bytes_38172);
        if (err != FUTHARK_SUCCESS)
            goto cleanup;
    }
    if (mem_38548_cached_sizze_39834 < bytes_38172) {
        err = lexical_realloc(&ctx->error, &mem_38548,
                              &mem_38548_cached_sizze_39834, bytes_38172);
        if (err != FUTHARK_SUCCESS)
            goto cleanup;
    }
    if (mem_38551_cached_sizze_39835 < bytes_38172) {
        err = lexical_realloc(&ctx->error, &mem_38551,
                              &mem_38551_cached_sizze_39835, bytes_38172);
        if (err != FUTHARK_SUCCESS)
            goto cleanup;
    }
    if (mem_38602_cached_sizze_39836 < bytes_38172) {
        err = lexical_realloc(&ctx->error, &mem_38602,
                              &mem_38602_cached_sizze_39836, bytes_38172);
        if (err != FUTHARK_SUCCESS)
            goto cleanup;
    }
    if (mem_38617_cached_sizze_39837 < bytes_38172) {
        err = lexical_realloc(&ctx->error, &mem_38617,
                              &mem_38617_cached_sizze_39837, bytes_38172);
        if (err != FUTHARK_SUCCESS)
            goto cleanup;
    }
    if (mem_38620_cached_sizze_39838 < bytes_38172) {
        err = lexical_realloc(&ctx->error, &mem_38620,
                              &mem_38620_cached_sizze_39838, bytes_38172);
        if (err != FUTHARK_SUCCESS)
            goto cleanup;
    }
    if (mem_38647_cached_sizze_39839 < bytes_38172) {
        err = lexical_realloc(&ctx->error, &mem_38647,
                              &mem_38647_cached_sizze_39839, bytes_38172);
        if (err != FUTHARK_SUCCESS)
            goto cleanup;
    }
    if (mem_38650_cached_sizze_39840 < bytes_38172) {
        err = lexical_realloc(&ctx->error, &mem_38650,
                              &mem_38650_cached_sizze_39840, bytes_38172);
        if (err != FUTHARK_SUCCESS)
            goto cleanup;
    }
    if (mem_38677_cached_sizze_39841 < bytes_38172) {
        err = lexical_realloc(&ctx->error, &mem_38677,
                              &mem_38677_cached_sizze_39841, bytes_38172);
        if (err != FUTHARK_SUCCESS)
            goto cleanup;
    }
    if (mem_38692_cached_sizze_39842 < bytes_38172) {
        err = lexical_realloc(&ctx->error, &mem_38692,
                              &mem_38692_cached_sizze_39842, bytes_38172);
        if (err != FUTHARK_SUCCESS)
            goto cleanup;
    }
    if (mem_38695_cached_sizze_39843 < bytes_38172) {
        err = lexical_realloc(&ctx->error, &mem_38695,
                              &mem_38695_cached_sizze_39843, bytes_38172);
        if (err != FUTHARK_SUCCESS)
            goto cleanup;
    }
    if (mem_38722_cached_sizze_39844 < bytes_38172) {
        err = lexical_realloc(&ctx->error, &mem_38722,
                              &mem_38722_cached_sizze_39844, bytes_38172);
        if (err != FUTHARK_SUCCESS)
            goto cleanup;
    }
    if (mem_38725_cached_sizze_39845 < bytes_38172) {
        err = lexical_realloc(&ctx->error, &mem_38725,
                              &mem_38725_cached_sizze_39845, bytes_38172);
        if (err != FUTHARK_SUCCESS)
            goto cleanup;
    }
    if (mem_38752_cached_sizze_39846 < bytes_38172) {
        err = lexical_realloc(&ctx->error, &mem_38752,
                              &mem_38752_cached_sizze_39846, bytes_38172);
        if (err != FUTHARK_SUCCESS)
            goto cleanup;
    }
    for (int64_t i_37333 = 0; i_37333 < xdim_31402; i_37333++) {
        int32_t defunc_0_f_res_35843 = sext_i64_i32(i_37333);
        bool cond_35859 = sle32(2, defunc_0_f_res_35843);
        bool cond_t_res_35860 = slt32(defunc_0_f_res_35843, y_35853);
        bool x_35861 = cond_35859 && cond_t_res_35860;
        
        for (int64_t i_37185 = 0; i_37185 < ydim_31403; i_37185++) {
            int32_t x_35864;
            
            x_35864 = ((int32_t *) mem_38188)[i_37185];
            
            bool cond_t_res_35865 = sle32(2, x_35864);
            bool x_35866 = x_35861 && cond_t_res_35865;
            bool cond_t_res_35867 = slt32(x_35864, y_35855);
            bool x_35868 = x_35866 && cond_t_res_35867;
            
            for (int64_t i_37181 = 0; i_37181 < zzdim_31404; i_37181++) {
                float x_35870;
                
                x_35870 = ((float *) tketau_mem_37734.mem)[i_37333 *
                                                           (zzdim_31404 *
                                                            ydim_31403) +
                                                           i_37185 *
                                                           zzdim_31404 +
                                                           i_37181];
                
                int32_t x_35871;
                
                x_35871 = ((int32_t *) mem_38173)[i_37181];
                
                float max_res_35872 = fmax32(0.0F, x_35870);
                float sqrt_res_35873;
                
                sqrt_res_35873 = futrts_sqrt32(max_res_35872);
                
                float defunc_3_f_res_35874;
                
                if (x_35868) {
                    int64_t x_35875 = sext_i32_i64(defunc_0_f_res_35843);
                    bool x_35876 = sle64((int64_t) 0, x_35875);
                    bool y_35877 = slt64(x_35875, xdim_31402);
                    bool bounds_check_35878 = x_35876 && y_35877;
                    int64_t y_35879 = sext_i32_i64(x_35864);
                    bool x_35880 = sle64((int64_t) 0, y_35879);
                    bool y_35881 = slt64(y_35879, ydim_31403);
                    bool bounds_check_35882 = x_35880 && y_35881;
                    bool index_ok_35883 = bounds_check_35878 &&
                         bounds_check_35882;
                    bool index_certs_35884;
                    
                    if (!index_ok_35883) {
                        ctx->error =
                            msgprintf("Error: %s%lld%s%lld%s%lld%s%lld%s\n\nBacktrace:\n%s",
                                      "Index [", (long long) x_35875, ", ",
                                      (long long) y_35879,
                                      "] out of bounds for array of shape [",
                                      (long long) xdim_31402, "][",
                                      (long long) ydim_31403, "].",
                                      "-> #0  sample_programs/tke.fut:135:38-46\n   #1  sample_programs/tke.fut:43:37-46\n   #2  /prelude/soacs.fut:67:19-23\n   #3  /prelude/soacs.fut:67:3-37\n   #4  sample_programs/tke.fut:43:21-44:54\n   #5  /prelude/soacs.fut:67:19-23\n   #6  /prelude/soacs.fut:67:3-37\n   #7  sample_programs/tke.fut:42:13-45:50\n   #8  /prelude/soacs.fut:67:19-23\n   #9  /prelude/soacs.fut:67:3-37\n   #10 sample_programs/tke.fut:41:5-46:39\n   #11 sample_programs/tke.fut:130:17-155:9\n   #12 sample_programs/tke.fut:57:1-363:81\n");
                        err = FUTHARK_PROGRAM_ERROR;
                        goto cleanup;
                    }
                    
                    int32_t x_35885;
                    
                    x_35885 = ((int32_t *) kbot_mem_37754.mem)[x_35875 *
                                                               ydim_31403 +
                                                               y_35879];
                    
                    int32_t ks_val_35886 = sub32(x_35885, 1);
                    bool land_mask_35887 = sle32(0, ks_val_35886);
                    bool edge_mask_t_res_35888 = x_35871 == ks_val_35886;
                    bool x_35889 = land_mask_35887 && edge_mask_t_res_35888;
                    bool water_mask_t_res_35890 = sle32(ks_val_35886, x_35871);
                    bool x_35891 = land_mask_35887 && water_mask_t_res_35890;
                    bool cond_35892 = !x_35891;
                    float defunc_3_f_res_t_res_35893;
                    
                    if (cond_35892) {
                        defunc_3_f_res_t_res_35893 = 1.0F;
                    } else {
                        float defunc_3_f_res_t_res_f_res_35894;
                        
                        if (x_35889) {
                            int64_t zz_35895 = sext_i32_i64(x_35871);
                            bool x_35896 = sle64((int64_t) 0, zz_35895);
                            bool y_35897 = slt64(zz_35895, zzdim_31404);
                            bool bounds_check_35898 = x_35896 && y_35897;
                            bool y_35899 = bounds_check_35878 &&
                                 bounds_check_35898;
                            bool index_ok_35900 = bounds_check_35882 && y_35899;
                            bool index_certs_35901;
                            
                            if (!index_ok_35900) {
                                ctx->error =
                                    msgprintf("Error: %s%lld%s%lld%s%lld%s%lld%s%lld%s%lld%s\n\nBacktrace:\n%s",
                                              "Index [", (long long) x_35875,
                                              ", ", (long long) y_35879, ", ",
                                              (long long) zz_35895,
                                              "] out of bounds for array of shape [",
                                              (long long) xdim_31402, "][",
                                              (long long) ydim_31403, "][",
                                              (long long) zzdim_31404, "].",
                                              "-> #0  sample_programs/tke.fut:143:33-44\n   #1  sample_programs/tke.fut:43:37-46\n   #2  /prelude/soacs.fut:67:19-23\n   #3  /prelude/soacs.fut:67:3-37\n   #4  sample_programs/tke.fut:43:21-44:54\n   #5  /prelude/soacs.fut:67:19-23\n   #6  /prelude/soacs.fut:67:3-37\n   #7  sample_programs/tke.fut:42:13-45:50\n   #8  /prelude/soacs.fut:67:19-23\n   #9  /prelude/soacs.fut:67:3-37\n   #10 sample_programs/tke.fut:41:5-46:39\n   #11 sample_programs/tke.fut:130:17-155:9\n   #12 sample_programs/tke.fut:57:1-363:81\n");
                                err = FUTHARK_PROGRAM_ERROR;
                                goto cleanup;
                            }
                            
                            float x_35902;
                            
                            x_35902 = ((float *) mem_37763)[x_35875 *
                                                            ctx_val_37824 +
                                                            y_35879 *
                                                            zzdim_31404 +
                                                            zz_35895];
                            
                            bool index_certs_35903;
                            
                            if (!bounds_check_35898) {
                                ctx->error =
                                    msgprintf("Error: %s%lld%s%lld%s\n\nBacktrace:\n%s",
                                              "Index [", (long long) zz_35895,
                                              "] out of bounds for array of shape [",
                                              (long long) zzdim_31404, "].",
                                              "-> #0  sample_programs/tke.fut:143:48-53\n   #1  sample_programs/tke.fut:43:37-46\n   #2  /prelude/soacs.fut:67:19-23\n   #3  /prelude/soacs.fut:67:3-37\n   #4  sample_programs/tke.fut:43:21-44:54\n   #5  /prelude/soacs.fut:67:19-23\n   #6  /prelude/soacs.fut:67:3-37\n   #7  sample_programs/tke.fut:42:13-45:50\n   #8  /prelude/soacs.fut:67:19-23\n   #9  /prelude/soacs.fut:67:3-37\n   #10 sample_programs/tke.fut:41:5-46:39\n   #11 sample_programs/tke.fut:130:17-155:9\n   #12 sample_programs/tke.fut:57:1-363:81\n");
                                err = FUTHARK_PROGRAM_ERROR;
                                goto cleanup;
                            }
                            
                            float y_35904;
                            
                            y_35904 = ((float *) dzzw_mem_37751.mem)[zz_35895];
                            
                            float y_35905 = x_35902 / y_35904;
                            float x_35906 = 1.0F + y_35905;
                            float y_35908;
                            
                            y_35908 = ((float *) mxl_mem_37756.mem)[x_35875 *
                                                                    (zzdim_31404 *
                                                                     ydim_31403) +
                                                                    y_35879 *
                                                                    zzdim_31404 +
                                                                    zz_35895];
                            
                            float x_35909 = 0.7F / y_35908;
                            float y_35910 = sqrt_res_35873 * x_35909;
                            float defunc_3_f_res_t_res_f_res_t_res_35911 =
                                  x_35906 + y_35910;
                            
                            defunc_3_f_res_t_res_f_res_35894 =
                                defunc_3_f_res_t_res_f_res_t_res_35911;
                        } else {
                            bool cond_35912 = slt32(0, x_35871);
                            int32_t i64_res_35913 = sext_i64_i32(zzdim_31404);
                            int32_t y_35914 = sub32(i64_res_35913, 1);
                            bool cond_t_res_35915 = slt32(x_35871, y_35914);
                            bool x_35916 = cond_35912 && cond_t_res_35915;
                            float defunc_3_f_res_t_res_f_res_f_res_35917;
                            
                            if (x_35916) {
                                int64_t zz_35918 = sext_i32_i64(x_35871);
                                bool x_35919 = sle64((int64_t) 0, zz_35918);
                                bool y_35920 = slt64(zz_35918, zzdim_31404);
                                bool bounds_check_35921 = x_35919 && y_35920;
                                bool y_35922 = bounds_check_35878 &&
                                     bounds_check_35921;
                                bool index_ok_35923 = bounds_check_35882 &&
                                     y_35922;
                                bool index_certs_35924;
                                
                                if (!index_ok_35923) {
                                    ctx->error =
                                        msgprintf("Error: %s%lld%s%lld%s%lld%s%lld%s%lld%s%lld%s\n\nBacktrace:\n%s",
                                                  "Index [",
                                                  (long long) x_35875, ", ",
                                                  (long long) y_35879, ", ",
                                                  (long long) zz_35918,
                                                  "] out of bounds for array of shape [",
                                                  (long long) xdim_31402, "][",
                                                  (long long) ydim_31403, "][",
                                                  (long long) zzdim_31404, "].",
                                                  "-> #0  sample_programs/tke.fut:147:43-56\n   #1  sample_programs/tke.fut:43:37-46\n   #2  /prelude/soacs.fut:67:19-23\n   #3  /prelude/soacs.fut:67:3-37\n   #4  sample_programs/tke.fut:43:21-44:54\n   #5  /prelude/soacs.fut:67:19-23\n   #6  /prelude/soacs.fut:67:3-37\n   #7  sample_programs/tke.fut:42:13-45:50\n   #8  /prelude/soacs.fut:67:19-23\n   #9  /prelude/soacs.fut:67:3-37\n   #10 sample_programs/tke.fut:41:5-46:39\n   #11 sample_programs/tke.fut:130:17-155:9\n   #12 sample_programs/tke.fut:57:1-363:81\n");
                                    err = FUTHARK_PROGRAM_ERROR;
                                    goto cleanup;
                                }
                                
                                float x_35925;
                                
                                x_35925 = ((float *) mem_37763)[x_35875 *
                                                                ctx_val_37824 +
                                                                y_35879 *
                                                                zzdim_31404 +
                                                                zz_35918];
                                
                                int32_t i_35926 = sub32(x_35871, 1);
                                int64_t i_35927 = sext_i32_i64(i_35926);
                                bool x_35928 = sle64((int64_t) 0, i_35927);
                                bool y_35929 = slt64(i_35927, zzdim_31404);
                                bool bounds_check_35930 = x_35928 && y_35929;
                                bool y_35931 = bounds_check_35878 &&
                                     bounds_check_35930;
                                bool index_ok_35932 = bounds_check_35882 &&
                                     y_35931;
                                bool index_certs_35933;
                                
                                if (!index_ok_35932) {
                                    ctx->error =
                                        msgprintf("Error: %s%lld%s%lld%s%lld%s%lld%s%lld%s%lld%s\n\nBacktrace:\n%s",
                                                  "Index [",
                                                  (long long) x_35875, ", ",
                                                  (long long) y_35879, ", ",
                                                  (long long) i_35927,
                                                  "] out of bounds for array of shape [",
                                                  (long long) xdim_31402, "][",
                                                  (long long) ydim_31403, "][",
                                                  (long long) zzdim_31404, "].",
                                                  "-> #0  sample_programs/tke.fut:147:60-75\n   #1  sample_programs/tke.fut:43:37-46\n   #2  /prelude/soacs.fut:67:19-23\n   #3  /prelude/soacs.fut:67:3-37\n   #4  sample_programs/tke.fut:43:21-44:54\n   #5  /prelude/soacs.fut:67:19-23\n   #6  /prelude/soacs.fut:67:3-37\n   #7  sample_programs/tke.fut:42:13-45:50\n   #8  /prelude/soacs.fut:67:19-23\n   #9  /prelude/soacs.fut:67:3-37\n   #10 sample_programs/tke.fut:41:5-46:39\n   #11 sample_programs/tke.fut:130:17-155:9\n   #12 sample_programs/tke.fut:57:1-363:81\n");
                                    err = FUTHARK_PROGRAM_ERROR;
                                    goto cleanup;
                                }
                                
                                float y_35934;
                                
                                y_35934 = ((float *) mem_37763)[x_35875 *
                                                                ctx_val_37824 +
                                                                y_35879 *
                                                                zzdim_31404 +
                                                                i_35927];
                                
                                float x_35935 = x_35925 + y_35934;
                                bool index_certs_35936;
                                
                                if (!bounds_check_35921) {
                                    ctx->error =
                                        msgprintf("Error: %s%lld%s%lld%s\n\nBacktrace:\n%s",
                                                  "Index [",
                                                  (long long) zz_35918,
                                                  "] out of bounds for array of shape [",
                                                  (long long) zzdim_31404, "].",
                                                  "-> #0  sample_programs/tke.fut:147:80-85\n   #1  sample_programs/tke.fut:43:37-46\n   #2  /prelude/soacs.fut:67:19-23\n   #3  /prelude/soacs.fut:67:3-37\n   #4  sample_programs/tke.fut:43:21-44:54\n   #5  /prelude/soacs.fut:67:19-23\n   #6  /prelude/soacs.fut:67:3-37\n   #7  sample_programs/tke.fut:42:13-45:50\n   #8  /prelude/soacs.fut:67:19-23\n   #9  /prelude/soacs.fut:67:3-37\n   #10 sample_programs/tke.fut:41:5-46:39\n   #11 sample_programs/tke.fut:130:17-155:9\n   #12 sample_programs/tke.fut:57:1-363:81\n");
                                    err = FUTHARK_PROGRAM_ERROR;
                                    goto cleanup;
                                }
                                
                                float y_35937;
                                
                                y_35937 =
                                    ((float *) dzzw_mem_37751.mem)[zz_35918];
                                
                                float y_35938 = x_35935 / y_35937;
                                float x_35939 = 1.0F + y_35938;
                                float x_35941 = 0.7F * sqrt_res_35873;
                                float y_35942;
                                
                                y_35942 =
                                    ((float *) mxl_mem_37756.mem)[x_35875 *
                                                                  (zzdim_31404 *
                                                                   ydim_31403) +
                                                                  y_35879 *
                                                                  zzdim_31404 +
                                                                  zz_35918];
                                
                                float y_35943 = x_35941 / y_35942;
                                float
                                defunc_3_f_res_t_res_f_res_f_res_t_res_35944 =
                                x_35939 + y_35943;
                                
                                defunc_3_f_res_t_res_f_res_f_res_35917 =
                                    defunc_3_f_res_t_res_f_res_f_res_t_res_35944;
                            } else {
                                bool cond_35945 = x_35871 == y_35914;
                                float
                                defunc_3_f_res_t_res_f_res_f_res_f_res_35946;
                                
                                if (cond_35945) {
                                    int32_t i_35947 = sub32(x_35871, 1);
                                    int64_t i_35948 = sext_i32_i64(i_35947);
                                    bool x_35949 = sle64((int64_t) 0, i_35948);
                                    bool y_35950 = slt64(i_35948, zzdim_31404);
                                    bool bounds_check_35951 = x_35949 &&
                                         y_35950;
                                    bool y_35952 = bounds_check_35878 &&
                                         bounds_check_35951;
                                    bool index_ok_35953 = bounds_check_35882 &&
                                         y_35952;
                                    bool index_certs_35954;
                                    
                                    if (!index_ok_35953) {
                                        ctx->error =
                                            msgprintf("Error: %s%lld%s%lld%s%lld%s%lld%s%lld%s%lld%s\n\nBacktrace:\n%s",
                                                      "Index [",
                                                      (long long) x_35875, ", ",
                                                      (long long) y_35879, ", ",
                                                      (long long) i_35948,
                                                      "] out of bounds for array of shape [",
                                                      (long long) xdim_31402,
                                                      "][",
                                                      (long long) ydim_31403,
                                                      "][",
                                                      (long long) zzdim_31404,
                                                      "].",
                                                      "-> #0  sample_programs/tke.fut:151:42-57\n   #1  sample_programs/tke.fut:43:37-46\n   #2  /prelude/soacs.fut:67:19-23\n   #3  /prelude/soacs.fut:67:3-37\n   #4  sample_programs/tke.fut:43:21-44:54\n   #5  /prelude/soacs.fut:67:19-23\n   #6  /prelude/soacs.fut:67:3-37\n   #7  sample_programs/tke.fut:42:13-45:50\n   #8  /prelude/soacs.fut:67:19-23\n   #9  /prelude/soacs.fut:67:3-37\n   #10 sample_programs/tke.fut:41:5-46:39\n   #11 sample_programs/tke.fut:130:17-155:9\n   #12 sample_programs/tke.fut:57:1-363:81\n");
                                        err = FUTHARK_PROGRAM_ERROR;
                                        goto cleanup;
                                    }
                                    
                                    float x_35955;
                                    
                                    x_35955 = ((float *) mem_37763)[x_35875 *
                                                                    ctx_val_37824 +
                                                                    y_35879 *
                                                                    zzdim_31404 +
                                                                    i_35948];
                                    
                                    int64_t zz_35956 = sext_i32_i64(x_35871);
                                    bool x_35957 = sle64((int64_t) 0, zz_35956);
                                    bool y_35958 = slt64(zz_35956, zzdim_31404);
                                    bool bounds_check_35959 = x_35957 &&
                                         y_35958;
                                    bool index_certs_35960;
                                    
                                    if (!bounds_check_35959) {
                                        ctx->error =
                                            msgprintf("Error: %s%lld%s%lld%s\n\nBacktrace:\n%s",
                                                      "Index [",
                                                      (long long) zz_35956,
                                                      "] out of bounds for array of shape [",
                                                      (long long) zzdim_31404,
                                                      "].",
                                                      "-> #0  sample_programs/tke.fut:151:68-73\n   #1  sample_programs/tke.fut:43:37-46\n   #2  /prelude/soacs.fut:67:19-23\n   #3  /prelude/soacs.fut:67:3-37\n   #4  sample_programs/tke.fut:43:21-44:54\n   #5  /prelude/soacs.fut:67:19-23\n   #6  /prelude/soacs.fut:67:3-37\n   #7  sample_programs/tke.fut:42:13-45:50\n   #8  /prelude/soacs.fut:67:19-23\n   #9  /prelude/soacs.fut:67:3-37\n   #10 sample_programs/tke.fut:41:5-46:39\n   #11 sample_programs/tke.fut:130:17-155:9\n   #12 sample_programs/tke.fut:57:1-363:81\n");
                                        err = FUTHARK_PROGRAM_ERROR;
                                        goto cleanup;
                                    }
                                    
                                    float y_35961;
                                    
                                    y_35961 =
                                        ((float *) dzzw_mem_37751.mem)[zz_35956];
                                    
                                    float y_35962 = 0.5F * y_35961;
                                    float y_35963 = x_35955 / y_35962;
                                    float x_35964 = 1.0F + y_35963;
                                    bool y_35966 = bounds_check_35878 &&
                                         bounds_check_35959;
                                    bool index_ok_35967 = bounds_check_35882 &&
                                         y_35966;
                                    bool index_certs_35968;
                                    
                                    if (!index_ok_35967) {
                                        ctx->error =
                                            msgprintf("Error: %s%lld%s%lld%s%lld%s%lld%s%lld%s%lld%s\n\nBacktrace:\n%s",
                                                      "Index [",
                                                      (long long) x_35875, ", ",
                                                      (long long) y_35879, ", ",
                                                      (long long) zz_35956,
                                                      "] out of bounds for array of shape [",
                                                      (long long) xdim_31402,
                                                      "][",
                                                      (long long) ydim_31403,
                                                      "][",
                                                      (long long) zzdim_31404,
                                                      "].",
                                                      "-> #0  sample_programs/tke.fut:152:52-61\n   #1  sample_programs/tke.fut:43:37-46\n   #2  /prelude/soacs.fut:67:19-23\n   #3  /prelude/soacs.fut:67:3-37\n   #4  sample_programs/tke.fut:43:21-44:54\n   #5  /prelude/soacs.fut:67:19-23\n   #6  /prelude/soacs.fut:67:3-37\n   #7  sample_programs/tke.fut:42:13-45:50\n   #8  /prelude/soacs.fut:67:19-23\n   #9  /prelude/soacs.fut:67:3-37\n   #10 sample_programs/tke.fut:41:5-46:39\n   #11 sample_programs/tke.fut:130:17-155:9\n   #12 sample_programs/tke.fut:57:1-363:81\n");
                                        err = FUTHARK_PROGRAM_ERROR;
                                        goto cleanup;
                                    }
                                    
                                    float y_35969;
                                    
                                    y_35969 =
                                        ((float *) mxl_mem_37756.mem)[x_35875 *
                                                                      (zzdim_31404 *
                                                                       ydim_31403) +
                                                                      y_35879 *
                                                                      zzdim_31404 +
                                                                      zz_35956];
                                    
                                    float x_35970 = 0.7F / y_35969;
                                    float y_35971 = sqrt_res_35873 * x_35970;
                                    float
                                    defunc_3_f_res_t_res_f_res_f_res_f_res_t_res_35972
                                    = x_35964 + y_35971;
                                    
                                    defunc_3_f_res_t_res_f_res_f_res_f_res_35946 =
                                        defunc_3_f_res_t_res_f_res_f_res_f_res_t_res_35972;
                                } else {
                                    defunc_3_f_res_t_res_f_res_f_res_f_res_35946 =
                                        0.0F;
                                }
                                defunc_3_f_res_t_res_f_res_f_res_35917 =
                                    defunc_3_f_res_t_res_f_res_f_res_f_res_35946;
                            }
                            defunc_3_f_res_t_res_f_res_35894 =
                                defunc_3_f_res_t_res_f_res_f_res_35917;
                        }
                        defunc_3_f_res_t_res_35893 =
                            defunc_3_f_res_t_res_f_res_35894;
                    }
                    defunc_3_f_res_35874 = defunc_3_f_res_t_res_35893;
                } else {
                    defunc_3_f_res_35874 = 0.0F;
                }
                ((float *) mem_38280)[i_37185 * zzdim_31404 + i_37181] =
                    defunc_3_f_res_35874;
            }
        }
        
        bool cond_36029 = sle64((int64_t) 2, i_37333);
        bool cond_t_res_36030 = slt64(i_37333, y_35716);
        bool x_36031 = cond_36029 && cond_t_res_36030;
        
        for (int64_t i_37193 = 0; i_37193 < ydim_31403; i_37193++) {
            bool cond_t_res_36034 = sle64((int64_t) 2, i_37193);
            bool x_36035 = x_36031 && cond_t_res_36034;
            bool cond_t_res_36036 = slt64(i_37193, y_35717);
            bool x_36037 = x_36035 && cond_t_res_36036;
            
            for (int64_t i_37189 = 0; i_37189 < zzdim_31404; i_37189++) {
                float defunc_0_f_res_36040;
                
                if (x_36037) {
                    int32_t x_36049;
                    
                    x_36049 = ((int32_t *) kbot_mem_37754.mem)[i_37333 *
                                                               ydim_31403 +
                                                               i_37193];
                    
                    int32_t ks_val_36050 = sub32(x_36049, 1);
                    bool land_mask_36051 = sle32(0, ks_val_36050);
                    int32_t i64_res_36052 = sext_i64_i32(i_37189);
                    bool water_mask_t_res_36053 = sle32(ks_val_36050,
                                                        i64_res_36052);
                    bool x_36054 = land_mask_36051 && water_mask_t_res_36053;
                    bool cond_36055 = !x_36054;
                    float defunc_0_f_res_t_res_36056;
                    
                    if (cond_36055) {
                        defunc_0_f_res_t_res_36056 = 0.0F;
                    } else {
                        float x_36063;
                        
                        x_36063 = ((float *) tketau_mem_37734.mem)[i_37333 *
                                                                   (zzdim_31404 *
                                                                    ydim_31403) +
                                                                   i_37193 *
                                                                   zzdim_31404 +
                                                                   i_37189];
                        
                        float y_36064;
                        
                        y_36064 = ((float *) forc_mem_37757.mem)[i_37333 *
                                                                 (zzdim_31404 *
                                                                  ydim_31403) +
                                                                 i_37193 *
                                                                 zzdim_31404 +
                                                                 i_37189];
                        
                        float tmp_36066 = x_36063 + y_36064;
                        bool cond_36068 = i_37189 == y_35718;
                        float defunc_0_f_res_t_res_f_res_36069;
                        
                        if (cond_36068) {
                            float y_36070;
                            
                            y_36070 =
                                ((float *) forc_tke_surface_mem_37758.mem)[i_37333 *
                                                                           ydim_31403 +
                                                                           i_37193];
                            
                            float y_36073;
                            
                            y_36073 = ((float *) dzzw_mem_37751.mem)[i_37189];
                            
                            float y_36074 = 0.5F * y_36073;
                            float y_36075 = y_36070 / y_36074;
                            float defunc_0_f_res_t_res_f_res_t_res_36076 =
                                  tmp_36066 + y_36075;
                            
                            defunc_0_f_res_t_res_f_res_36069 =
                                defunc_0_f_res_t_res_f_res_t_res_36076;
                        } else {
                            defunc_0_f_res_t_res_f_res_36069 = tmp_36066;
                        }
                        defunc_0_f_res_t_res_36056 =
                            defunc_0_f_res_t_res_f_res_36069;
                    }
                    defunc_0_f_res_36040 = defunc_0_f_res_t_res_36056;
                } else {
                    defunc_0_f_res_36040 = 0.0F;
                }
                ((float *) mem_38327)[i_37193 * zzdim_31404 + i_37189] =
                    defunc_0_f_res_36040;
            }
        }
        for (int64_t i_37201 = 0; i_37201 < ydim_31403; i_37201++) {
            bool cond_t_res_35986 = sle64((int64_t) 2, i_37201);
            bool x_35987 = cond_t_res_35986 && x_36031;
            bool cond_t_res_35988 = slt64(i_37201, y_35717);
            bool x_35989 = x_35987 && cond_t_res_35988;
            
            for (int64_t i_37197 = 0; i_37197 < zzdim_31404; i_37197++) {
                bool cond_t_res_35992 = slt64(i_37197, y_35718);
                bool x_35993 = x_35989 && cond_t_res_35992;
                float defunc_0_f_res_35994;
                
                if (x_35993) {
                    int32_t x_36003;
                    
                    x_36003 = ((int32_t *) kbot_mem_37754.mem)[i_37333 *
                                                               ydim_31403 +
                                                               i_37201];
                    
                    int32_t ks_val_36004 = sub32(x_36003, 1);
                    bool land_mask_36005 = sle32(0, ks_val_36004);
                    int32_t i64_res_36006 = sext_i64_i32(i_37197);
                    bool water_mask_t_res_36007 = sle32(ks_val_36004,
                                                        i64_res_36006);
                    bool x_36008 = land_mask_36005 && water_mask_t_res_36007;
                    bool cond_36009 = !x_36008;
                    float defunc_0_f_res_t_res_36010;
                    
                    if (cond_36009) {
                        defunc_0_f_res_t_res_36010 = 0.0F;
                    } else {
                        float negate_arg_36017;
                        
                        negate_arg_36017 = ((float *) mem_37763)[i_37333 *
                                                                 ctx_val_37824 +
                                                                 i_37201 *
                                                                 zzdim_31404 +
                                                                 i_37197];
                        
                        float x_36018 = 0.0F - negate_arg_36017;
                        float y_36020;
                        
                        y_36020 = ((float *) dzzw_mem_37751.mem)[i_37197];
                        
                        float defunc_0_f_res_t_res_f_res_36021 = x_36018 /
                              y_36020;
                        
                        defunc_0_f_res_t_res_36010 =
                            defunc_0_f_res_t_res_f_res_36021;
                    }
                    defunc_0_f_res_35994 = defunc_0_f_res_t_res_36010;
                } else {
                    defunc_0_f_res_35994 = 0.0F;
                }
                ((float *) mem_38374)[i_37201 * zzdim_31404 + i_37197] =
                    defunc_0_f_res_35994;
            }
        }
        for (int64_t i_37209 = 0; i_37209 < ydim_31403; i_37209++) {
            bool cond_t_res_35774 = sle64((int64_t) 2, i_37209);
            bool x_35775 = cond_t_res_35774 && x_36031;
            bool cond_t_res_35776 = slt64(i_37209, y_35717);
            bool x_35777 = x_35775 && cond_t_res_35776;
            
            for (int64_t i_37205 = 0; i_37205 < zzdim_31404; i_37205++) {
                float defunc_0_f_res_35780;
                
                if (x_35777) {
                    int32_t x_35789;
                    
                    x_35789 = ((int32_t *) kbot_mem_37754.mem)[i_37333 *
                                                               ydim_31403 +
                                                               i_37209];
                    
                    int32_t ks_val_35790 = sub32(x_35789, 1);
                    bool land_mask_35791 = sle32(0, ks_val_35790);
                    int32_t i64_res_35792 = sext_i64_i32(i_37205);
                    bool edge_mask_t_res_35793 = i64_res_35792 == ks_val_35790;
                    bool x_35794 = land_mask_35791 && edge_mask_t_res_35793;
                    bool water_mask_t_res_35795 = sle32(ks_val_35790,
                                                        i64_res_35792);
                    bool x_35796 = land_mask_35791 && water_mask_t_res_35795;
                    bool cond_f_res_35797 = !x_35796;
                    bool x_35798 = !x_35794;
                    bool y_35799 = cond_f_res_35797 && x_35798;
                    bool cond_35800 = x_35794 || y_35799;
                    float defunc_0_f_res_t_res_35801;
                    
                    if (cond_35800) {
                        defunc_0_f_res_t_res_35801 = 0.0F;
                    } else {
                        bool cond_35802 = slt64((int64_t) 0, i_37205);
                        bool cond_t_res_35804 = slt64(i_37205, y_35718);
                        bool x_35805 = cond_35802 && cond_t_res_35804;
                        float defunc_0_f_res_t_res_f_res_35806;
                        
                        if (x_35805) {
                            int64_t i_35807 = sub64(i_37205, (int64_t) 1);
                            bool x_35808 = sle64((int64_t) 0, i_35807);
                            bool y_35809 = slt64(i_35807, zzdim_31404);
                            bool bounds_check_35810 = x_35808 && y_35809;
                            bool index_certs_35813;
                            
                            if (!bounds_check_35810) {
                                ctx->error =
                                    msgprintf("Error: %s%lld%s%lld%s%lld%s%lld%s%lld%s%lld%s\n\nBacktrace:\n%s",
                                              "Index [", (long long) i_37333,
                                              ", ", (long long) i_37209, ", ",
                                              (long long) i_35807,
                                              "] out of bounds for array of shape [",
                                              (long long) xdim_31402, "][",
                                              (long long) ydim_31403, "][",
                                              (long long) zzdim_31404, "].",
                                              "-> #0  sample_programs/tke.fut:123:43-56\n   #1  /prelude/soacs.fut:59:3-10\n   #2  /prelude/array.fut:195:3-17\n   #3  /prelude/functional.fut:39:59-65\n   #4  /prelude/soacs.fut:59:3-10\n   #5  /prelude/array.fut:203:3-34\n   #6  /prelude/functional.fut:39:59-65\n   #7  /prelude/soacs.fut:59:3-10\n   #8  /prelude/array.fut:211:3-39\n   #9  sample_programs/tke.fut:112:17-128:17\n   #10 sample_programs/tke.fut:57:1-363:81\n");
                                err = FUTHARK_PROGRAM_ERROR;
                                goto cleanup;
                            }
                            
                            float negate_arg_35814;
                            
                            negate_arg_35814 = ((float *) mem_37763)[i_37333 *
                                                                     ctx_val_37824 +
                                                                     i_37209 *
                                                                     zzdim_31404 +
                                                                     i_35807];
                            
                            float x_35815 = 0.0F - negate_arg_35814;
                            float y_35820;
                            
                            y_35820 = ((float *) dzzw_mem_37751.mem)[i_37205];
                            
                            float defunc_0_f_res_t_res_f_res_t_res_35821 =
                                  x_35815 / y_35820;
                            
                            defunc_0_f_res_t_res_f_res_35806 =
                                defunc_0_f_res_t_res_f_res_t_res_35821;
                        } else {
                            bool cond_35822 = i_37205 == y_35718;
                            float defunc_0_f_res_t_res_f_res_f_res_35823;
                            
                            if (cond_35822) {
                                int64_t i_35824 = sub64(i_37205, (int64_t) 1);
                                bool x_35825 = sle64((int64_t) 0, i_35824);
                                bool y_35826 = slt64(i_35824, zzdim_31404);
                                bool bounds_check_35827 = x_35825 && y_35826;
                                bool index_certs_35830;
                                
                                if (!bounds_check_35827) {
                                    ctx->error =
                                        msgprintf("Error: %s%lld%s%lld%s%lld%s%lld%s%lld%s%lld%s\n\nBacktrace:\n%s",
                                                  "Index [",
                                                  (long long) i_37333, ", ",
                                                  (long long) i_37209, ", ",
                                                  (long long) i_35824,
                                                  "] out of bounds for array of shape [",
                                                  (long long) xdim_31402, "][",
                                                  (long long) ydim_31403, "][",
                                                  (long long) zzdim_31404, "].",
                                                  "-> #0  sample_programs/tke.fut:125:43-58\n   #1  /prelude/soacs.fut:59:3-10\n   #2  /prelude/array.fut:195:3-17\n   #3  /prelude/functional.fut:39:59-65\n   #4  /prelude/soacs.fut:59:3-10\n   #5  /prelude/array.fut:203:3-34\n   #6  /prelude/functional.fut:39:59-65\n   #7  /prelude/soacs.fut:59:3-10\n   #8  /prelude/array.fut:211:3-39\n   #9  sample_programs/tke.fut:112:17-128:17\n   #10 sample_programs/tke.fut:57:1-363:81\n");
                                    err = FUTHARK_PROGRAM_ERROR;
                                    goto cleanup;
                                }
                                
                                float negate_arg_35831;
                                
                                negate_arg_35831 =
                                    ((float *) mem_37763)[i_37333 *
                                                          ctx_val_37824 +
                                                          i_37209 *
                                                          zzdim_31404 +
                                                          i_35824];
                                
                                float x_35832 = 0.0F - negate_arg_35831;
                                float y_35837;
                                
                                y_35837 =
                                    ((float *) dzzw_mem_37751.mem)[i_37205];
                                
                                float y_35838 = 0.5F * y_35837;
                                float
                                defunc_0_f_res_t_res_f_res_f_res_t_res_35839 =
                                x_35832 / y_35838;
                                
                                defunc_0_f_res_t_res_f_res_f_res_35823 =
                                    defunc_0_f_res_t_res_f_res_f_res_t_res_35839;
                            } else {
                                defunc_0_f_res_t_res_f_res_f_res_35823 = 0.0F;
                            }
                            defunc_0_f_res_t_res_f_res_35806 =
                                defunc_0_f_res_t_res_f_res_f_res_35823;
                        }
                        defunc_0_f_res_t_res_35801 =
                            defunc_0_f_res_t_res_f_res_35806;
                    }
                    defunc_0_f_res_35780 = defunc_0_f_res_t_res_35801;
                } else {
                    defunc_0_f_res_35780 = 0.0F;
                }
                ((float *) mem_38421)[i_37209 * zzdim_31404 + i_37205] =
                    defunc_0_f_res_35780;
            }
        }
        for (int64_t i_37299 = 0; i_37299 < ydim_31403; i_37299++) {
            float b0_36088;
            
            b0_36088 = ((float *) mem_38280)[i_37299 * zzdim_31404];
            for (int64_t i_37219 = 0; i_37219 < zzdim_31404; i_37219++) {
                float x_elem_37122;
                
                x_elem_37122 = ((float *) mem_38280)[i_37299 * zzdim_31404 +
                                                     i_37219];
                
                float x_elem_37123;
                
                x_elem_37123 = ((float *) mem_38421)[i_37299 * zzdim_31404 +
                                                     i_37219];
                
                bool cond_36094 = slt64((int64_t) 0, i_37219);
                float defunc_0_f_res_36095;
                
                if (cond_36094) {
                    defunc_0_f_res_36095 = 1.0F;
                } else {
                    defunc_0_f_res_36095 = 0.0F;
                }
                
                float defunc_0_f_res_36096;
                
                if (cond_36094) {
                    defunc_0_f_res_36096 = 0.0F;
                } else {
                    defunc_0_f_res_36096 = 1.0F;
                }
                
                float defunc_0_f_res_36097;
                
                if (cond_36094) {
                    defunc_0_f_res_36097 = x_elem_37122;
                } else {
                    defunc_0_f_res_36097 = 1.0F;
                }
                
                float defunc_0_f_res_36098;
                
                if (cond_36094) {
                    int64_t i_36101 = sub64(i_37219, (int64_t) 1);
                    float y_36102;
                    
                    y_36102 = ((float *) mem_38374)[i_37299 * zzdim_31404 +
                                                    i_36101];
                    
                    float y_36103 = y_36102 * x_elem_37123;
                    float defunc_0_f_res_t_res_36104 = 0.0F - y_36103;
                    
                    defunc_0_f_res_36098 = defunc_0_f_res_t_res_36104;
                } else {
                    defunc_0_f_res_36098 = 0.0F;
                }
                ((float *) mem_38482)[i_37219] = defunc_0_f_res_36097;
                ((float *) mem_38485)[i_37219] = defunc_0_f_res_36098;
                ((float *) mem_38488)[i_37219] = defunc_0_f_res_36095;
                ((float *) mem_38491)[i_37219] = defunc_0_f_res_36096;
            }
            
            float discard_37241;
            float discard_37242;
            float discard_37243;
            float discard_37244;
            float scanacc_37228;
            float scanacc_37229;
            float scanacc_37230;
            float scanacc_37231;
            
            scanacc_37228 = 1.0F;
            scanacc_37229 = 0.0F;
            scanacc_37230 = 0.0F;
            scanacc_37231 = 1.0F;
            for (int64_t i_37236 = 0; i_37236 < zzdim_31404; i_37236++) {
                float x_36134;
                
                x_36134 = ((float *) mem_38482)[i_37236];
                
                float x_36135;
                
                x_36135 = ((float *) mem_38485)[i_37236];
                
                float x_36136;
                
                x_36136 = ((float *) mem_38488)[i_37236];
                
                float x_36137;
                
                x_36137 = ((float *) mem_38491)[i_37236];
                
                float y_36117 = x_36134 * scanacc_37228;
                float value_36118 = 1.0F / y_36117;
                float y_36119 = x_36135 * scanacc_37230;
                float x_36120 = y_36117 + y_36119;
                float defunc_1_op_res_36121 = value_36118 * x_36120;
                float x_36122 = x_36134 * scanacc_37229;
                float y_36123 = x_36135 * scanacc_37231;
                float x_36124 = x_36122 + y_36123;
                float defunc_1_op_res_36125 = value_36118 * x_36124;
                float x_36126 = x_36136 * scanacc_37228;
                float y_36127 = x_36137 * scanacc_37230;
                float x_36128 = x_36126 + y_36127;
                float defunc_1_op_res_36129 = value_36118 * x_36128;
                float x_36130 = x_36136 * scanacc_37229;
                float y_36131 = x_36137 * scanacc_37231;
                float x_36132 = x_36130 + y_36131;
                float defunc_1_op_res_36133 = value_36118 * x_36132;
                
                ((float *) mem_38542)[i_37236] = defunc_1_op_res_36121;
                ((float *) mem_38545)[i_37236] = defunc_1_op_res_36125;
                ((float *) mem_38548)[i_37236] = defunc_1_op_res_36129;
                ((float *) mem_38551)[i_37236] = defunc_1_op_res_36133;
                
                float scanacc_tmp_39763 = defunc_1_op_res_36121;
                float scanacc_tmp_39764 = defunc_1_op_res_36125;
                float scanacc_tmp_39765 = defunc_1_op_res_36129;
                float scanacc_tmp_39766 = defunc_1_op_res_36133;
                
                scanacc_37228 = scanacc_tmp_39763;
                scanacc_37229 = scanacc_tmp_39764;
                scanacc_37230 = scanacc_tmp_39765;
                scanacc_37231 = scanacc_tmp_39766;
            }
            discard_37241 = scanacc_37228;
            discard_37242 = scanacc_37229;
            discard_37243 = scanacc_37230;
            discard_37244 = scanacc_37231;
            for (int64_t i_37247 = 0; i_37247 < zzdim_31404; i_37247++) {
                float x_36139;
                
                x_36139 = ((float *) mem_38542)[i_37247];
                
                float x_36140;
                
                x_36140 = ((float *) mem_38545)[i_37247];
                
                float x_36141;
                
                x_36141 = ((float *) mem_38548)[i_37247];
                
                float x_36142;
                
                x_36142 = ((float *) mem_38551)[i_37247];
                
                float x_36143 = b0_36088 * x_36139;
                float x_36144 = x_36140 + x_36143;
                float x_36145 = b0_36088 * x_36141;
                float y_36146 = x_36142 + x_36145;
                float defunc_0_f_res_36147 = x_36144 / y_36146;
                
                ((float *) mem_38602)[i_37247] = defunc_0_f_res_36147;
            }
            
            float y0_36148;
            
            y0_36148 = ((float *) mem_38327)[i_37299 * zzdim_31404];
            for (int64_t i_37253 = 0; i_37253 < zzdim_31404; i_37253++) {
                float x_elem_37124;
                
                x_elem_37124 = ((float *) mem_38327)[i_37299 * zzdim_31404 +
                                                     i_37253];
                
                float x_elem_37125;
                
                x_elem_37125 = ((float *) mem_38421)[i_37299 * zzdim_31404 +
                                                     i_37253];
                
                bool cond_36152 = slt64((int64_t) 0, i_37253);
                float defunc_0_f_res_36153;
                
                if (cond_36152) {
                    defunc_0_f_res_36153 = x_elem_37124;
                } else {
                    defunc_0_f_res_36153 = 0.0F;
                }
                
                float defunc_0_f_res_36154;
                
                if (cond_36152) {
                    int64_t i_36157 = sub64(i_37253, (int64_t) 1);
                    float y_36158;
                    
                    y_36158 = ((float *) mem_38602)[i_36157];
                    
                    float y_36159 = x_elem_37125 / y_36158;
                    float defunc_0_f_res_t_res_36160 = 0.0F - y_36159;
                    
                    defunc_0_f_res_36154 = defunc_0_f_res_t_res_36160;
                } else {
                    defunc_0_f_res_36154 = 1.0F;
                }
                ((float *) mem_38617)[i_37253] = defunc_0_f_res_36153;
                ((float *) mem_38620)[i_37253] = defunc_0_f_res_36154;
            }
            
            float discard_37265;
            float discard_37266;
            float scanacc_37258;
            float scanacc_37259;
            
            scanacc_37258 = 0.0F;
            scanacc_37259 = 1.0F;
            for (int64_t i_37262 = 0; i_37262 < zzdim_31404; i_37262++) {
                float x_36170;
                
                x_36170 = ((float *) mem_38617)[i_37262];
                
                float x_36171;
                
                x_36171 = ((float *) mem_38620)[i_37262];
                
                float y_36167 = x_36171 * scanacc_37258;
                float defunc_1_op_res_36168 = y_36167 + x_36170;
                float defunc_1_op_res_36169 = x_36171 * scanacc_37259;
                
                ((float *) mem_38647)[i_37262] = defunc_1_op_res_36168;
                ((float *) mem_38650)[i_37262] = defunc_1_op_res_36169;
                
                float scanacc_tmp_39774 = defunc_1_op_res_36168;
                float scanacc_tmp_39775 = defunc_1_op_res_36169;
                
                scanacc_37258 = scanacc_tmp_39774;
                scanacc_37259 = scanacc_tmp_39775;
            }
            discard_37265 = scanacc_37258;
            discard_37266 = scanacc_37259;
            for (int64_t i_37269 = 0; i_37269 < zzdim_31404; i_37269++) {
                float x_36173;
                
                x_36173 = ((float *) mem_38647)[i_37269];
                
                float x_36174;
                
                x_36174 = ((float *) mem_38650)[i_37269];
                
                float y_36175 = y0_36148 * x_36174;
                float defunc_0_f_res_36176 = x_36173 + y_36175;
                
                ((float *) mem_38677)[i_37269] = defunc_0_f_res_36176;
            }
            
            float x_36177;
            
            x_36177 = ((float *) mem_38677)[y_35718];
            
            float y_36178;
            
            y_36178 = ((float *) mem_38602)[y_35718];
            
            float yn_36179 = x_36177 / y_36178;
            
            for (int64_t i_37275 = 0; i_37275 < zzdim_31404; i_37275++) {
                int64_t x_36183 = sub64(zzdim_31404, i_37275);
                int64_t i_36184 = sub64(x_36183, (int64_t) 1);
                bool cond_36185 = slt64((int64_t) 0, i_37275);
                float defunc_0_f_res_36186;
                float defunc_0_f_res_36187;
                
                if (cond_36185) {
                    float x_36188;
                    
                    x_36188 = ((float *) mem_38677)[i_36184];
                    
                    float y_36189;
                    
                    y_36189 = ((float *) mem_38602)[i_36184];
                    
                    float defunc_0_f_res_t_res_36190 = x_36188 / y_36189;
                    float x_36191;
                    
                    x_36191 = ((float *) mem_38374)[i_37299 * zzdim_31404 +
                                                    i_36184];
                    
                    float y_36192 = x_36191 / y_36189;
                    float defunc_0_f_res_t_res_36193 = 0.0F - y_36192;
                    
                    defunc_0_f_res_36186 = defunc_0_f_res_t_res_36190;
                    defunc_0_f_res_36187 = defunc_0_f_res_t_res_36193;
                } else {
                    defunc_0_f_res_36186 = 0.0F;
                    defunc_0_f_res_36187 = 1.0F;
                }
                ((float *) mem_38692)[i_37275] = defunc_0_f_res_36186;
                ((float *) mem_38695)[i_37275] = defunc_0_f_res_36187;
            }
            
            float discard_37287;
            float discard_37288;
            float scanacc_37280;
            float scanacc_37281;
            
            scanacc_37280 = 0.0F;
            scanacc_37281 = 1.0F;
            for (int64_t i_37284 = 0; i_37284 < zzdim_31404; i_37284++) {
                float x_36203;
                
                x_36203 = ((float *) mem_38692)[i_37284];
                
                float x_36204;
                
                x_36204 = ((float *) mem_38695)[i_37284];
                
                float y_36200 = x_36204 * scanacc_37280;
                float defunc_1_op_res_36201 = y_36200 + x_36203;
                float defunc_1_op_res_36202 = x_36204 * scanacc_37281;
                
                ((float *) mem_38722)[i_37284] = defunc_1_op_res_36201;
                ((float *) mem_38725)[i_37284] = defunc_1_op_res_36202;
                
                float scanacc_tmp_39781 = defunc_1_op_res_36201;
                float scanacc_tmp_39782 = defunc_1_op_res_36202;
                
                scanacc_37280 = scanacc_tmp_39781;
                scanacc_37281 = scanacc_tmp_39782;
            }
            discard_37287 = scanacc_37280;
            discard_37288 = scanacc_37281;
            for (int64_t i_37291 = 0; i_37291 < zzdim_31404; i_37291++) {
                float x_36206;
                
                x_36206 = ((float *) mem_38722)[i_37291];
                
                float x_36207;
                
                x_36207 = ((float *) mem_38725)[i_37291];
                
                float y_36208 = yn_36179 * x_36207;
                float defunc_0_f_res_36209 = x_36206 + y_36208;
                
                ((float *) mem_38752)[i_37291] = defunc_0_f_res_36209;
            }
            for (int64_t i_37295 = 0; i_37295 < zzdim_31404; i_37295++) {
                int64_t x_36212 = sub64(zzdim_31404, i_37295);
                int64_t i_36213 = sub64(x_36212, (int64_t) 1);
                
                if ((int64_t) 4 > 0)
                    memmove(mem_38205 + (i_37333 * ctx_val_37824 + i_37299 *
                                         zzdim_31404 + i_37295) * (int64_t) 4,
                            mem_38752 + ((int64_t) 0 + (int64_t) 1 * i_36213) *
                            (int64_t) 4, (int64_t) 4);
            }
        }
        for (int64_t i_37307 = 0; i_37307 < ydim_31403; i_37307++) {
            bool cond_t_res_36730 = sle64((int64_t) 2, i_37307);
            bool x_36731 = x_36031 && cond_t_res_36730;
            bool cond_t_res_36732 = slt64(i_37307, y_35717);
            bool x_36733 = x_36731 && cond_t_res_36732;
            
            for (int64_t i_37303 = 0; i_37303 < zzdim_31404; i_37303++) {
                float tmp_36736;
                
                if (x_36733) {
                    float x_36749;
                    
                    x_36749 = ((float *) maskW_mem_37745.mem)[i_37333 *
                                                              (zzdim_31404 *
                                                               ydim_31403) +
                                                              i_37307 *
                                                              zzdim_31404 +
                                                              i_37303];
                    
                    float x_36750;
                    
                    x_36750 = ((float *) mem_37768)[i_37333 * ctx_val_37824 +
                                                    i_37307 * zzdim_31404 +
                                                    i_37303];
                    
                    int64_t i_36751 = sub64(i_37333, (int64_t) 1);
                    bool x_36752 = sle64((int64_t) 0, i_36751);
                    bool y_36753 = slt64(i_36751, xdim_31402);
                    bool bounds_check_36754 = x_36752 && y_36753;
                    bool index_certs_36757;
                    
                    if (!bounds_check_36754) {
                        ctx->error =
                            msgprintf("Error: %s%lld%s%lld%s%lld%s%lld%s%lld%s%lld%s\n\nBacktrace:\n%s",
                                      "Index [", (long long) i_36751, ", ",
                                      (long long) i_37307, ", ",
                                      (long long) i_37303,
                                      "] out of bounds for array of shape [",
                                      (long long) xdim_31402, "][",
                                      (long long) ydim_31403, "][",
                                      (long long) zzdim_31404, "].",
                                      "-> #0  sample_programs/tke.fut:344:66-85\n   #1  /prelude/soacs.fut:59:3-10\n   #2  /prelude/array.fut:195:3-17\n   #3  /prelude/functional.fut:39:59-65\n   #4  /prelude/soacs.fut:59:3-10\n   #5  /prelude/array.fut:203:3-34\n   #6  /prelude/functional.fut:39:59-65\n   #7  /prelude/soacs.fut:59:3-10\n   #8  /prelude/array.fut:211:3-39\n   #9  sample_programs/tke.fut:340:19-357:17\n   #10 sample_programs/tke.fut:57:1-363:81\n");
                        err = FUTHARK_PROGRAM_ERROR;
                        goto cleanup;
                    }
                    
                    float y_36758;
                    
                    y_36758 = ((float *) mem_37768)[i_36751 * ctx_val_37824 +
                                                    i_37307 * zzdim_31404 +
                                                    i_37303];
                    
                    float negate_arg_36759 = x_36750 - y_36758;
                    float x_36760 = 0.0F - negate_arg_36759;
                    float x_36762;
                    
                    x_36762 = ((float *) cost_mem_37752.mem)[i_37307];
                    
                    float y_36764;
                    
                    y_36764 = ((float *) dxt_mem_37746.mem)[i_37333];
                    
                    float y_36765 = x_36762 * y_36764;
                    float x_36766 = x_36760 / y_36765;
                    float x_36767;
                    
                    x_36767 = ((float *) mem_37773)[i_37333 * ctx_val_37824 +
                                                    i_37307 * zzdim_31404 +
                                                    i_37303];
                    
                    int64_t i_36768 = sub64(i_37307, (int64_t) 1);
                    bool x_36769 = sle64((int64_t) 0, i_36768);
                    bool y_36770 = slt64(i_36768, ydim_31403);
                    bool bounds_check_36771 = x_36769 && y_36770;
                    bool index_certs_36773;
                    
                    if (!bounds_check_36771) {
                        ctx->error =
                            msgprintf("Error: %s%lld%s%lld%s%lld%s%lld%s%lld%s%lld%s\n\nBacktrace:\n%s",
                                      "Index [", (long long) i_37333, ", ",
                                      (long long) i_36768, ", ",
                                      (long long) i_37303,
                                      "] out of bounds for array of shape [",
                                      (long long) xdim_31402, "][",
                                      (long long) ydim_31403, "][",
                                      (long long) zzdim_31404, "].",
                                      "-> #0  sample_programs/tke.fut:346:56-76\n   #1  /prelude/soacs.fut:59:3-10\n   #2  /prelude/array.fut:195:3-17\n   #3  /prelude/functional.fut:39:59-65\n   #4  /prelude/soacs.fut:59:3-10\n   #5  /prelude/array.fut:203:3-34\n   #6  /prelude/functional.fut:39:59-65\n   #7  /prelude/soacs.fut:59:3-10\n   #8  /prelude/array.fut:211:3-39\n   #9  sample_programs/tke.fut:340:19-357:17\n   #10 sample_programs/tke.fut:57:1-363:81\n");
                        err = FUTHARK_PROGRAM_ERROR;
                        goto cleanup;
                    }
                    
                    float y_36774;
                    
                    y_36774 = ((float *) mem_37773)[i_37333 * ctx_val_37824 +
                                                    i_36768 * zzdim_31404 +
                                                    i_37303];
                    
                    float x_36775 = x_36767 - y_36774;
                    float y_36776;
                    
                    y_36776 = ((float *) dyt_mem_37748.mem)[i_37307];
                    
                    float y_36777 = x_36762 * y_36776;
                    float y_36778 = x_36775 / y_36777;
                    float y_36779 = x_36766 - y_36778;
                    float tmp_t_res_36780 = x_36749 * y_36779;
                    
                    tmp_36736 = tmp_t_res_36780;
                } else {
                    float tmp_f_res_36793;
                    
                    tmp_f_res_36793 =
                        ((float *) dtketau_mem_37737.mem)[i_37333 *
                                                          (zzdim_31404 *
                                                           ydim_31403) +
                                                          i_37307 *
                                                          zzdim_31404 +
                                                          i_37303];
                    tmp_36736 = tmp_f_res_36793;
                }
                
                bool cond_36794 = i_37303 == (int64_t) 0;
                float zz0_update_36795;
                
                if (cond_36794) {
                    bool y_36802 = slt64((int64_t) 0, zzdim_31404);
                    bool index_certs_36805;
                    
                    if (!y_36802) {
                        ctx->error =
                            msgprintf("Error: %s%lld%s%lld%s%lld%s%lld%s%lld%s%lld%s\n\nBacktrace:\n%s",
                                      "Index [", (long long) i_37333, ", ",
                                      (long long) i_37307, ", ",
                                      (long long) (int64_t) 0,
                                      "] out of bounds for array of shape [",
                                      (long long) xdim_31402, "][",
                                      (long long) ydim_31403, "][",
                                      (long long) zzdim_31404, "].",
                                      "-> #0  sample_programs/tke.fut:350:62-78\n   #1  /prelude/soacs.fut:59:3-10\n   #2  /prelude/array.fut:195:3-17\n   #3  /prelude/functional.fut:39:59-65\n   #4  /prelude/soacs.fut:59:3-10\n   #5  /prelude/array.fut:203:3-34\n   #6  /prelude/functional.fut:39:59-65\n   #7  /prelude/soacs.fut:59:3-10\n   #8  /prelude/array.fut:211:3-39\n   #9  sample_programs/tke.fut:340:19-357:17\n   #10 sample_programs/tke.fut:57:1-363:81\n");
                        err = FUTHARK_PROGRAM_ERROR;
                        goto cleanup;
                    }
                    
                    float x_36806;
                    
                    x_36806 = ((float *) mem_37778)[i_37333 * ctx_val_37824 +
                                                    i_37307 * zzdim_31404];
                    
                    bool index_certs_36807;
                    
                    if (!y_36802) {
                        ctx->error =
                            msgprintf("Error: %s%lld%s%lld%s\n\nBacktrace:\n%s",
                                      "Index [", (long long) (int64_t) 0,
                                      "] out of bounds for array of shape [",
                                      (long long) zzdim_31404, "].",
                                      "-> #0  sample_programs/tke.fut:350:82-87\n   #1  /prelude/soacs.fut:59:3-10\n   #2  /prelude/array.fut:195:3-17\n   #3  /prelude/functional.fut:39:59-65\n   #4  /prelude/soacs.fut:59:3-10\n   #5  /prelude/array.fut:203:3-34\n   #6  /prelude/functional.fut:39:59-65\n   #7  /prelude/soacs.fut:59:3-10\n   #8  /prelude/array.fut:211:3-39\n   #9  sample_programs/tke.fut:340:19-357:17\n   #10 sample_programs/tke.fut:57:1-363:81\n");
                        err = FUTHARK_PROGRAM_ERROR;
                        goto cleanup;
                    }
                    
                    float y_36808;
                    
                    y_36808 = ((float *) dzzw_mem_37751.mem)[(int64_t) 0];
                    
                    float y_36809 = x_36806 / y_36808;
                    float zz0_update_t_res_36810 = tmp_36736 - y_36809;
                    
                    zz0_update_36795 = zz0_update_t_res_36810;
                } else {
                    zz0_update_36795 = tmp_36736;
                }
                
                bool cond_36811 = sle64((int64_t) 1, i_37303);
                bool cond_t_res_36812 = slt64(i_37303, y_35718);
                bool x_36813 = cond_36811 && cond_t_res_36812;
                float zz_middle_update_36814;
                
                if (x_36813) {
                    float x_36827;
                    
                    x_36827 = ((float *) mem_37778)[i_37333 * ctx_val_37824 +
                                                    i_37307 * zzdim_31404 +
                                                    i_37303];
                    
                    int64_t i_36828 = sub64(i_37303, (int64_t) 1);
                    bool x_36829 = sle64((int64_t) 0, i_36828);
                    bool y_36830 = slt64(i_36828, zzdim_31404);
                    bool bounds_check_36831 = x_36829 && y_36830;
                    bool index_certs_36834;
                    
                    if (!bounds_check_36831) {
                        ctx->error =
                            msgprintf("Error: %s%lld%s%lld%s%lld%s%lld%s%lld%s%lld%s\n\nBacktrace:\n%s",
                                      "Index [", (long long) i_37333, ", ",
                                      (long long) i_37307, ", ",
                                      (long long) i_36828,
                                      "] out of bounds for array of shape [",
                                      (long long) xdim_31402, "][",
                                      (long long) ydim_31403, "][",
                                      (long long) zzdim_31404, "].",
                                      "-> #0  sample_programs/tke.fut:352:87-105\n   #1  /prelude/soacs.fut:59:3-10\n   #2  /prelude/array.fut:195:3-17\n   #3  /prelude/functional.fut:39:59-65\n   #4  /prelude/soacs.fut:59:3-10\n   #5  /prelude/array.fut:203:3-34\n   #6  /prelude/functional.fut:39:59-65\n   #7  /prelude/soacs.fut:59:3-10\n   #8  /prelude/array.fut:211:3-39\n   #9  sample_programs/tke.fut:340:19-357:17\n   #10 sample_programs/tke.fut:57:1-363:81\n");
                        err = FUTHARK_PROGRAM_ERROR;
                        goto cleanup;
                    }
                    
                    float y_36835;
                    
                    y_36835 = ((float *) mem_37778)[i_37333 * ctx_val_37824 +
                                                    i_37307 * zzdim_31404 +
                                                    i_36828];
                    
                    float x_36836 = x_36827 - y_36835;
                    float y_36838;
                    
                    y_36838 = ((float *) dzzw_mem_37751.mem)[i_37303];
                    
                    float y_36839 = x_36836 / y_36838;
                    float zz_middle_update_t_res_36840 = zz0_update_36795 -
                          y_36839;
                    
                    zz_middle_update_36814 = zz_middle_update_t_res_36840;
                } else {
                    zz_middle_update_36814 = zz0_update_36795;
                }
                
                bool cond_36841 = i_37303 == y_35718;
                float defunc_0_f_res_36842;
                
                if (cond_36841) {
                    float x_36855;
                    
                    x_36855 = ((float *) mem_37778)[i_37333 * ctx_val_37824 +
                                                    i_37307 * zzdim_31404 +
                                                    i_37303];
                    
                    int64_t i_36856 = sub64(i_37303, (int64_t) 1);
                    bool x_36857 = sle64((int64_t) 0, i_36856);
                    bool y_36858 = slt64(i_36856, zzdim_31404);
                    bool bounds_check_36859 = x_36857 && y_36858;
                    bool index_certs_36862;
                    
                    if (!bounds_check_36859) {
                        ctx->error =
                            msgprintf("Error: %s%lld%s%lld%s%lld%s%lld%s%lld%s%lld%s\n\nBacktrace:\n%s",
                                      "Index [", (long long) i_37333, ", ",
                                      (long long) i_37307, ", ",
                                      (long long) i_36856,
                                      "] out of bounds for array of shape [",
                                      (long long) xdim_31402, "][",
                                      (long long) ydim_31403, "][",
                                      (long long) zzdim_31404, "].",
                                      "-> #0  sample_programs/tke.fut:354:87-105\n   #1  /prelude/soacs.fut:59:3-10\n   #2  /prelude/array.fut:195:3-17\n   #3  /prelude/functional.fut:39:59-65\n   #4  /prelude/soacs.fut:59:3-10\n   #5  /prelude/array.fut:203:3-34\n   #6  /prelude/functional.fut:39:59-65\n   #7  /prelude/soacs.fut:59:3-10\n   #8  /prelude/array.fut:211:3-39\n   #9  sample_programs/tke.fut:340:19-357:17\n   #10 sample_programs/tke.fut:57:1-363:81\n");
                        err = FUTHARK_PROGRAM_ERROR;
                        goto cleanup;
                    }
                    
                    float y_36863;
                    
                    y_36863 = ((float *) mem_37778)[i_37333 * ctx_val_37824 +
                                                    i_37307 * zzdim_31404 +
                                                    i_36856];
                    
                    float x_36864 = x_36855 - y_36863;
                    float y_36866;
                    
                    y_36866 = ((float *) dzzw_mem_37751.mem)[i_37303];
                    
                    float y_36867 = 0.5F * y_36866;
                    float y_36868 = x_36864 / y_36867;
                    float defunc_0_f_res_t_res_36869 = zz_middle_update_36814 -
                          y_36868;
                    
                    defunc_0_f_res_36842 = defunc_0_f_res_t_res_36869;
                } else {
                    defunc_0_f_res_36842 = zz_middle_update_36814;
                }
                ((float *) mem_38210)[i_37333 * ctx_val_37824 + i_37307 *
                                      zzdim_31404 + i_37303] =
                    defunc_0_f_res_36842;
            }
        }
        for (int64_t i_37315 = 0; i_37315 < ydim_31403; i_37315++) {
            bool cond_36397 = slt64(i_37315, y_36392);
            
            for (int64_t i_37311 = 0; i_37311 < zzdim_31404; i_37311++) {
                float defunc_0_f_res_36400;
                
                if (cond_36397) {
                    int64_t i_36404 = add64((int64_t) 1, i_37315);
                    bool x_36405 = sle64((int64_t) 0, i_36404);
                    bool y_36406 = slt64(i_36404, ydim_31403);
                    bool bounds_check_36407 = x_36405 && y_36406;
                    bool index_certs_36413;
                    
                    if (!bounds_check_36407) {
                        ctx->error =
                            msgprintf("Error: %s%lld%s%lld%s%lld%s%lld%s%lld%s%lld%s\n\nBacktrace:\n%s",
                                      "Index [", (long long) i_37333, ", ",
                                      (long long) i_36404, ", ",
                                      (long long) i_37311,
                                      "] out of bounds for array of shape [",
                                      (long long) xdim_31402, "][",
                                      (long long) ydim_31403, "][",
                                      (long long) zzdim_31404, "].",
                                      "-> #0  sample_programs/tke.fut:266:40-56\n   #1  /prelude/soacs.fut:59:3-10\n   #2  /prelude/array.fut:195:3-17\n   #3  /prelude/functional.fut:39:59-65\n   #4  /prelude/soacs.fut:59:3-10\n   #5  /prelude/array.fut:203:3-34\n   #6  /prelude/functional.fut:39:59-65\n   #7  /prelude/soacs.fut:59:3-10\n   #8  /prelude/array.fut:211:3-39\n   #9  sample_programs/tke.fut:263:22-269:21\n   #10 sample_programs/tke.fut:57:1-363:81\n");
                        err = FUTHARK_PROGRAM_ERROR;
                        goto cleanup;
                    }
                    
                    float x_36414;
                    
                    x_36414 = ((float *) tketau_mem_37734.mem)[i_37333 *
                                                               (zzdim_31404 *
                                                                ydim_31403) +
                                                               i_36404 *
                                                               zzdim_31404 +
                                                               i_37311];
                    
                    float y_36420;
                    
                    y_36420 = ((float *) tketau_mem_37734.mem)[i_37333 *
                                                               (zzdim_31404 *
                                                                ydim_31403) +
                                                               i_37315 *
                                                               zzdim_31404 +
                                                               i_37311];
                    
                    float y_36421 = x_36414 - y_36420;
                    float x_36422 = 2000.0F * y_36421;
                    float y_36424;
                    
                    y_36424 = ((float *) dyu_mem_37749.mem)[i_37315];
                    
                    float x_36425 = x_36422 / y_36424;
                    float y_36426;
                    
                    y_36426 = ((float *) maskV_mem_37744.mem)[i_37333 *
                                                              (zzdim_31404 *
                                                               ydim_31403) +
                                                              i_37315 *
                                                              zzdim_31404 +
                                                              i_37311];
                    
                    float x_36427 = x_36425 * y_36426;
                    float y_36428;
                    
                    y_36428 = ((float *) cosu_mem_37753.mem)[i_37315];
                    
                    float defunc_0_f_res_t_res_36429 = x_36427 * y_36428;
                    
                    defunc_0_f_res_36400 = defunc_0_f_res_t_res_36429;
                } else {
                    defunc_0_f_res_36400 = 0.0F;
                }
                ((float *) mem_38215)[i_37333 * ctx_val_37824 + i_37315 *
                                      zzdim_31404 + i_37311] =
                    defunc_0_f_res_36400;
            }
        }
        
        bool cond_36355 = slt64(i_37333, y_36352);
        
        for (int64_t i_37323 = 0; i_37323 < ydim_31403; i_37323++) {
            for (int64_t i_37319 = 0; i_37319 < zzdim_31404; i_37319++) {
                float defunc_0_f_res_36360;
                
                if (cond_36355) {
                    int64_t i_36361 = add64((int64_t) 1, i_37333);
                    bool x_36362 = sle64((int64_t) 0, i_36361);
                    bool y_36363 = slt64(i_36361, xdim_31402);
                    bool bounds_check_36364 = x_36362 && y_36363;
                    bool index_certs_36373;
                    
                    if (!bounds_check_36364) {
                        ctx->error =
                            msgprintf("Error: %s%lld%s%lld%s%lld%s%lld%s%lld%s%lld%s\n\nBacktrace:\n%s",
                                      "Index [", (long long) i_36361, ", ",
                                      (long long) i_37323, ", ",
                                      (long long) i_37319,
                                      "] out of bounds for array of shape [",
                                      (long long) xdim_31402, "][",
                                      (long long) ydim_31403, "][",
                                      (long long) zzdim_31404, "].",
                                      "-> #0  sample_programs/tke.fut:257:40-56\n   #1  /prelude/soacs.fut:59:3-10\n   #2  /prelude/array.fut:195:3-17\n   #3  /prelude/functional.fut:39:59-65\n   #4  /prelude/soacs.fut:59:3-10\n   #5  /prelude/array.fut:203:3-34\n   #6  /prelude/functional.fut:39:59-65\n   #7  /prelude/soacs.fut:59:3-10\n   #8  /prelude/array.fut:211:3-39\n   #9  sample_programs/tke.fut:254:21-261:5\n   #10 sample_programs/tke.fut:57:1-363:81\n");
                        err = FUTHARK_PROGRAM_ERROR;
                        goto cleanup;
                    }
                    
                    float x_36374;
                    
                    x_36374 = ((float *) tketau_mem_37734.mem)[i_36361 *
                                                               (zzdim_31404 *
                                                                ydim_31403) +
                                                               i_37323 *
                                                               zzdim_31404 +
                                                               i_37319];
                    
                    float y_36381;
                    
                    y_36381 = ((float *) tketau_mem_37734.mem)[i_37333 *
                                                               (zzdim_31404 *
                                                                ydim_31403) +
                                                               i_37323 *
                                                               zzdim_31404 +
                                                               i_37319];
                    
                    float y_36382 = x_36374 - y_36381;
                    float x_36383 = 2000.0F * y_36382;
                    float x_36385;
                    
                    x_36385 = ((float *) cost_mem_37752.mem)[i_37323];
                    
                    float y_36387;
                    
                    y_36387 = ((float *) dxu_mem_37747.mem)[i_37333];
                    
                    float y_36388 = x_36385 * y_36387;
                    float x_36389 = x_36383 / y_36388;
                    float y_36390;
                    
                    y_36390 = ((float *) maskU_mem_37743.mem)[i_37333 *
                                                              (zzdim_31404 *
                                                               ydim_31403) +
                                                              i_37323 *
                                                              zzdim_31404 +
                                                              i_37319];
                    
                    float defunc_0_f_res_t_res_36391 = x_36389 * y_36390;
                    
                    defunc_0_f_res_36360 = defunc_0_f_res_t_res_36391;
                } else {
                    defunc_0_f_res_36360 = 0.0F;
                }
                ((float *) mem_38220)[i_37333 * ctx_val_37824 + i_37323 *
                                      zzdim_31404 + i_37319] =
                    defunc_0_f_res_36360;
            }
        }
    }
    if (mem_39090_cached_sizze_39847 < bytes_37762) {
        err = lexical_realloc(&ctx->error, &mem_39090,
                              &mem_39090_cached_sizze_39847, bytes_37762);
        if (err != FUTHARK_SUCCESS)
            goto cleanup;
    }
    for (int64_t i_37348 = 0; i_37348 < xdim_31402; i_37348++) {
        bool cond_36225 = sle64((int64_t) 2, i_37348);
        bool cond_t_res_36226 = slt64(i_37348, y_35716);
        bool x_36227 = cond_36225 && cond_t_res_36226;
        
        for (int64_t i_37344 = 0; i_37344 < ydim_31403; i_37344++) {
            int32_t x_36235;
            
            x_36235 = ((int32_t *) kbot_mem_37754.mem)[i_37348 * ydim_31403 +
                                                       i_37344];
            
            int32_t ks_val_36236 = sub32(x_36235, 1);
            bool cond_36237 = sle32(0, ks_val_36236);
            bool cond_t_res_36238 = sle64((int64_t) 2, i_37344);
            bool x_36239 = x_36227 && cond_t_res_36238;
            bool cond_t_res_36240 = slt64(i_37344, y_35717);
            bool x_36241 = x_36239 && cond_t_res_36240;
            
            for (int64_t i_37340 = 0; i_37340 < zzdim_31404; i_37340++) {
                int32_t i64_res_36244 = sext_i64_i32(i_37340);
                bool water_mask_t_res_36245 = sle32(ks_val_36236,
                                                    i64_res_36244);
                bool x_36246 = cond_36237 && water_mask_t_res_36245;
                bool x_36247 = x_36241 && x_36246;
                float defunc_0_f_res_36248;
                
                if (x_36247) {
                    float defunc_0_f_res_t_res_36261;
                    
                    defunc_0_f_res_t_res_36261 = ((float *) mem_38205)[i_37348 *
                                                                       ctx_val_37824 +
                                                                       i_37344 *
                                                                       zzdim_31404 +
                                                                       i_37340];
                    defunc_0_f_res_36248 = defunc_0_f_res_t_res_36261;
                } else {
                    float defunc_0_f_res_f_res_36274;
                    
                    defunc_0_f_res_f_res_36274 =
                        ((float *) tketaup1_mem_37735.mem)[i_37348 *
                                                           (zzdim_31404 *
                                                            ydim_31403) +
                                                           i_37344 *
                                                           zzdim_31404 +
                                                           i_37340];
                    defunc_0_f_res_36248 = defunc_0_f_res_f_res_36274;
                }
                ((float *) mem_39090)[i_37348 * ctx_val_37824 + i_37344 *
                                      zzdim_31404 + i_37340] =
                    defunc_0_f_res_36248;
            }
        }
    }
    if (mem_39193_cached_sizze_39848 < bytes_37762) {
        err = lexical_realloc(&ctx->error, &mem_39193,
                              &mem_39193_cached_sizze_39848, bytes_37762);
        if (err != FUTHARK_SUCCESS)
            goto cleanup;
    }
    for (int64_t i_37360 = 0; i_37360 < xdim_31402; i_37360++) {
        bool cond_36310 = sle64((int64_t) 2, i_37360);
        bool cond_t_res_36311 = slt64(i_37360, y_35716);
        bool x_36312 = cond_36310 && cond_t_res_36311;
        
        for (int64_t i_37356 = 0; i_37356 < ydim_31403; i_37356++) {
            bool cond_t_res_36315 = sle64((int64_t) 2, i_37356);
            bool x_36316 = x_36312 && cond_t_res_36315;
            bool cond_t_res_36317 = slt64(i_37356, y_35717);
            bool x_36318 = x_36316 && cond_t_res_36317;
            
            for (int64_t i_37352 = 0; i_37352 < zzdim_31404; i_37352++) {
                bool cond_t_res_36321 = i_37352 == y_35718;
                bool x_36322 = x_36318 && cond_t_res_36321;
                float defunc_0_f_res_36323;
                
                if (x_36322) {
                    float tke_val_36336;
                    
                    tke_val_36336 = ((float *) mem_39090)[i_37360 *
                                                          ctx_val_37824 +
                                                          i_37356 *
                                                          zzdim_31404 +
                                                          i_37352];
                    
                    bool cond_36337 = tke_val_36336 < 0.0F;
                    float defunc_0_f_res_t_res_36338;
                    
                    if (cond_36337) {
                        defunc_0_f_res_t_res_36338 = 0.0F;
                    } else {
                        defunc_0_f_res_t_res_36338 = tke_val_36336;
                    }
                    defunc_0_f_res_36323 = defunc_0_f_res_t_res_36338;
                } else {
                    float defunc_0_f_res_f_res_36351;
                    
                    defunc_0_f_res_f_res_36351 = ((float *) mem_39090)[i_37360 *
                                                                       ctx_val_37824 +
                                                                       i_37356 *
                                                                       zzdim_31404 +
                                                                       i_37352];
                    defunc_0_f_res_36323 = defunc_0_f_res_f_res_36351;
                }
                ((float *) mem_39193)[i_37360 * ctx_val_37824 + i_37356 *
                                      zzdim_31404 + i_37352] =
                    defunc_0_f_res_36323;
            }
        }
    }
    if (mem_39296_cached_sizze_39849 < bytes_37762) {
        err = lexical_realloc(&ctx->error, &mem_39296,
                              &mem_39296_cached_sizze_39849, bytes_37762);
        if (err != FUTHARK_SUCCESS)
            goto cleanup;
    }
    for (int64_t i_37372 = 0; i_37372 < xdim_31402; i_37372++) {
        bool cond_36435 = sle64((int64_t) 2, i_37372);
        bool cond_t_res_36436 = slt64(i_37372, y_35716);
        bool x_36437 = cond_36435 && cond_t_res_36436;
        
        for (int64_t i_37368 = 0; i_37368 < ydim_31403; i_37368++) {
            bool cond_t_res_36443 = sle64((int64_t) 2, i_37368);
            bool x_36444 = x_36437 && cond_t_res_36443;
            bool cond_t_res_36445 = slt64(i_37368, y_35717);
            bool x_36446 = x_36444 && cond_t_res_36445;
            
            for (int64_t i_37364 = 0; i_37364 < zzdim_31404; i_37364++) {
                float previous_36455;
                
                previous_36455 = ((float *) mem_39193)[i_37372 * ctx_val_37824 +
                                                       i_37368 * zzdim_31404 +
                                                       i_37364];
                
                bool cond_t_res_36456 = i_37364 == y_35718;
                bool x_36457 = x_36446 && cond_t_res_36456;
                float defunc_0_f_res_36458;
                
                if (x_36457) {
                    float y_36468;
                    
                    y_36468 = ((float *) maskW_mem_37745.mem)[i_37372 *
                                                              (zzdim_31404 *
                                                               ydim_31403) +
                                                              i_37368 *
                                                              zzdim_31404 +
                                                              i_37364];
                    
                    float x_36469;
                    
                    x_36469 = ((float *) mem_38220)[i_37372 * ctx_val_37824 +
                                                    i_37368 * zzdim_31404 +
                                                    i_37364];
                    
                    int64_t i_36470 = sub64(i_37372, (int64_t) 1);
                    bool x_36471 = sle64((int64_t) 0, i_36470);
                    bool y_36472 = slt64(i_36470, xdim_31402);
                    bool bounds_check_36473 = x_36471 && y_36472;
                    bool index_certs_36476;
                    
                    if (!bounds_check_36473) {
                        ctx->error =
                            msgprintf("Error: %s%lld%s%lld%s%lld%s%lld%s%lld%s%lld%s\n\nBacktrace:\n%s",
                                      "Index [", (long long) i_36470, ", ",
                                      (long long) i_37368, ", ",
                                      (long long) i_37364,
                                      "] out of bounds for array of shape [",
                                      (long long) xdim_31402, "][",
                                      (long long) ydim_31403, "][",
                                      (long long) zzdim_31404, "].",
                                      "-> #0  sample_programs/tke.fut:275:54-73\n   #1  /prelude/soacs.fut:59:3-10\n   #2  /prelude/array.fut:195:3-17\n   #3  /prelude/functional.fut:39:59-65\n   #4  /prelude/soacs.fut:59:3-10\n   #5  /prelude/array.fut:203:3-34\n   #6  /prelude/functional.fut:39:59-65\n   #7  /prelude/soacs.fut:59:3-10\n   #8  /prelude/array.fut:211:3-39\n   #9  sample_programs/tke.fut:271:20-280:21\n   #10 sample_programs/tke.fut:57:1-363:81\n");
                        err = FUTHARK_PROGRAM_ERROR;
                        goto cleanup;
                    }
                    
                    float y_36477;
                    
                    y_36477 = ((float *) mem_38220)[i_36470 * ctx_val_37824 +
                                                    i_37368 * zzdim_31404 +
                                                    i_37364];
                    
                    float x_36478 = x_36469 - y_36477;
                    float x_36480;
                    
                    x_36480 = ((float *) cost_mem_37752.mem)[i_37368];
                    
                    float y_36482;
                    
                    y_36482 = ((float *) dxt_mem_37746.mem)[i_37372];
                    
                    float y_36483 = x_36480 * y_36482;
                    float x_36484 = x_36478 / y_36483;
                    float x_36485;
                    
                    x_36485 = ((float *) mem_38215)[i_37372 * ctx_val_37824 +
                                                    i_37368 * zzdim_31404 +
                                                    i_37364];
                    
                    int64_t i_36486 = sub64(i_37368, (int64_t) 1);
                    bool x_36487 = sle64((int64_t) 0, i_36486);
                    bool y_36488 = slt64(i_36486, ydim_31403);
                    bool bounds_check_36489 = x_36487 && y_36488;
                    bool index_certs_36491;
                    
                    if (!bounds_check_36489) {
                        ctx->error =
                            msgprintf("Error: %s%lld%s%lld%s%lld%s%lld%s%lld%s%lld%s\n\nBacktrace:\n%s",
                                      "Index [", (long long) i_37372, ", ",
                                      (long long) i_36486, ", ",
                                      (long long) i_37364,
                                      "] out of bounds for array of shape [",
                                      (long long) xdim_31402, "][",
                                      (long long) ydim_31403, "][",
                                      (long long) zzdim_31404, "].",
                                      "-> #0  sample_programs/tke.fut:277:56-76\n   #1  /prelude/soacs.fut:59:3-10\n   #2  /prelude/array.fut:195:3-17\n   #3  /prelude/functional.fut:39:59-65\n   #4  /prelude/soacs.fut:59:3-10\n   #5  /prelude/array.fut:203:3-34\n   #6  /prelude/functional.fut:39:59-65\n   #7  /prelude/soacs.fut:59:3-10\n   #8  /prelude/array.fut:211:3-39\n   #9  sample_programs/tke.fut:271:20-280:21\n   #10 sample_programs/tke.fut:57:1-363:81\n");
                        err = FUTHARK_PROGRAM_ERROR;
                        goto cleanup;
                    }
                    
                    float y_36492;
                    
                    y_36492 = ((float *) mem_38215)[i_37372 * ctx_val_37824 +
                                                    i_36486 * zzdim_31404 +
                                                    i_37364];
                    
                    float x_36493 = x_36485 - y_36492;
                    float y_36494;
                    
                    y_36494 = ((float *) dyt_mem_37748.mem)[i_37368];
                    
                    float y_36495 = x_36480 * y_36494;
                    float y_36496 = x_36493 / y_36495;
                    float y_36497 = x_36484 + y_36496;
                    float y_36498 = y_36468 * y_36497;
                    float defunc_0_f_res_t_res_36499 = previous_36455 + y_36498;
                    
                    defunc_0_f_res_36458 = defunc_0_f_res_t_res_36499;
                } else {
                    defunc_0_f_res_36458 = previous_36455;
                }
                ((float *) mem_39296)[i_37372 * ctx_val_37824 + i_37368 *
                                      zzdim_31404 + i_37364] =
                    defunc_0_f_res_36458;
            }
        }
    }
    
    int64_t binop_y_39396 = (int64_t) 4 * binop_x_37759;
    int64_t bytes_39397 = smax64((int64_t) 0, binop_y_39396);
    
    if (mem_39398_cached_sizze_39850 < bytes_39397) {
        err = lexical_realloc(&ctx->error, &mem_39398,
                              &mem_39398_cached_sizze_39850, bytes_39397);
        if (err != FUTHARK_SUCCESS)
            goto cleanup;
    }
    if (mem_39403_cached_sizze_39851 < bytes_37762) {
        err = lexical_realloc(&ctx->error, &mem_39403,
                              &mem_39403_cached_sizze_39851, bytes_37762);
        if (err != FUTHARK_SUCCESS)
            goto cleanup;
    }
    for (int64_t i_37390 = 0; i_37390 < xdim_31402; i_37390++) {
        bool cond_36277 = sle64((int64_t) 2, i_37390);
        bool cond_t_res_36278 = slt64(i_37390, y_35716);
        bool x_36279 = cond_36277 && cond_t_res_36278;
        
        for (int64_t i_37376 = 0; i_37376 < ydim_31403; i_37376++) {
            bool cond_t_res_36282 = sle64((int64_t) 2, i_37376);
            bool x_36283 = x_36279 && cond_t_res_36282;
            bool cond_t_res_36284 = slt64(i_37376, y_35717);
            bool x_36285 = x_36283 && cond_t_res_36284;
            float defunc_0_f_res_36286;
            
            if (x_36285) {
                bool x_36294 = sle64((int64_t) 0, y_35718);
                bool y_36295 = slt64(y_35718, zzdim_31404);
                bool bounds_check_36296 = x_36294 && y_36295;
                bool index_certs_36299;
                
                if (!bounds_check_36296) {
                    ctx->error =
                        msgprintf("Error: %s%lld%s%lld%s%lld%s%lld%s%lld%s%lld%s\n\nBacktrace:\n%s",
                                  "Index [", (long long) i_37390, ", ",
                                  (long long) i_37376, ", ",
                                  (long long) y_35718,
                                  "] out of bounds for array of shape [",
                                  (long long) xdim_31402, "][",
                                  (long long) ydim_31403, "][",
                                  (long long) zzdim_31404, "].",
                                  "-> #0  sample_programs/tke.fut:237:39-60\n   #1  /prelude/soacs.fut:59:3-10\n   #2  /prelude/array.fut:195:3-17\n   #3  /prelude/functional.fut:39:59-65\n   #4  /prelude/soacs.fut:59:3-10\n   #5  /prelude/array.fut:203:3-34\n   #6  sample_programs/tke.fut:234:25-242:21\n   #7  sample_programs/tke.fut:57:1-363:81\n");
                    err = FUTHARK_PROGRAM_ERROR;
                    goto cleanup;
                }
                
                float tke_val_36300;
                
                tke_val_36300 = ((float *) mem_39090)[i_37390 * ctx_val_37824 +
                                                      i_37376 * zzdim_31404 +
                                                      y_35718];
                
                bool cond_36301 = tke_val_36300 < 0.0F;
                float defunc_0_f_res_t_res_36302;
                
                if (cond_36301) {
                    float x_36303 = 0.0F - tke_val_36300;
                    float x_36304 = 0.5F * x_36303;
                    bool index_certs_36305;
                    
                    if (!bounds_check_36296) {
                        ctx->error =
                            msgprintf("Error: %s%lld%s%lld%s\n\nBacktrace:\n%s",
                                      "Index [", (long long) y_35718,
                                      "] out of bounds for array of shape [",
                                      (long long) zzdim_31404, "].",
                                      "-> #0  sample_programs/tke.fut:239:51-61\n   #1  /prelude/soacs.fut:59:3-10\n   #2  /prelude/array.fut:195:3-17\n   #3  /prelude/functional.fut:39:59-65\n   #4  /prelude/soacs.fut:59:3-10\n   #5  /prelude/array.fut:203:3-34\n   #6  sample_programs/tke.fut:234:25-242:21\n   #7  sample_programs/tke.fut:57:1-363:81\n");
                        err = FUTHARK_PROGRAM_ERROR;
                        goto cleanup;
                    }
                    
                    float y_36306;
                    
                    y_36306 = ((float *) dzzw_mem_37751.mem)[y_35718];
                    
                    float x_36307 = x_36304 * y_36306;
                    
                    defunc_0_f_res_t_res_36302 = x_36307;
                } else {
                    defunc_0_f_res_t_res_36302 = 0.0F;
                }
                defunc_0_f_res_36286 = defunc_0_f_res_t_res_36302;
            } else {
                defunc_0_f_res_36286 = 0.0F;
            }
            ((float *) mem_39398)[i_37390 * ydim_31403 + i_37376] =
                defunc_0_f_res_36286;
        }
        for (int64_t i_37384 = 0; i_37384 < ydim_31403; i_37384++) {
            for (int64_t i_37380 = 0; i_37380 < zzdim_31404; i_37380++) {
                float x_36888;
                
                x_36888 = ((float *) mem_39296)[i_37390 * ctx_val_37824 +
                                                i_37384 * zzdim_31404 +
                                                i_37380];
                
                float y_36889;
                
                y_36889 = ((float *) mem_38210)[i_37390 * ctx_val_37824 +
                                                i_37384 * zzdim_31404 +
                                                i_37380];
                
                float x_36890 = 1.6F * y_36889;
                float y_36891;
                
                y_36891 = ((float *) dtketaum1_mem_37739.mem)[i_37390 *
                                                              (zzdim_31404 *
                                                               ydim_31403) +
                                                              i_37384 *
                                                              zzdim_31404 +
                                                              i_37380];
                
                float y_36892 = 0.6F * y_36891;
                float y_36893 = x_36890 - y_36892;
                float defunc_0_f_res_36894 = x_36888 + y_36893;
                
                ((float *) mem_39403)[i_37390 * ctx_val_37824 + i_37384 *
                                      zzdim_31404 + i_37380] =
                    defunc_0_f_res_36894;
            }
        }
    }
    if (memblock_alloc(ctx, &mem_39549, bytes_37762, "mem_39549")) {
        err = 1;
        goto cleanup;
    }
    if (xdim_31402 * ydim_31403 * zzdim_31404 * (int64_t) 4 > 0)
        memmove(mem_39549.mem + (int64_t) 0, mem_39403 + (int64_t) 0,
                xdim_31402 * ydim_31403 * zzdim_31404 * (int64_t) 4);
    if (memblock_alloc(ctx, &mem_39555, bytes_37762, "mem_39555")) {
        err = 1;
        goto cleanup;
    }
    if (xdim_31402 * ydim_31403 * zzdim_31404 * (int64_t) 4 > 0)
        memmove(mem_39555.mem + (int64_t) 0, mem_38210 + (int64_t) 0,
                xdim_31402 * ydim_31403 * zzdim_31404 * (int64_t) 4);
    if (memblock_alloc(ctx, &mem_39560, bytes_39397, "mem_39560")) {
        err = 1;
        goto cleanup;
    }
    if (xdim_31402 * ydim_31403 * (int64_t) 4 > 0)
        memmove(mem_39560.mem + (int64_t) 0, mem_39398 + (int64_t) 0,
                xdim_31402 * ydim_31403 * (int64_t) 4);
    if (memblock_set(ctx, &mem_out_39725, &tketau_mem_37734,
                     "tketau_mem_37734") != 0)
        return 1;
    if (memblock_set(ctx, &mem_out_39726, &mem_39549, "mem_39549") != 0)
        return 1;
    if (memblock_set(ctx, &mem_out_39727, &tketaum1_mem_37736,
                     "tketaum1_mem_37736") != 0)
        return 1;
    if (memblock_set(ctx, &mem_out_39728, &mem_39555, "mem_39555") != 0)
        return 1;
    if (memblock_set(ctx, &mem_out_39729, &dtketaup1_mem_37738,
                     "dtketaup1_mem_37738") != 0)
        return 1;
    if (memblock_set(ctx, &mem_out_39730, &dtketaum1_mem_37739,
                     "dtketaum1_mem_37739") != 0)
        return 1;
    if (memblock_set(ctx, &mem_out_39731, &mem_39560, "mem_39560") != 0)
        return 1;
    (*mem_out_p_39807).references = NULL;
    if (memblock_set(ctx, &*mem_out_p_39807, &mem_out_39725, "mem_out_39725") !=
        0)
        return 1;
    (*mem_out_p_39808).references = NULL;
    if (memblock_set(ctx, &*mem_out_p_39808, &mem_out_39726, "mem_out_39726") !=
        0)
        return 1;
    (*mem_out_p_39809).references = NULL;
    if (memblock_set(ctx, &*mem_out_p_39809, &mem_out_39727, "mem_out_39727") !=
        0)
        return 1;
    (*mem_out_p_39810).references = NULL;
    if (memblock_set(ctx, &*mem_out_p_39810, &mem_out_39728, "mem_out_39728") !=
        0)
        return 1;
    (*mem_out_p_39811).references = NULL;
    if (memblock_set(ctx, &*mem_out_p_39811, &mem_out_39729, "mem_out_39729") !=
        0)
        return 1;
    (*mem_out_p_39812).references = NULL;
    if (memblock_set(ctx, &*mem_out_p_39812, &mem_out_39730, "mem_out_39730") !=
        0)
        return 1;
    (*mem_out_p_39813).references = NULL;
    if (memblock_set(ctx, &*mem_out_p_39813, &mem_out_39731, "mem_out_39731") !=
        0)
        return 1;
    
  cleanup:
    {
        free(mem_37763);
        free(mem_37768);
        free(mem_37773);
        free(mem_37778);
        free(mem_38173);
        free(mem_38188);
        free(mem_38205);
        free(mem_38210);
        free(mem_38215);
        free(mem_38220);
        free(mem_38280);
        free(mem_38327);
        free(mem_38374);
        free(mem_38421);
        free(mem_38482);
        free(mem_38485);
        free(mem_38488);
        free(mem_38491);
        free(mem_38542);
        free(mem_38545);
        free(mem_38548);
        free(mem_38551);
        free(mem_38602);
        free(mem_38617);
        free(mem_38620);
        free(mem_38647);
        free(mem_38650);
        free(mem_38677);
        free(mem_38692);
        free(mem_38695);
        free(mem_38722);
        free(mem_38725);
        free(mem_38752);
        free(mem_39090);
        free(mem_39193);
        free(mem_39296);
        free(mem_39398);
        free(mem_39403);
        if (memblock_unref(ctx, &mem_39560, "mem_39560") != 0)
            return 1;
        if (memblock_unref(ctx, &mem_39555, "mem_39555") != 0)
            return 1;
        if (memblock_unref(ctx, &mem_39549, "mem_39549") != 0)
            return 1;
        if (memblock_unref(ctx, &mem_out_39731, "mem_out_39731") != 0)
            return 1;
        if (memblock_unref(ctx, &mem_out_39730, "mem_out_39730") != 0)
            return 1;
        if (memblock_unref(ctx, &mem_out_39729, "mem_out_39729") != 0)
            return 1;
        if (memblock_unref(ctx, &mem_out_39728, "mem_out_39728") != 0)
            return 1;
        if (memblock_unref(ctx, &mem_out_39727, "mem_out_39727") != 0)
            return 1;
        if (memblock_unref(ctx, &mem_out_39726, "mem_out_39726") != 0)
            return 1;
        if (memblock_unref(ctx, &mem_out_39725, "mem_out_39725") != 0)
            return 1;
    }
    return err;
}

int futhark_entry_main(struct futhark_context *ctx,
                       struct futhark_f32_3d **out0,
                       struct futhark_f32_3d **out1,
                       struct futhark_f32_3d **out2,
                       struct futhark_f32_3d **out3,
                       struct futhark_f32_3d **out4,
                       struct futhark_f32_3d **out5,
                       struct futhark_f32_2d **out6, const
                       struct futhark_f32_3d *in0, const
                       struct futhark_f32_3d *in1, const
                       struct futhark_f32_3d *in2, const
                       struct futhark_f32_3d *in3, const
                       struct futhark_f32_3d *in4, const
                       struct futhark_f32_3d *in5, const
                       struct futhark_f32_3d *in6, const
                       struct futhark_f32_3d *in7, const
                       struct futhark_f32_3d *in8, const
                       struct futhark_f32_3d *in9, const
                       struct futhark_f32_3d *in10, const
                       struct futhark_f32_3d *in11, const
                       struct futhark_f32_1d *in12, const
                       struct futhark_f32_1d *in13, const
                       struct futhark_f32_1d *in14, const
                       struct futhark_f32_1d *in15, const
                       struct futhark_f32_1d *in16, const
                       struct futhark_f32_1d *in17, const
                       struct futhark_f32_1d *in18, const
                       struct futhark_f32_1d *in19, const
                       struct futhark_i32_2d *in20, const
                       struct futhark_f32_3d *in21, const
                       struct futhark_f32_3d *in22, const
                       struct futhark_f32_3d *in23, const
                       struct futhark_f32_2d *in24)
{
    int64_t xdim_31402;
    int64_t ydim_31403;
    int64_t zzdim_31404;
    int ret = 0;
    
    lock_lock(&ctx->lock);
    
    struct memblock mem_out_39731;
    
    mem_out_39731.references = NULL;
    
    struct memblock mem_out_39730;
    
    mem_out_39730.references = NULL;
    
    struct memblock mem_out_39729;
    
    mem_out_39729.references = NULL;
    
    struct memblock mem_out_39728;
    
    mem_out_39728.references = NULL;
    
    struct memblock mem_out_39727;
    
    mem_out_39727.references = NULL;
    
    struct memblock mem_out_39726;
    
    mem_out_39726.references = NULL;
    
    struct memblock mem_out_39725;
    
    mem_out_39725.references = NULL;
    
    struct memblock forc_tke_surface_mem_37758;
    
    forc_tke_surface_mem_37758.references = NULL;
    
    struct memblock forc_mem_37757;
    
    forc_mem_37757.references = NULL;
    
    struct memblock mxl_mem_37756;
    
    mxl_mem_37756.references = NULL;
    
    struct memblock kappaM_mem_37755;
    
    kappaM_mem_37755.references = NULL;
    
    struct memblock kbot_mem_37754;
    
    kbot_mem_37754.references = NULL;
    
    struct memblock cosu_mem_37753;
    
    cosu_mem_37753.references = NULL;
    
    struct memblock cost_mem_37752;
    
    cost_mem_37752.references = NULL;
    
    struct memblock dzzw_mem_37751;
    
    dzzw_mem_37751.references = NULL;
    
    struct memblock dzzt_mem_37750;
    
    dzzt_mem_37750.references = NULL;
    
    struct memblock dyu_mem_37749;
    
    dyu_mem_37749.references = NULL;
    
    struct memblock dyt_mem_37748;
    
    dyt_mem_37748.references = NULL;
    
    struct memblock dxu_mem_37747;
    
    dxu_mem_37747.references = NULL;
    
    struct memblock dxt_mem_37746;
    
    dxt_mem_37746.references = NULL;
    
    struct memblock maskW_mem_37745;
    
    maskW_mem_37745.references = NULL;
    
    struct memblock maskV_mem_37744;
    
    maskV_mem_37744.references = NULL;
    
    struct memblock maskU_mem_37743;
    
    maskU_mem_37743.references = NULL;
    
    struct memblock wtau_mem_37742;
    
    wtau_mem_37742.references = NULL;
    
    struct memblock vtau_mem_37741;
    
    vtau_mem_37741.references = NULL;
    
    struct memblock utau_mem_37740;
    
    utau_mem_37740.references = NULL;
    
    struct memblock dtketaum1_mem_37739;
    
    dtketaum1_mem_37739.references = NULL;
    
    struct memblock dtketaup1_mem_37738;
    
    dtketaup1_mem_37738.references = NULL;
    
    struct memblock dtketau_mem_37737;
    
    dtketau_mem_37737.references = NULL;
    
    struct memblock tketaum1_mem_37736;
    
    tketaum1_mem_37736.references = NULL;
    
    struct memblock tketaup1_mem_37735;
    
    tketaup1_mem_37735.references = NULL;
    
    struct memblock tketau_mem_37734;
    
    tketau_mem_37734.references = NULL;
    tketau_mem_37734 = in0->mem;
    xdim_31402 = in0->shape[0];
    ydim_31403 = in0->shape[1];
    zzdim_31404 = in0->shape[2];
    tketaup1_mem_37735 = in1->mem;
    xdim_31402 = in1->shape[0];
    ydim_31403 = in1->shape[1];
    zzdim_31404 = in1->shape[2];
    tketaum1_mem_37736 = in2->mem;
    xdim_31402 = in2->shape[0];
    ydim_31403 = in2->shape[1];
    zzdim_31404 = in2->shape[2];
    dtketau_mem_37737 = in3->mem;
    xdim_31402 = in3->shape[0];
    ydim_31403 = in3->shape[1];
    zzdim_31404 = in3->shape[2];
    dtketaup1_mem_37738 = in4->mem;
    xdim_31402 = in4->shape[0];
    ydim_31403 = in4->shape[1];
    zzdim_31404 = in4->shape[2];
    dtketaum1_mem_37739 = in5->mem;
    xdim_31402 = in5->shape[0];
    ydim_31403 = in5->shape[1];
    zzdim_31404 = in5->shape[2];
    utau_mem_37740 = in6->mem;
    xdim_31402 = in6->shape[0];
    ydim_31403 = in6->shape[1];
    zzdim_31404 = in6->shape[2];
    vtau_mem_37741 = in7->mem;
    xdim_31402 = in7->shape[0];
    ydim_31403 = in7->shape[1];
    zzdim_31404 = in7->shape[2];
    wtau_mem_37742 = in8->mem;
    xdim_31402 = in8->shape[0];
    ydim_31403 = in8->shape[1];
    zzdim_31404 = in8->shape[2];
    maskU_mem_37743 = in9->mem;
    xdim_31402 = in9->shape[0];
    ydim_31403 = in9->shape[1];
    zzdim_31404 = in9->shape[2];
    maskV_mem_37744 = in10->mem;
    xdim_31402 = in10->shape[0];
    ydim_31403 = in10->shape[1];
    zzdim_31404 = in10->shape[2];
    maskW_mem_37745 = in11->mem;
    xdim_31402 = in11->shape[0];
    ydim_31403 = in11->shape[1];
    zzdim_31404 = in11->shape[2];
    dxt_mem_37746 = in12->mem;
    xdim_31402 = in12->shape[0];
    dxu_mem_37747 = in13->mem;
    xdim_31402 = in13->shape[0];
    dyt_mem_37748 = in14->mem;
    ydim_31403 = in14->shape[0];
    dyu_mem_37749 = in15->mem;
    ydim_31403 = in15->shape[0];
    dzzt_mem_37750 = in16->mem;
    zzdim_31404 = in16->shape[0];
    dzzw_mem_37751 = in17->mem;
    zzdim_31404 = in17->shape[0];
    cost_mem_37752 = in18->mem;
    ydim_31403 = in18->shape[0];
    cosu_mem_37753 = in19->mem;
    ydim_31403 = in19->shape[0];
    kbot_mem_37754 = in20->mem;
    xdim_31402 = in20->shape[0];
    ydim_31403 = in20->shape[1];
    kappaM_mem_37755 = in21->mem;
    xdim_31402 = in21->shape[0];
    ydim_31403 = in21->shape[1];
    zzdim_31404 = in21->shape[2];
    mxl_mem_37756 = in22->mem;
    xdim_31402 = in22->shape[0];
    ydim_31403 = in22->shape[1];
    zzdim_31404 = in22->shape[2];
    forc_mem_37757 = in23->mem;
    xdim_31402 = in23->shape[0];
    ydim_31403 = in23->shape[1];
    zzdim_31404 = in23->shape[2];
    forc_tke_surface_mem_37758 = in24->mem;
    xdim_31402 = in24->shape[0];
    ydim_31403 = in24->shape[1];
    if (!((xdim_31402 == in0->shape[0] && (ydim_31403 == in0->shape[1] &&
                                           zzdim_31404 == in0->shape[2])) &&
          ((xdim_31402 == in1->shape[0] && (ydim_31403 == in1->shape[1] &&
                                            zzdim_31404 == in1->shape[2])) &&
           ((xdim_31402 == in2->shape[0] && (ydim_31403 == in2->shape[1] &&
                                             zzdim_31404 == in2->shape[2])) &&
            ((xdim_31402 == in3->shape[0] && (ydim_31403 == in3->shape[1] &&
                                              zzdim_31404 == in3->shape[2])) &&
             ((xdim_31402 == in4->shape[0] && (ydim_31403 == in4->shape[1] &&
                                               zzdim_31404 == in4->shape[2])) &&
              ((xdim_31402 == in5->shape[0] && (ydim_31403 == in5->shape[1] &&
                                                zzdim_31404 ==
                                                in5->shape[2])) &&
               ((xdim_31402 == in6->shape[0] && (ydim_31403 == in6->shape[1] &&
                                                 zzdim_31404 ==
                                                 in6->shape[2])) &&
                ((xdim_31402 == in7->shape[0] && (ydim_31403 == in7->shape[1] &&
                                                  zzdim_31404 ==
                                                  in7->shape[2])) &&
                 ((xdim_31402 == in8->shape[0] && (ydim_31403 ==
                                                   in8->shape[1] &&
                                                   zzdim_31404 ==
                                                   in8->shape[2])) &&
                  ((xdim_31402 == in9->shape[0] && (ydim_31403 ==
                                                    in9->shape[1] &&
                                                    zzdim_31404 ==
                                                    in9->shape[2])) &&
                   ((xdim_31402 == in10->shape[0] && (ydim_31403 ==
                                                      in10->shape[1] &&
                                                      zzdim_31404 ==
                                                      in10->shape[2])) &&
                    ((xdim_31402 == in11->shape[0] && (ydim_31403 ==
                                                       in11->shape[1] &&
                                                       zzdim_31404 ==
                                                       in11->shape[2])) &&
                     (xdim_31402 == in12->shape[0] && (xdim_31402 ==
                                                       in13->shape[0] &&
                                                       (ydim_31403 ==
                                                        in14->shape[0] &&
                                                        (ydim_31403 ==
                                                         in15->shape[0] &&
                                                         (zzdim_31404 ==
                                                          in16->shape[0] &&
                                                          (zzdim_31404 ==
                                                           in17->shape[0] &&
                                                           (ydim_31403 ==
                                                            in18->shape[0] &&
                                                            (ydim_31403 ==
                                                             in19->shape[0] &&
                                                             ((xdim_31402 ==
                                                               in20->shape[0] &&
                                                               ydim_31403 ==
                                                               in20->shape[1]) &&
                                                              ((xdim_31402 ==
                                                                in21->shape[0] &&
                                                                (ydim_31403 ==
                                                                 in21->shape[1] &&
                                                                 zzdim_31404 ==
                                                                 in21->shape[2])) &&
                                                               ((xdim_31402 ==
                                                                 in22->shape[0] &&
                                                                 (ydim_31403 ==
                                                                  in22->shape[1] &&
                                                                  zzdim_31404 ==
                                                                  in22->shape[2])) &&
                                                                ((xdim_31402 ==
                                                                  in23->shape[0] &&
                                                                  (ydim_31403 ==
                                                                   in23->shape[1] &&
                                                                   zzdim_31404 ==
                                                                   in23->shape[2])) &&
                                                                 (xdim_31402 ==
                                                                  in24->shape[0] &&
                                                                  ydim_31403 ==
                                                                  in24->shape[1])))))))))))))))))))))))))) {
        ret = 1;
        if (!ctx->error)
            ctx->error =
                msgprintf("Error: entry point arguments have invalid sizes.\n");
    }
    if (ret == 0) {
        ret = futrts_entry_main(ctx, &mem_out_39725, &mem_out_39726,
                                &mem_out_39727, &mem_out_39728, &mem_out_39729,
                                &mem_out_39730, &mem_out_39731,
                                tketau_mem_37734, tketaup1_mem_37735,
                                tketaum1_mem_37736, dtketau_mem_37737,
                                dtketaup1_mem_37738, dtketaum1_mem_37739,
                                utau_mem_37740, vtau_mem_37741, wtau_mem_37742,
                                maskU_mem_37743, maskV_mem_37744,
                                maskW_mem_37745, dxt_mem_37746, dxu_mem_37747,
                                dyt_mem_37748, dyu_mem_37749, dzzt_mem_37750,
                                dzzw_mem_37751, cost_mem_37752, cosu_mem_37753,
                                kbot_mem_37754, kappaM_mem_37755, mxl_mem_37756,
                                forc_mem_37757, forc_tke_surface_mem_37758,
                                xdim_31402, ydim_31403, zzdim_31404);
        if (ret == 0) {
            assert((*out0 =
                    (struct futhark_f32_3d *) malloc(sizeof(struct futhark_f32_3d))) !=
                NULL);
            (*out0)->mem = mem_out_39725;
            (*out0)->shape[0] = xdim_31402;
            (*out0)->shape[1] = ydim_31403;
            (*out0)->shape[2] = zzdim_31404;
            assert((*out1 =
                    (struct futhark_f32_3d *) malloc(sizeof(struct futhark_f32_3d))) !=
                NULL);
            (*out1)->mem = mem_out_39726;
            (*out1)->shape[0] = xdim_31402;
            (*out1)->shape[1] = ydim_31403;
            (*out1)->shape[2] = zzdim_31404;
            assert((*out2 =
                    (struct futhark_f32_3d *) malloc(sizeof(struct futhark_f32_3d))) !=
                NULL);
            (*out2)->mem = mem_out_39727;
            (*out2)->shape[0] = xdim_31402;
            (*out2)->shape[1] = ydim_31403;
            (*out2)->shape[2] = zzdim_31404;
            assert((*out3 =
                    (struct futhark_f32_3d *) malloc(sizeof(struct futhark_f32_3d))) !=
                NULL);
            (*out3)->mem = mem_out_39728;
            (*out3)->shape[0] = xdim_31402;
            (*out3)->shape[1] = ydim_31403;
            (*out3)->shape[2] = zzdim_31404;
            assert((*out4 =
                    (struct futhark_f32_3d *) malloc(sizeof(struct futhark_f32_3d))) !=
                NULL);
            (*out4)->mem = mem_out_39729;
            (*out4)->shape[0] = xdim_31402;
            (*out4)->shape[1] = ydim_31403;
            (*out4)->shape[2] = zzdim_31404;
            assert((*out5 =
                    (struct futhark_f32_3d *) malloc(sizeof(struct futhark_f32_3d))) !=
                NULL);
            (*out5)->mem = mem_out_39730;
            (*out5)->shape[0] = xdim_31402;
            (*out5)->shape[1] = ydim_31403;
            (*out5)->shape[2] = zzdim_31404;
            assert((*out6 =
                    (struct futhark_f32_2d *) malloc(sizeof(struct futhark_f32_2d))) !=
                NULL);
            (*out6)->mem = mem_out_39731;
            (*out6)->shape[0] = xdim_31402;
            (*out6)->shape[1] = ydim_31403;
        }
    }
    lock_unlock(&ctx->lock);
    return ret;
}
  
