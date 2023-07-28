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
static char *strclone(const char *str);

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

struct cost_centre {
  const char *name;
  int64_t runs;
  int64_t runtime;
};

// Dynamic dictionary for tallying cost centres when aggregating
// profiling information.  Not performance-critical.
struct cost_centres {
  size_t capacity;
  size_t used;
  struct cost_centre* centres;
};

static struct cost_centres *cost_centres_new() {
  struct cost_centres *ccs = malloc(sizeof(struct cost_centres));
  ccs->capacity = 100;
  ccs->used = 0;
  ccs->centres = calloc(ccs->capacity, sizeof(struct cost_centre));
  return ccs;
}

static void cost_centres_free(struct cost_centres* ccs) {
  free(ccs->centres);
  free(ccs);
}

static void cost_centres_init(struct cost_centres* ccs, const char *name) {
  if (ccs->used == ccs->capacity) {
    ccs->capacity *= 2;
    ccs->centres = realloc(ccs->centres, ccs->capacity*sizeof(struct cost_centre));
  }
  ccs->centres[ccs->used].name = name;
  ccs->centres[ccs->used].runs = 0;
  ccs->centres[ccs->used].runtime = 0;
  ccs->used++;
}

static void cost_centres_add(struct cost_centres* ccs, struct cost_centre c) {
  size_t i = 0;
  for (i = 0; i < ccs->used; i++) {
    if (strcmp(c.name, ccs->centres[i].name) == 0) {
      ccs->centres[i].runs += c.runs;
      ccs->centres[i].runtime += c.runtime;
      return;
    }
  }
  if (i == ccs->capacity) {
    ccs->capacity *= 2;
    ccs->centres = realloc(ccs->centres, ccs->capacity*sizeof(struct cost_centre));
  }
  ccs->centres[i] = c;
  ccs->used++;
}

static void cost_centre_report(struct cost_centres* ccs, struct str_builder *b) {
  int64_t total_runs = 0;
  int64_t total_runtime = 0;
  for (size_t i = 0; i < ccs->used; i++) {
    struct cost_centre c = ccs->centres[i];
    str_builder(b,
                "%-40s ran %5d times; avg %8ldus; total: %8ldus\n",
                c.name,
                c.runs, c.runs == 0 ? 0 : c.runtime/c.runs, c.runtime);
    total_runs += c.runs;
    total_runtime += c.runtime;
  }
  str_builder(b,
              "%d operations with cumulative runtime: %6ldus\n",
              total_runs, total_runtime);
}

static char *strclone(const char *str) {
  size_t size = strlen(str) + 1;
  char *copy = (char*) malloc(size);
  if (copy == NULL) {
    return NULL;
  }

  memcpy(copy, str, size);
  return copy;
}

// End of util.h.
