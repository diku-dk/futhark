// Start of copy.h

// Cache-oblivious map-transpose function.
#define GEN_MAP_TRANSPOSE(NAME, ELEM_TYPE)                              \
  static void transpose_##NAME                                          \
  (ELEM_TYPE* dst, int64_t dst_row_stride,                              \
   ELEM_TYPE* src, int64_t src_row_stride,                              \
   int64_t rb, int64_t re, int64_t cb, int64_t ce)                      \
  {                                                                     \
    int64_t r = re - rb;                                                \
    int64_t c = ce - cb;                                                \
    if (r <= 64 && c <= 64) {                                           \
      for (int64_t j = cb; j < ce; j++) {                               \
        for (int64_t i = rb; i < re; i++) {                             \
          dst[j * dst_row_stride + i] = src[i * src_row_stride + j];    \
        }                                                               \
      }                                                                 \
    } else if (c <= r) {                                                \
      transpose_##NAME(dst, dst_row_stride, src, src_row_stride,        \
                       rb, rb + r/2, cb, ce);                           \
      transpose_##NAME(dst, dst_row_stride, src, src_row_stride,        \
                       rb + r/2, re, cb, ce);                           \
    } else {                                                            \
      transpose_##NAME(dst, dst_row_stride, src, src_row_stride,        \
                       rb, re, cb, cb + c/2);                           \
      transpose_##NAME(dst, dst_row_stride, src, src_row_stride,        \
                       rb, re, cb + c/2, ce);                           \
    }                                                                   \
  }                                                                     \
                                                                        \
  static void map_transpose_##NAME                                      \
  (ELEM_TYPE* dst, ELEM_TYPE* src, const struct lmad_transpose *tr)     \
  {                                                                     \
    for (int64_t k = 0; k < tr->num_arrays; k++) {                      \
      transpose_##NAME(dst + k * tr->dst_array_stride,                  \
                       tr->dst_row_stride,                              \
                       src + k * tr->src_array_stride,                  \
                       tr->src_row_stride,                              \
                       0, tr->rows, 0, tr->cols);                       \
    }                                                                   \
  }

// Straightforward LMAD copy function.
#define GEN_LMAD_COPY_ELEMENTS(NAME, ELEM_TYPE)                         \
  static void lmad_copy_elements_##NAME(int r,                          \
                                        ELEM_TYPE* dst, int64_t dst_strides[r], \
                                        ELEM_TYPE *src, int64_t src_strides[r], \
                                        int64_t shape[r]) {             \
    if (r == 1) {                                                       \
      for (int64_t i = 0; i < shape[0]; i++) {                          \
        dst[i*dst_strides[0]] = src[i*src_strides[0]];                  \
      }                                                                 \
    } else if (r > 1) {                                                 \
      for (int64_t i = 0; i < shape[0]; i++) {                          \
        lmad_copy_elements_##NAME(r-1,                                  \
                                  dst+i*dst_strides[0], dst_strides+1,  \
                                  src+i*src_strides[0], src_strides+1,  \
                                  shape+1);                             \
      }                                                                 \
    }                                                                   \
  }                                                                     \

// Determine whether a copy is a map(transpose), and if so, describe it in 'tr'.
// We first drop dimensions of size 1, and merge adjacent dimensions that can be
// expressed as a single dimension in both LMADs. If this leaves two or three
// dimensions, then the copy is a map(transpose) if one of the innermost two has
// unit stride in the source, and the other has unit stride in the destination.
// Only those strides are constrained; in particular the arrays need not be
// contiguous.
static bool lmad_map_tr(struct lmad_transpose *tr,
                        int r,
                        const int64_t dst_strides[r],
                        const int64_t src_strides[r],
                        const int64_t shape[r]) {
  int64_t c_shape[3], c_dst_strides[3], c_src_strides[3];
  int c_r = 0;

  for (int i = 0; i < r; i++) {
    if (shape[i] == 1) {
      continue;
    }
    if (c_r > 0 &&
        c_dst_strides[c_r-1] == dst_strides[i] * shape[i] &&
        c_src_strides[c_r-1] == src_strides[i] * shape[i]) {
      c_shape[c_r-1] *= shape[i];
      c_dst_strides[c_r-1] = dst_strides[i];
      c_src_strides[c_r-1] = src_strides[i];
    } else if (c_r == 3) {
      // More than three dimensions, so not a map(transpose).
      return false;
    } else {
      c_shape[c_r] = shape[i];
      c_dst_strides[c_r] = dst_strides[i];
      c_src_strides[c_r] = src_strides[i];
      c_r++;
    }
  }

  // Here c_r is at most 3.
  if (c_r < 2) {
    return false;
  }

  // The innermost two dimensions are transposed, and the outermost one (if
  // any) is the map.
  int a = c_r-2, b = c_r-1;
  if (c_dst_strides[b] == 1 && c_src_strides[a] == 1) {
    tr->rows = c_shape[b];
    tr->cols = c_shape[a];
    tr->dst_row_stride = c_dst_strides[a];
    tr->src_row_stride = c_src_strides[b];
  } else if (c_dst_strides[a] == 1 && c_src_strides[b] == 1) {
    tr->rows = c_shape[a];
    tr->cols = c_shape[b];
    tr->dst_row_stride = c_dst_strides[b];
    tr->src_row_stride = c_src_strides[a];
  } else {
    return false;
  }

  if (c_r == 3) {
    tr->num_arrays = c_shape[0];
    tr->dst_array_stride = c_dst_strides[0];
    tr->src_array_stride = c_src_strides[0];
  } else {
    tr->num_arrays = 1;
    tr->dst_array_stride = 0;
    tr->src_array_stride = 0;
  }
  return true;
}

// Check if the strides correspond to row-major strides of *any*
// permutation of the shape.  This is done by recursive search with
// backtracking.  This is worst-case exponential, but hopefully the
// arrays we encounter do not have that many dimensions.
static bool lmad_contiguous_search(int checked, int64_t expected,
                                   int r,
                                   int64_t strides[r], int64_t shape[r], bool used[r]) {
  for (int i = 0; i < r; i++) {
    for (int j = 0; j < r; j++) {
      if (!used[j] && strides[j] == expected && strides[j] >= 0) {
        used[j] = true;
        if (checked+1 == r ||
            lmad_contiguous_search(checked+1, expected * shape[j], r, strides, shape, used)) {
          return true;
        }
        used[j] = false;
      }
    }
  }
  return false;
}

// Does this LMAD correspond to an array with positive strides and no
// holes?
static bool lmad_contiguous(int r, int64_t strides[r], int64_t shape[r]) {
  bool used[r];
  for (int i = 0; i < r; i++) {
    used[i] = false;
  }
  return lmad_contiguous_search(0, 1, r, strides, shape, used);
}

// Does this copy correspond to something that could be done with a
// memcpy()-like operation?  I.e. do the LMADs actually represent the
// same in-memory layout and are they contiguous?
static bool lmad_memcpyable(int r,
                            int64_t dst_strides[r], int64_t src_strides[r], int64_t shape[r]) {
  if (!lmad_contiguous(r, dst_strides, shape)) {
    return false;
  }
  for (int i = 0; i < r; i++) {
    if (dst_strides[i] != src_strides[i] && shape[i] != 1) {
      return false;
    }
  }
  return true;
}


static void log_copy(struct futhark_context* ctx,
                     const char *kind, const char *provenance,
                     int r,
                     int64_t dst_offset, int64_t dst_strides[r],
                     int64_t src_offset, int64_t src_strides[r],
                     int64_t shape[r]) {
  if (ctx->logging) {
    fprintf(ctx->log, "\n# Copy %s\n", kind);
    if (provenance) { fprintf(ctx->log, "At: %s\n", provenance); }
    fprintf(ctx->log, "Shape: ");
    for (int i = 0; i < r; i++) { fprintf(ctx->log, "[%ld]", (long int)shape[i]); }
    fprintf(ctx->log, "\n");
    fprintf(ctx->log, "Dst offset: %ld\n", (long int)dst_offset);
    fprintf(ctx->log, "Dst strides:");
    for (int i = 0; i < r; i++) { fprintf(ctx->log, " %ld", (long int)dst_strides[i]); }
    fprintf(ctx->log, "\n");
    fprintf(ctx->log, "Src offset: %ld\n", (long int)src_offset);
    fprintf(ctx->log, "Src strides:");
    for (int i = 0; i < r; i++) { fprintf(ctx->log, " %ld", (long int)src_strides[i]); }
    fprintf(ctx->log, "\n");
  }
}

static void log_transpose(struct futhark_context* ctx,
                          const struct lmad_transpose *tr) {
  if (ctx->logging) {
    fprintf(ctx->log, "## Transpose\n");
    fprintf(ctx->log, "Arrays     : %ld\n", (long int)tr->num_arrays);
    fprintf(ctx->log, "X elements : %ld\n", (long int)tr->cols);
    fprintf(ctx->log, "Y elements : %ld\n", (long int)tr->rows);
    fprintf(ctx->log, "Dst strides: %ld %ld\n",
            (long int)tr->dst_array_stride, (long int)tr->dst_row_stride);
    fprintf(ctx->log, "Src strides: %ld %ld\n",
            (long int)tr->src_array_stride, (long int)tr->src_row_stride);
    fprintf(ctx->log, "\n");
  }
}

#define GEN_LMAD_COPY(NAME, ELEM_TYPE)                                  \
  static void lmad_copy_##NAME                                          \
  (struct futhark_context *ctx, int r,                                  \
   ELEM_TYPE* dst, int64_t dst_offset, int64_t dst_strides[r],          \
   ELEM_TYPE *src, int64_t src_offset, int64_t src_strides[r],          \
   int64_t shape[r]) {                                                  \
    log_copy(ctx, "CPU to CPU", NULL, r, dst_offset, dst_strides,       \
             src_offset, src_strides, shape);                           \
    int64_t size = 1;                                                   \
    for (int i = 0; i < r; i++) { size *= shape[i]; }                   \
    if (size == 0) { return; }                                          \
    struct lmad_transpose tr;                                           \
    if (lmad_map_tr(&tr, r, dst_strides, src_strides, shape)) {         \
      log_transpose(ctx, &tr);                                          \
      map_transpose_##NAME(dst+dst_offset, src+src_offset, &tr);        \
    } else if (lmad_memcpyable(r, dst_strides, src_strides, shape)) {   \
      if (ctx->logging) {fprintf(ctx->log, "## Flat copy\n\n");}          \
      memcpy(dst+dst_offset, src+src_offset, size*sizeof(*dst));        \
    } else {                                                            \
      if (ctx->logging) {fprintf(ctx->log, "## General copy\n\n");}       \
      lmad_copy_elements_##NAME                                         \
        (r,                                                             \
         dst+dst_offset, dst_strides,                                   \
         src+src_offset, src_strides, shape);                           \
    }                                                                   \
  }

GEN_MAP_TRANSPOSE(1b, uint8_t)
GEN_MAP_TRANSPOSE(2b, uint16_t)
GEN_MAP_TRANSPOSE(4b, uint32_t)
GEN_MAP_TRANSPOSE(8b, uint64_t)

GEN_LMAD_COPY_ELEMENTS(1b, uint8_t)
GEN_LMAD_COPY_ELEMENTS(2b, uint16_t)
GEN_LMAD_COPY_ELEMENTS(4b, uint32_t)
GEN_LMAD_COPY_ELEMENTS(8b, uint64_t)

GEN_LMAD_COPY(1b, uint8_t)
GEN_LMAD_COPY(2b, uint16_t)
GEN_LMAD_COPY(4b, uint32_t)
GEN_LMAD_COPY(8b, uint64_t)

// End of copy.h
