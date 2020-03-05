// Start of opengl.h.

#define OPENGL_SUCCEED_FATAL(e) opengl_succeed_fatal(e, #e, __FILE__, __LINE__)
#define OPENGL_SUCCEED_NONFATAL(e) opengl_succeed_nonfatal(e, #e, __FILE__, __LINE__)
// Take care not to override an existing error.
#define OPENGL_SUCCEED_OR_RETURN(e) {             \
    char *error = OPENGL_SUCCEED_NONFATAL(e);     \
    if (error) {                                  \
      if (!ctx->error) {                          \
        ctx->error = error;                       \
        return bad;                               \
      } else {                                    \
        free(error);                              \
      }                                           \
    }                                             \
  }

typedef GLXContext (*glXCreateContextAttribsARBProc)(Display*,
                                                    GLXFBConfig,
                                                    GLXContext,
                                                    Bool,
                                                    const int*);
typedef Bool (*glXMakeContextCurrentARBProc)(Display*,
                                             GLXDrawable,
                                             GLXDrawable,
                                             GLXContext);

static glXCreateContextAttribsARBProc glXCreateContextAttribsARB = 0;
static glXMakeContextCurrentARBProc glXMakeContextCurrentARB     = 0;

struct opengl_config {
  int debugging;
  int profiling;
  int logging;

  const char* dump_program_to;
  const char* load_program_from;
  const char* dump_binary_to;
  const char* load_binary_from;

  size_t default_group_size;
  size_t default_num_groups;
  size_t default_tile_size;
  size_t default_threshold;

  int default_group_size_changed;
  int default_tile_size_changed;

  int num_sizes;
  const char **size_names;
  const char **size_vars;
  size_t      *size_values;
  const char **size_classes;
};

static void opengl_config_init(struct opengl_config *cfg,
                               int         num_sizes,
                               const char *size_names[],
                               const char *size_vars[],
                               size_t     *size_values,
                               const char *size_classes[]) {
  cfg->debugging = 0;
  cfg->logging   = 0;
  cfg->profiling = 0;

  cfg->dump_program_to   = NULL;
  cfg->load_program_from = NULL;
  cfg->dump_binary_to    = NULL;
  cfg->load_binary_from  = NULL;

  cfg->default_group_size = 256;
  cfg->default_num_groups = 128;
  cfg->default_tile_size  = 32;
  cfg->default_threshold  = 32*1024;

  cfg->default_group_size_changed = 0;
  cfg->default_tile_size_changed  = 0;

  cfg->num_sizes    = num_sizes;
  cfg->size_names   = size_names;
  cfg->size_vars    = size_vars;
  cfg->size_values  = size_values;
  cfg->size_classes = size_classes;
}

struct opengl_context {

  GLuint SSBOs;

  struct opengl_config cfg;

  struct free_list free_list;

  size_t max_group_size;
  size_t max_num_groups;
  size_t max_tile_size;
  size_t max_threshold;
  size_t max_shared_memory;

  size_t lockstep_width;

};

static char *strclone(const char *str) {
  size_t size = strlen(str) + 1;
  char *copy = (char*) malloc(size);
  if (copy == NULL) {
    return NULL;
  }

  memcpy(copy, str, size);
  return copy;
}

// Read a file into a NUL-terminated string; returns NULL on error.
static char* slurp_file(const char *filename, size_t *size) {
  char *s;
  FILE *f = fopen(filename, "rb"); // To avoid Windows messing with linebreaks.
  if (f == NULL) return NULL;
  fseek(f, 0, SEEK_END);
  size_t src_size = ftell(f);
  fseek(f, 0, SEEK_SET);
  s = (char*) malloc(src_size + 1);
  if (fread(s, 1, src_size, f) != src_size) {
    free(s);
    s = NULL;
  } else {
    s[src_size] = '\0';
  }
  fclose(f);

  if (size) {
    *size = src_size;
  }

  return s;
}

static const char* opengl_error_string(GLenum err) {
  switch(err) {
    case GL_NO_ERROR:                      return "GL_NO_ERROR";
    case GL_INVALID_ENUM:                  return "INVALID_ENUM";
    case GL_INVALID_VALUE:                 return "INVALID_VALUE";
    case GL_INVALID_OPERATION:             return "INVALID_OPERATION";
    case GL_INVALID_FRAMEBUFFER_OPERATION: return "INVALID_FRAMEBUFFER_OPERATION";
    case GL_OUT_OF_MEMORY:                 return "OUT_OF_MEMORY";
    case GL_STACK_UNDERFLOW:               return "GL_STACK_UNDERFLOW";
    case GL_STACK_OVERFLOW:                return "GL_STACK_OVERFLOW";
  }
}

static void opengl_succeed_fatal(GLenum ret,
                                 const char *call,
                                 const char *file,
                                 int line) {
  if (ret != GL_NO_ERROR) {
    panic(-1, "%s:%d: OpenGL call\n  %s\nfailed with error code %d (%s)\n",
          file, line, call, ret, opengl_error_string(ret));
  }
}

static char* opengl_succeed_nonfatal(unsigned int ret,
                                     const char *call,
                                     const char *file,
                                     int line) {
  if (ret != GL_NO_ERROR) {
    return msgprintf("%s:%d: OpenGL call\n  %s\nfailed with error code %d (%s)\n",
                     file, line, call, ret, opengl_error_string(ret));
  } else {
    return NULL;
  }
}

static void setup_size_opengl(struct opengl_context *ctx) {

   int max_shared_memory;
   int max_group_size;
   int max_num_groups;

   glGetIntegerv(GL_MAX_COMPUTE_SHARED_MEMORY_SIZE,
                 &max_shared_memory);
   glGetIntegerv(GL_MAX_COMPUTE_WORK_GROUP_SIZE,
                 &max_group_size);
   glGetIntegerv(GL_MAX_COMPUTE_WORK_GROUP_INVOCATIONS,
                 &max_num_groups);

   ctx->max_threshold     = 0;
   ctx->max_shared_memory = max_shared_memory;
   ctx->max_group_size    = max_group_size;
   ctx->max_num_groups    = max_num_groups;

  // Go through all the sizes, clamp them to the valid range,
  // or set them to the default.
  for (int i = 0; i < ctx->cfg.num_sizes; i++) {
    const char *size_class = ctx->cfg.size_classes[i];
    size_t *size_value = &ctx->cfg.size_values[i];
    const char* size_name = ctx->cfg.size_names[i];
    size_t max_value, default_value;
    if (strstr(size_class, "group_size") == size_class) {
      max_value = max_group_size;
      default_value = ctx->cfg.default_group_size;
    } else if (strstr(size_class, "num_groups") == size_class) {
      max_value = max_group_size; // Futhark assumes this constraint.
      default_value = ctx->cfg.default_num_groups;
    } else if (strstr(size_class, "tile_size") == size_class) {
      max_value = sqrt(max_group_size);
      default_value = ctx->cfg.default_tile_size;
    } else if (strstr(size_class, "threshold") == size_class) {
      max_value = 0; // No limit.
      default_value = ctx->cfg.default_threshold;
    } else {
      // Bespoke sizes have no limit or default.
      max_value = 0;
    }
    if (*size_value == 0) {
      *size_value = default_value;
    } else if (max_value > 0 && *size_value > max_value) {
      fprintf(stderr, "Note: Device limits %s to %d (down from %d)\n",
              size_name, (int)max_value, (int)*size_value);
      *size_value = max_value;
    }
  }
}

static void setup_opengl(struct opengl_context *ctx,
                         const char *srcs[],
                         const char *extra_build_opts[]) {

  static int visual_attribs[] = { None };

  int context_attribs[] = {
    GLX_CONTEXT_MAJOR_VERSION_ARB, 4,
    GLX_CONTEXT_MINOR_VERSION_ARB, 3,
    None
  };

  Display*     dpy     = XOpenDisplay(0);
  int          fbcount = 0;
  GLXFBConfig* fbc     = NULL;
  GLXContext   glctx;
  GLXPbuffer   pbuf;

  /* open display */
  if ( !(dpy = XOpenDisplay(0)) ) {
    fprintf(stderr, "Failed to open display\n");
    exit(1);
  }

  /* get framebuffer configs, any is usable (might want to add proper attribs) */
  if ( !(fbc = glXChooseFBConfig(dpy, DefaultScreen(dpy), visual_attribs,
                                 &fbcount) ) ) {
    fprintf(stderr, "Failed to get FBConfig\n");
    exit(1);
  }

  /* get the required extensions */
  glXCreateContextAttribsARB = (glXCreateContextAttribsARBProc)glXGetProcAddressARB(
                                (const GLubyte *) "glXCreateContextAttribsARB");
  glXMakeContextCurrentARB = (glXMakeContextCurrentARBProc)glXGetProcAddressARB(
                                (const GLubyte *) "glXMakeContextCurrent");
  if ( !(glXCreateContextAttribsARB && glXMakeContextCurrentARB) ) {
    fprintf(stderr, "missing support for GLX_ARB_create_context\n");
    XFree(fbc);
    exit(1);
  }

  /* create a context using glXCreateContextAttribsARB */
  if ( !( glctx = glXCreateContextAttribsARB(dpy, fbc[0], 0, True,
                                             context_attribs)) ) {
    fprintf(stderr, "Failed to create opengl context\n");
    XFree(fbc);
    exit(1);
  }

  /* create temporary pbuffer */
  int pbuffer_attribs[] = {
    GLX_PBUFFER_WIDTH,  800,
    GLX_PBUFFER_HEIGHT, 600,
    None
  };
  pbuf = glXCreatePbuffer(dpy, fbc[0], pbuffer_attribs);

  XFree(fbc);
  XSync(dpy, False);

  /* try to make it the current context */
  if ( !glXMakeContextCurrent(dpy, pbuf, pbuf, glctx) ) {
    /* some drivers does not support context without default framebuffer,
    *  so fallback on using the default window.
    */
    if ( !glXMakeContextCurrent(dpy, DefaultRootWindow(dpy),
                                DefaultRootWindow(dpy), glctx) ) {
      fprintf(stderr, "failed to make current\n");
      exit(1);
    }
  }

  if ( !gladLoadGLLoader((GLADloadproc)glXGetProcAddress) ) {
      fprintf(stderr, "glad: Failed to initialize OpenGL context\n");
      exit(1);
    }

  setup_size_opengl(ctx);

}

static GLenum opengl_alloc(struct opengl_context *ctx,
                           size_t      min_size,
                           const char *tag,
                           GLuint     *mem_out) {
  GLenum error;
  size_t size;

  if (min_size < sizeof(int)) {
    min_size = sizeof(int);
  }

  if (free_list_find(&ctx->free_list, tag, &size, mem_out) == 0) {
    // Successfully found a free block.  Is it big enough?
    //
    // FIXME: See `opencl_alloc(...)` in `opencl.h`
    if (size >= min_size) {
      return GL_NO_ERROR;
    } else {
      glDeleteBuffers(size, mem_out);
      error = glGetError();
      if (error != GL_NO_ERROR) {
        return error;
      }
    }
  }

  GLuint ssbo;

  glGenBuffers(1, &ssbo);

  glBindBuffer(GL_SHADER_STORAGE_BUFFER, ssbo);

  error = glGetError();
  if (error != GL_NO_ERROR) {
    return error;
  }

  glBufferData(GL_SHADER_STORAGE_BUFFER, size, NULL,
               GL_DYNAMIC_DRAW);

  glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 0, ssbo);

  glBindBuffer(GL_SHADER_STORAGE_BUFFER, ssbo);

  error = glGetError();

  *mem_out = ssbo;

  return error;
}

static GLenum opengl_free(struct opengl_context *ctx,
                          GLuint      mem,
                          const char *tag) {
  size_t size;
  GLuint existing_mem;
  GLenum error;

  // If there is already a block with this tag, then remove it.
  if (free_list_find(&ctx->free_list, tag, &size, &existing_mem) == 0) {
    glDeleteBuffers(1, &existing_mem);
    error = glGetError();
    if (error != GL_NO_ERROR) {
      return error;
    }
  }

  //TODO: get object info
  error = glGetError();

  if (error == GL_NO_ERROR) {
    free_list_insert(&ctx->free_list, size, mem, tag);
  }

  return error;
}

static GLenum opengl_free_all(struct opengl_context *ctx) {
  GLuint mem;
  GLenum error;
  free_list_pack(&ctx->free_list);
    while (free_list_first(&ctx->free_list, &mem) == 0) {
      glDeleteBuffers(1, &mem);
      error = glGetError();
      if (error != GL_NO_ERROR) {
        return error;
      }
    }

  return GL_NO_ERROR;
}

// End of opengl.h.
