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

typedef GLXContext (*glXCreateContextAttribsARBProc)(Display*, GLXFBConfig, GLXContext, Bool, const int*);
typedef Bool (*glXMakeContextCurrentARBProc)(Display*, GLXDrawable, GLXDrawable, GLXContext);
static glXCreateContextAttribsARBProc glXCreateContextAttribsARB = 0;
static glXMakeContextCurrentARBProc glXMakeContextCurrentARB = 0;

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
 // TODO
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
  if ( !(fbc = glXChooseFBConfig(dpy, DefaultScreen(dpy), visual_attribs, &fbcount) ) ) {
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
  if ( !( glctx = glXCreateContextAttribsARB(dpy, fbc[0], 0, True, context_attribs)) ) {
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
    /* some drivers does not support context without default framebuffer, so fallback on
    *  using the default window.
    */
    if ( !glXMakeContextCurrent(dpy, DefaultRootWindow(dpy), DefaultRootWindow(dpy), glctx) ) {
      fprintf(stderr, "failed to make current\n");
      exit(1);
    }
  }

  if ( !gladLoadGLLoader((GLADloadproc)glXGetProcAddress) ) {
      fprintf(stderr, "glad: Failed to initialize OpenGL context\n");
      exit(1);
    }
}

static int opengl_alloc(struct opengl_context *ctx,
                        size_t      min_size,
                        const char *tag,
                        GLenum      *mem_out) {
  return 0;
}

static int opengl_free(struct opengl_context *ctx,
                       GLenum      mem, 
                       const char *tag) {
  return 0;
}

// End of opengl.h.
