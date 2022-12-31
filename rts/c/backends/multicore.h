// Start of backends/multicore.h

struct futhark_context_config {
  int in_use;
  int debugging;
  int profiling;
  int logging;
  const char *cache_fname;
  int num_tuning_params;
  int64_t *tuning_params;
  const char** tuning_param_names;
  const char** tuning_param_vars;
  const char** tuning_param_classes;
  // Uniform fields above.

  int num_threads;
};

static void backend_context_config_setup(struct futhark_context_config* cfg) {
  cfg->num_threads = 0;
}

static void backend_context_config_teardown(struct futhark_context_config* cfg) {
  (void)cfg;
}

void futhark_context_config_set_num_threads(struct futhark_context_config *cfg, int n) {
  cfg->num_threads = n;
}

// End of backends/multicore.h
