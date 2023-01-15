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

int futhark_context_config_set_tuning_param(struct futhark_context_config* cfg, const char *param_name, size_t param_value) {
  (void)cfg; (void)param_name; (void)param_value;
  return 1;
}

struct futhark_context {
  struct futhark_context_config* cfg;
  int detail_memory;
  int debugging;
  int profiling;
  int profiling_paused;
  int logging;
  lock_t lock;
  char *error;
  lock_t error_lock;
  FILE *log;
  struct constants *constants;
  struct free_list free_list;
  int64_t peak_mem_usage_default;
  int64_t cur_mem_usage_default;
  struct program* program;
  // Uniform fields above.

  struct scheduler scheduler;
  int total_runs;
  long int total_runtime;
  int64_t tuning_timing;
  int64_t tuning_iter;
};

int backend_context_setup(struct futhark_context* ctx) {
  // Initialize rand()
  fast_srand(time(0));

  int tune_kappa = 0;
  double kappa = 5.1f * 1000;

  if (tune_kappa) {
    if (determine_kappa(&kappa) != 0) {
      ctx->error = strdup("Failed to determine kappa.");
      return 1;
    }
  }

  if (scheduler_init(&ctx->scheduler,
                     ctx->cfg->num_threads > 0 ?
                     ctx->cfg->num_threads : num_processors(),
                     kappa) != 0) {
    ctx->error = strdup("Failed to initialise scheduler.");
    return 1;
  }

  return 0;
}

void backend_context_teardown(struct futhark_context* ctx) {
  (void)scheduler_destroy(&ctx->scheduler);
}

int futhark_context_sync(struct futhark_context* ctx) {
  (void)ctx;
  return 0;
}

// End of backends/multicore.h
