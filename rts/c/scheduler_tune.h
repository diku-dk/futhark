/* The self-tuning program to estimate $\kappa$ */

struct futhark_mc_segred_stage_1_struct {
  struct futhark_context *ctx;
  int32_t *free_tuning_res;
  int32_t *array;
};

int futhark_mc_tuning_segred_stage_1(void *args, int64_t start, int64_t end,
                                     int flat_tid_22, int tid)
{
    int err = 0;
    struct futhark_mc_segred_stage_1_struct *futhark_mc_segred_stage_1_struct = (struct futhark_mc_segred_stage_1_struct *) args;
    struct futhark_context *ctx = futhark_mc_segred_stage_1_struct->ctx;
    int32_t *array = futhark_mc_segred_stage_1_struct->array;
    int32_t *tuning_res = futhark_mc_segred_stage_1_struct->free_tuning_res;

    int32_t sum = 0;
    for (int i = start; i < end; i++) {
      int32_t y = array[i];
      sum = add32(sum, y);
    }
    *tuning_res = sum;
    return err;
}

int futhark_segred_tuning_program(struct futhark_context *ctx)
{
  int err = 0;

  int64_t iterations = 100000000;
  int64_t tuning_time = 0;
  int64_t tuning_iter = 0;

  int32_t *array = malloc(sizeof(int32_t) * iterations);
  for (int64_t i = 0; i < iterations; i++) {
    array[i] = fast_rand();
  }

  int64_t start_tuning = get_wall_time_ns();
  /* **************************** */
  /* Run sequential reduce first' */
  /* **************************** */
  int64_t tuning_sequentiual_start = get_wall_time_ns();
  struct futhark_mc_segred_stage_1_struct futhark_mc_segred_stage_1_struct;
  int32_t tuning_res;
  futhark_mc_segred_stage_1_struct.ctx = ctx;
  futhark_mc_segred_stage_1_struct.free_tuning_res = &tuning_res;
  futhark_mc_segred_stage_1_struct.array = array;

  err = futhark_mc_tuning_segred_stage_1(&futhark_mc_segred_stage_1_struct, 0, iterations, 0, 0);
  int64_t tuning_sequentiual_end = get_wall_time_ns();
  int64_t sequential_elapsed = tuning_sequentiual_end - tuning_sequentiual_start;

  double C = (double)sequential_elapsed / (double)iterations;
  fprintf(stderr, " Time for sequential run is %lld - Found C %f\n", sequential_elapsed, C);

  /* Now run tuning process */
  int num_threads = ctx->scheduler.num_threads;
  ctx->scheduler.num_threads = 1;
  ctx->scheduler.workers = malloc(sizeof(struct worker));
  worker_local = &ctx->scheduler.workers[0];
  worker_local->tid = 0;
  CHECK_ERR(subtask_queue_init(&ctx->scheduler.workers[0].q, 1024), "failed to init queue for worker %d\n", 0);

  // Start tuning for kappa
  double kappa_tune = 1000;
  double ratio;
  int64_t time_elapsed;
  while(1) {
    int64_t min_iter_pr_subtask = (int64_t) (kappa_tune / C) == 0 ? 1 : (kappa_tune / C);
    int nsubtasks = iterations / min_iter_pr_subtask;
    struct scheduler_info info;
    info.iter_pr_subtask = min_iter_pr_subtask;

    info.nsubtasks = iterations / min_iter_pr_subtask;
    info.remainder = iterations % min_iter_pr_subtask;

    info.task_time = &tuning_time;
    info.task_iter = &tuning_iter;

    info.sched = STATIC;

    struct scheduler_parloop futhark_segred_tuning_scheduler_parloop;
    futhark_segred_tuning_scheduler_parloop.name = "futhark_mc_tuning_segred_stage_1";
    futhark_segred_tuning_scheduler_parloop.fn = futhark_mc_tuning_segred_stage_1;
    futhark_segred_tuning_scheduler_parloop.args = &futhark_mc_segred_stage_1_struct;
    futhark_segred_tuning_scheduler_parloop.iterations = iterations;
    futhark_segred_tuning_scheduler_parloop.info = info;

    int64_t tuning_chunked_start = get_wall_time_ns();
    int futhark_segred_tuning_program_err =
      scheduler_execute_task(&ctx->scheduler,
                             &futhark_segred_tuning_scheduler_parloop);
    assert(futhark_segred_tuning_program_err == 0);
    int64_t tuning_chunked_end = get_wall_time_ns();
    time_elapsed =  tuning_chunked_end - tuning_chunked_start;

    ratio = (double)time_elapsed / (double)sequential_elapsed;
    if (ratio < 1.055) {
      break;
    }
    kappa_tune += 100;
    fprintf(stderr, "nsubtask %d - kappa %f - ratio %f\n", nsubtasks, kappa_tune, ratio);
  }

  int64_t end_tuning = get_wall_time_ns();
  fprintf(stderr, "tuning took %lld ns and found kappa %f - time %lld - ratio %f\n",
          end_tuning- start_tuning, kappa_tune, time_elapsed,  ratio);
  kappa = kappa_tune;

  CHECK_ERR(subtask_queue_destroy(&ctx->scheduler.workers[0].q), "failed to destroy queue");
  free(array);
  free(ctx->scheduler.workers);
  ctx->scheduler.num_threads = num_threads;
  return err;
}
