
struct futhark_mc_segred_stage_1_struct {
    struct futhark_context *ctx;
    char *free_reduce_stage_1_tid_accum_arr;
};

int futhark_mc_tuning_segred_stage_1(void *args, int64_t start, int64_t end,
                                     int flat_tid_22, int tid)
{
    int err = 0;
    struct futhark_mc_segred_stage_1_struct *futhark_mc_segred_stage_1_struct = (struct futhark_mc_segred_stage_1_struct *) args;
    struct futhark_context *ctx = futhark_mc_segred_stage_1_struct->ctx;

    struct memblock reduce_stage_1_tid_accum_arr = {.desc =
                                                    "reduce_stage_1_tid_accum_arr",
                                                    .mem =
                                                    futhark_mc_segred_stage_1_struct->free_reduce_stage_1_tid_accum_arr,
                                                    .size =0, .references =
                                                    NULL};
    for (int i = start; i < end; i++) {
      int32_t gtid_23 = i;

      int32_t x = ((int32_t *) reduce_stage_1_tid_accum_arr.mem)[tid];
      int32_t y = gtid_23;

      int32_t res = 2 + add32(x, y) / 2;
      ((int32_t *) reduce_stage_1_tid_accum_arr.mem)[tid] = res;
    }

    return err;
}

int futhark_segred_tuning_program(struct futhark_context *ctx)
{
  int err = 0;

  int64_t iterations = 100000000;
  int64_t tuning_time = 0;
  int64_t tuning_iter = 0;

  int64_t start_tuning = get_wall_time();
  // Run sequential ''reduce'' first'
  int64_t tuning_sequentiual_start = get_wall_time();
  char reduce_stage_1_tid_accum_arr[sizeof(int32_t) * ctx->scheduler.num_threads];
  memset(reduce_stage_1_tid_accum_arr, 0, sizeof(int32_t) * ctx->scheduler.num_threads);

  struct futhark_mc_segred_stage_1_struct futhark_mc_segred_stage_1_struct;

  futhark_mc_segred_stage_1_struct.ctx = ctx;
  futhark_mc_segred_stage_1_struct.free_reduce_stage_1_tid_accum_arr =
    reduce_stage_1_tid_accum_arr;

  err = futhark_mc_tuning_segred_stage_1(&futhark_mc_segred_stage_1_struct, 0, iterations,
                                         0, 0);
  int64_t tuning_sequentiual_end = get_wall_time();
  int64_t sequential_elapsed = tuning_sequentiual_end - tuning_sequentiual_start;
  fprintf(stderr, "Time Elapsed %lld\n", sequential_elapsed);

  double C = (double)sequential_elapsed / (double)iterations;
  fprintf(stderr, "Found C is %f\n", C);

  int num_threads = ctx->scheduler.num_threads;
  ctx->scheduler.num_threads = 1;
  ctx->scheduler.workers = malloc(sizeof(struct worker));
  worker_local = &ctx->scheduler.workers[0];
  worker_local->tid = 0;
#ifdef MCCHASELEV
  CHECK_ERR(deque_init(&ctx->scheduler.workers[0].q, 1024), "failed to init queue for worker %d\n", 0);
#elif defined(MCJOBQUEUE)
  CHECK_ERR(subtask_queue_init(&ctx->scheduler.workers[0].q, 1024), "failed to init queue for worker %d\n", 0);
#endif

  // Start tuning for kappa
  double kappa_tune = 0.1;
  double ratio;
  int64_t time_elapsed;
  while(1) {
    memset(reduce_stage_1_tid_accum_arr, 0, sizeof(int32_t) * num_threads);
    int64_t min_iter_pr_subtask = kappa_tune / C;
    int nsubtasks = iterations / min_iter_pr_subtask;
    struct scheduler_info info;
    info.iter_pr_subtask = min_iter_pr_subtask;

    info.nsubtasks = iterations / min_iter_pr_subtask;
    info.remainder = iterations % min_iter_pr_subtask;

    info.total_time = &tuning_time;
    info.total_iter = &tuning_iter;

    info.sched = STATIC;

    struct scheduler_subtask futhark_segred_tuning_scheduler_subtask;
    futhark_segred_tuning_scheduler_subtask.name = "futhark_mc_tuning_segred_stage_1";
    futhark_segred_tuning_scheduler_subtask.fn = futhark_mc_tuning_segred_stage_1;
    futhark_segred_tuning_scheduler_subtask.args = &futhark_mc_segred_stage_1_struct;
    futhark_segred_tuning_scheduler_subtask.iterations = iterations;
    futhark_segred_tuning_scheduler_subtask.info = info;

    int64_t tuning_chunked_start = get_wall_time();
    int futhark_segred_tuning_program_err =
      scheduler_execute_task(&ctx->scheduler,
                             &futhark_segred_tuning_scheduler_subtask);
    assert(futhark_segred_tuning_program_err == 0);
    int64_t tuning_chunked_end = get_wall_time();
    time_elapsed =  tuning_chunked_end - tuning_chunked_start;

    ratio = (double)time_elapsed / (double)sequential_elapsed;
    if (ratio < 1.055) {
      break;
    }
    kappa_tune += 0.1;
  }

  int64_t end_tuning = get_wall_time();
  fprintf(stderr, "tuning took %lld us and found kappa %f - time %lld - ratio %f\n",
          end_tuning- start_tuning, kappa_tune, time_elapsed,  ratio);

  kappa = kappa_tune;
#ifdef MCCHASELV
  CHECK_ERR(deque_destroy(&ctx->scheduler.workers[0].q, 1024), "failed to init queue for worker %d\n", 0);
#elif defined(MCJOBQUEUE)
  subtask_queue_destroy(&ctx->scheduler.workers[0].q);
#endif
  free(ctx->scheduler.workers);
  ctx->scheduler.num_threads = num_threads;
  return err;
}
