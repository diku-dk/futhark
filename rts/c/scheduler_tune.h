
struct futhark_mc_segred_stage_1_struct {
    struct futhark_context *ctx;
    char *free_reduce_stage_1_tid_accum_arr;
};

int futhark_mc_tuning_segred_stage_1(void *args, int64_t start, int64_t end,
                                     int flat_tid_22, int tid, int64_t *time)
{
    int64_t futhark_mc_segred_stage_1_start = get_wall_time();
    int err = 0;
    struct futhark_mc_segred_stage_1_struct *futhark_mc_segred_stage_1_struct = (struct futhark_mc_segred_stage_1_struct *) args;
    struct futhark_context *ctx = futhark_mc_segred_stage_1_struct->ctx;

    struct memblock reduce_stage_1_tid_accum_arr = {.desc =
                                                    "reduce_stage_1_tid_accum_arr",
                                                    .mem =
                                                    futhark_mc_segred_stage_1_struct->free_reduce_stage_1_tid_accum_arr,
                                                    .size =0, .references =
                                                    NULL};
    for (int i = start; i < end; i++)
    {
      int32_t gtid_23 = i;

      int32_t x = ((int32_t *) reduce_stage_1_tid_accum_arr.mem)[tid];
      int32_t y = gtid_23;

      int32_t res = 2 + add32(x, y) / 2;
      ((int32_t *) reduce_stage_1_tid_accum_arr.mem)[tid] = res;
    }

    int64_t futhark_mc_segred_stage_1_end = get_wall_time();
    int64_t elapsed = futhark_mc_segred_stage_1_end - futhark_mc_segred_stage_1_start;

    __atomic_fetch_add(time, elapsed, __ATOMIC_RELAXED);

    return err;
}

int futhark_segred_tuning_program(struct futhark_context *ctx)
{
  int err = 0;

  int64_t iterations = 100000000;
  ctx->tuning_timing = 0;
  ctx->tuning_iter = iterations;


  // Run sequential ''reduce'' first'
  int64_t tuning_sequentiual_start = get_wall_time();
  char reduce_stage_1_tid_accum_arr[sizeof(int32_t) * ctx->scheduler.num_threads];
  memset(reduce_stage_1_tid_accum_arr, 0, sizeof(int32_t) * ctx->scheduler.num_threads);

  struct futhark_mc_segred_stage_1_struct futhark_mc_segred_stage_1_struct;

  futhark_mc_segred_stage_1_struct.ctx = ctx;
  futhark_mc_segred_stage_1_struct.free_reduce_stage_1_tid_accum_arr =
    reduce_stage_1_tid_accum_arr;

  err = futhark_mc_tuning_segred_stage_1(&futhark_mc_segred_stage_1_struct, 0, iterations,
                                         0, 0, &ctx->tuning_timing);
  int64_t tuning_sequentiual_end = get_wall_time();
  int64_t sequential_elapsed = tuning_sequentiual_end - tuning_sequentiual_start;
  fprintf(stderr, "Time Elapsed %lld\n", sequential_elapsed);

  double C = (double)ctx->tuning_timing / (double) ctx->tuning_iter;
  fprintf(stderr, "Found C is %f\n", C);


  // Start tuning for kappa
  worker_local = malloc(sizeof(struct worker));
  worker_local->tid = 0;
  CHECK_ERR(deque_init(&worker_local->q, iterations), "failed to init queue for worker %d\n", 0);

  int tuned = 0;
  double kappa_tune = 18.0;
  /* while (!tuned) { */

  memset(reduce_stage_1_tid_accum_arr, 0, sizeof(int32_t) * ctx->scheduler.num_threads);
  int64_t min_iter_pr_subtask = kappa_tune / C;
  int nsubtasks = iterations / min_iter_pr_subtask;
  struct scheduler_info info;
  info.iter_pr_subtask = min_iter_pr_subtask;
  info.nsubtasks = iterations / min_iter_pr_subtask;
  info.remainder = iterations % min_iter_pr_subtask;

  fprintf(stderr, "min iter %lld\n", info.iter_pr_subtask);
  fprintf(stderr, "num subtask %d\n", info.nsubtasks);
  info.total_time = &ctx->tuning_timing;
  info.total_iter = &ctx->tuning_iter;
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
  int64_t time_elapsed =  tuning_chunked_end- tuning_chunked_start;

  fprintf(stderr, "kappa %f\n", kappa_tune);
  fprintf(stderr, "time elapsed %lld\n", time_elapsed);
  fprintf(stderr, "ratio %f\n", (double)time_elapsed / (double)sequential_elapsed);
  /* } */

  kappa = kappa_tune;
  return err;
}
