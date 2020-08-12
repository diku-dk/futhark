
struct futhark_mc_segred_stage_1_struct {
    struct futhark_context *ctx;
    char *free_reduce_stage_1_tid_accum_arr;
};

int futhark_mc_tuning_segred_stage_1(void *args, int start, int end,
                                     int flat_tid_22, int tid)
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

      int32_t res = add32(x, y);
      ((int32_t *) reduce_stage_1_tid_accum_arr.mem)[tid] = res;
    }

    int64_t futhark_mc_segred_stage_1_end = get_wall_time();
    int64_t elapsed = futhark_mc_segred_stage_1_end - futhark_mc_segred_stage_1_start;

    ctx->futhark_tuning_mc_segred_stage_1_runtime[tid] += elapsed;
    ctx->futhark_tuning_mc_segred_stage_1_iter[tid] += end - start;

    return err;
}

int futhark_segred_tuning_program(struct futhark_context *ctx)
{
    int err = 0;

    int iterations = 100000;
    ctx->futhark_tuning_mc_segred_stage_1_runtime = calloc(sizeof(int64_t), ctx->scheduler.num_threads);
    ctx->futhark_tuning_mc_segred_stage_1_iter = calloc(sizeof(int64_t), ctx->scheduler.num_threads);
    ctx->tuning_timing = 0;
    ctx->tuning_iter = 0;

    ctx->scheduler.workers = calloc(ctx->scheduler.num_threads, sizeof(struct worker));
    if (ctx->scheduler.workers == NULL) return 1;

    num_workers = ctx->scheduler.num_threads;
    worker_local = &ctx->scheduler.workers[0];
    worker_local->tid = 0;
    worker_local->scheduler = &ctx->scheduler;



    // Initialize queues and threads
    CHECK_ERR(deque_init(&worker_local->q, iterations), "failed to init queue for worker %d\n", 0);
    for (int i = 1; i < ctx->scheduler.num_threads; i++) {
      struct worker *cur_worker = &ctx->scheduler.workers[i];
      cur_worker->tid = i;
      cur_worker->time_spent_working = 0;
      cur_worker->cur_working = 0;
      cur_worker->scheduler = &ctx->scheduler;
      cur_worker->output_usage = 1;
      CHECK_ERR(deque_init(&cur_worker->q, 1024), "failed to init queue for worker %d\n", i);
      CHECK_ERR(pthread_create(&cur_worker->thread, NULL, &scheduler_worker,
                               cur_worker),
                "Failed to create worker %d\n", i);
    }



    struct scheduler_info info;
    info.nsubtasks = iterations;
    info.iter_pr_subtask = 1;
    info.remainder = 0;

    info.total_time = &ctx->tuning_timing;
    info.total_iter = &ctx->tuning_iter;
    info.sched = STATIC;

    int num_threads = ctx->scheduler.num_threads;

    char reduce_stage_1_tid_accum_arr[sizeof(int32_t) * ctx->scheduler.num_threads];
    memset(reduce_stage_1_tid_accum_arr, 0, sizeof(int32_t) * ctx->scheduler.num_threads);

    for (int32_t i = 0; i < num_threads; i++) {
        ((int32_t *) reduce_stage_1_tid_accum_arr)[i] = 0;
    }

    struct futhark_mc_segred_stage_1_struct futhark_mc_segred_stage_1_struct;

    futhark_mc_segred_stage_1_struct.ctx = ctx;
    futhark_mc_segred_stage_1_struct.free_reduce_stage_1_tid_accum_arr =
        reduce_stage_1_tid_accum_arr;

    struct scheduler_subtask futhark_segred_tuning_scheduler_subtask;

    futhark_segred_tuning_scheduler_subtask.name = "futhark_mc_tuning_segred_stage_1";
    futhark_segred_tuning_scheduler_subtask.fn = futhark_mc_tuning_segred_stage_1;
    futhark_segred_tuning_scheduler_subtask.args = &futhark_mc_segred_stage_1_struct;
    futhark_segred_tuning_scheduler_subtask.iterations = iterations;
    futhark_segred_tuning_scheduler_subtask.info = info;

    int64_t futhark_segred_tuning_program_start = get_wall_time();

    int futhark_segred_tuning_program_err =
        scheduler_execute_task(&ctx->scheduler,
                               &futhark_segred_tuning_scheduler_subtask);

    assert(futhark_segred_tuning_program_err == 0);
    assert(ctx->tuning_iter == iterations);

    int sum = 0;
    for (int i = 0; i < num_threads; i++) {
        sum += ctx->futhark_tuning_mc_segred_stage_1_runtime[i];
    }

    int64_t futhark_segred_tuning_program_end = get_wall_time();
    int64_t elapsed = futhark_segred_tuning_program_end - futhark_segred_tuning_program_start;

    kappa = (double)(elapsed - sum) / iterations;
    fprintf(stderr, "found kappa is %f\n", kappa);

    // Teardown again
    for (int i = 1; i < ctx->scheduler.num_threads; i++)
    {
      struct worker *cur_worker = &ctx->scheduler.workers[i];
      cur_worker->dead = 1;
      CHECK_ERR(pthread_join(ctx->scheduler.workers[i].thread, NULL), "pthread_join");
    }

    deque_destroy(&worker_local->q);
    for (int i = 1; i < ctx->scheduler.num_threads; i++) {
      struct worker *cur_worker = &ctx->scheduler.workers[i];
      deque_destroy(&cur_worker->q);
    }

    num_workers = num_threads;
    free(ctx->scheduler.workers);

    return err;
}
