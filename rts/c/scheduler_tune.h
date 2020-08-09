
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

    struct scheduler_info info;
    int iterations = 10000;
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


    int64_t futhark_segred_tuning_program_end = get_wall_time();
    int64_t elapsed = futhark_segred_tuning_program_end - futhark_segred_tuning_program_start;

    kappa = (double)(elapsed - ctx->tuning_timing) / iterations;

    fprintf(stderr, "found kappa is %f\n", kappa);

    return err;
}
