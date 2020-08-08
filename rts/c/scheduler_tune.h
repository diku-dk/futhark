#include <stdint.h>
#include <stdlib.h>

struct futhark_mc_segred_stage_1_struct {
    struct futhark_context *ctx;
    char *free_reduce_stage_1_tid_accum_arr;
};

int futhark_mc_tuning_segred_stage_1(void *args, int start, int end,
                                     int flat_tid_22, int tid)
{
    /* uint64_t futhark_mc_segred_stage_1_start = get_wall_time(); */
    int err = 0;
    struct futhark_mc_segred_stage_1_struct *futhark_mc_segred_stage_1_struct = (struct futhark_mc_segred_stage_1_struct *) args;
    struct futhark_context *ctx = futhark_mc_segred_stage_1_struct->ctx;

    assert(tid < 12);
    struct memblock reduce_stage_1_tid_accum_arr = {.desc =
                                                    "reduce_stage_1_tid_accum_arr",
                                                    .mem =
                                                    futhark_mc_segred_stage_1_struct->free_reduce_stage_1_tid_accum_arr,
                                                    .size =0, .references =
                                                    NULL};
    for (int i = start; i < end; i++)
    {
      int32_t gtid_23 = i;
      int32_t x_18;
      int32_t x_19;

      x_18 = ((int32_t *) reduce_stage_1_tid_accum_arr.mem)[tid];
      x_19 = gtid_23;

      int32_t res_20 = add32(x_18, x_19);
      ((int32_t *) reduce_stage_1_tid_accum_arr.mem)[tid] =
        res_20;
    }

    /* uint64_t futhark_mc_segred_stage_1_end = get_wall_time(); */
    /* uint64_t elapsed = futhark_mc_segred_stage_1_end - futhark_mc_segred_stage_1_start; */

    /* ctx->futhark_tuning_mc_segred_stage_1_runtime[tid] += elapsed; */
    /* ctx->futhark_tuning_mc_segred_stage_1_iter[tid] += end - start; */

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

    /* char reduce_stage_1_tid_accum_arr[sext_i32_i64((int32_t) sizeof(int32_t)) * */
    /*                                   sext_i32_i64(ctx->scheduler.num_threads)]; */
    char * reduce_stage_1_tid_accum_arr = malloc(sext_i32_i64((int32_t) sizeof(int32_t)) *
                                                 sext_i32_i64(ctx->scheduler.num_threads));


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

    uint64_t futhark_segred_tuning_program_start = get_wall_time();

    int futhark_segred_tuning_program_err =
        scheduler_execute_task(&ctx->scheduler,
                               &futhark_segred_tuning_scheduler_subtask);

    assert(futhark_segred_tuning_program_err == 0);
    assert(ctx->tuning_iter == iterations);


    uint64_t futhark_segred_tuning_program_end = get_wall_time();
    uint64_t elapsed = futhark_segred_tuning_program_end - futhark_segred_tuning_program_start;
    double k = (double)(elapsed - ctx->tuning_timing) / iterations;

    kappa = k * num_threads;
    fprintf(stderr, "found kappa is %f\n", kappa);

    return err;
}
