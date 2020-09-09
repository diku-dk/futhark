// start of scheduler_common.h

#ifndef _SCHEDULER_COMMON_H_
#define _SCHEDULER_COMMON_H_

#include <float.h>

/* Scheduler definitions */
enum scheduling {
  DYNAMIC,
  STATIC
};

struct scheduler_info {
  int64_t iter_pr_subtask;
  int64_t remainder;
  int nsubtasks;
  enum scheduling sched;
  int wake_up_threads;

  int64_t *task_time;
  int64_t *task_iter;
};

struct scheduler {
  struct worker *workers;
  int num_threads;
};

/* A parallel parloop task  */
struct scheduler_parloop {
  const char* name;
  parloop_fn fn;
  void* args;
  int64_t iterations;
  struct scheduler_info info;
};


/* A task for the scheduler to execute */
struct scheduler_task {
  void *args;
  task_fn sequential_fn;
  task_fn canonical_fn;
  int64_t iterations;
  enum scheduling sched;

  // Pointers to timer and iter associated with the task
  int64_t *task_time;
  int64_t *task_iter;

  // For debugging
  const char* name;
};

sigset_t scheduler_sig_set;
static volatile int active_work = 0;

static volatile sig_atomic_t num_workers;
__thread struct worker* worker_local = NULL;

static volatile int scheduler_error = 0;

// kappa time unit in nanoseconds
static double kappa = 5.1f * 1000;

int64_t total_now(int64_t total, int64_t time) {
  return total + (get_wall_time_ns() - time);
}

int random_other_worker(struct scheduler *scheduler, int my_id)
{
  int my_num_workers = __atomic_load_n(&num_workers, __ATOMIC_RELAXED);
  assert(my_num_workers != 1);
  int i = fast_rand() % (my_num_workers - 1);
  if (i >= my_id) {
    i++;
  }
#ifdef MCDEBUG
  assert(i >= 0);
  assert(i < my_num_workers);
  assert(i != my_id);
#endif

  return i;
}


static inline int64_t compute_chunk_size(struct subtask* subtask, struct worker *worker)
{
  struct scheduler* scheduler = worker->scheduler;
  int64_t rem_iter = subtask->end - subtask->start;
  double C = (double)*subtask->task_time / (double)*subtask->task_iter;
  assert(C >= 0.0f);
  int64_t min_iter_pr_subtask = (int64_t)(kappa / (C + DBL_EPSILON));
  min_iter_pr_subtask = min_iter_pr_subtask <= 0 ? 1 : min_iter_pr_subtask;
  return min_iter_pr_subtask;
}

static inline struct subtask* chunk_subtask(struct worker* worker, struct subtask *subtask)
{
  if (subtask->chunkable) {
    // Do we have information from previous runs avaliable
    if (*subtask->task_iter > 0) {
      subtask->chunk_size = compute_chunk_size(subtask, worker);
      assert(subtask->chunk_size > 0);
    }
    int64_t remaining_iter = subtask->end - subtask->start;
    assert(remaining_iter > 0);
    if (remaining_iter > subtask->chunk_size) {
      struct subtask *new_subtask = malloc(sizeof(struct subtask));
      *new_subtask = *subtask;
      __atomic_fetch_add(subtask->counter, 1, __ATOMIC_RELAXED);
      subtask->end = subtask->start + subtask->chunk_size;
      new_subtask->start = subtask->end;
#ifdef MCCHASELEV
      push_back(&worker->q, new_subtask);
#elif defined(MCJOBQUEUE)
      subtask_queue_enqueue(worker, new_subtask);
#endif
    }
  }
  return subtask;
}

static inline int run_subtask(struct worker* worker, struct subtask* subtask)
{
  assert(subtask != NULL);
  assert(worker != NULL);

  worker->total = 0;
  worker->timer = get_wall_time_ns();
  worker->nested++;
  int err = subtask->fn(subtask->args, subtask->start, subtask->end,
                        subtask->chunkable ? worker->tid : subtask->id,
                        worker->tid);
  if (err != 0) {
    __atomic_store_n(&scheduler_error, err, __ATOMIC_RELAXED);
  }
  int64_t time_elapsed = total_now(worker->total, worker->timer);
  worker->time_spent_working += time_elapsed;
  int64_t iter = subtask->end - subtask->start;
  worker->nested--;
  // report measurements
  __atomic_fetch_add(subtask->task_time, time_elapsed, __ATOMIC_RELAXED);
  __atomic_fetch_add(subtask->task_iter, iter, __ATOMIC_RELAXED);
  // We need a fence here, since if the counter is decremented before either
  // of the two above are updated bad things can happen, if they are stack-allocated
  __atomic_thread_fence(__ATOMIC_SEQ_CST);
  __atomic_fetch_sub(subtask->counter, 1, __ATOMIC_RELAXED);
  free(subtask);
  return 0;
}


static inline int compute_max_num_subtasks(int nthreads,
                                           struct scheduler_info info,
                                           long int iterations)
{
  if (*info.task_iter == 0) return nthreads;
  double C = (double)*info.task_time / (double)*info.task_iter;
  int64_t min_iter_pr_subtask = (int64_t)(kappa / (C + DBL_EPSILON));
  if (min_iter_pr_subtask == 0) return nthreads; // => kappa < C
  return (int)min_int64(max_int64(iterations / min_iter_pr_subtask, 1), nthreads);
}


static inline int is_small(struct scheduler_task *task, int nsubtasks)
{
  int64_t time = *task->task_time;
  int64_t iter = *task->task_iter;

  if (task->sched == DYNAMIC) return 0;
  if (*task->task_iter == 0) return 0;

  // Estimate the constant C
  double C = (double)time / (double)iter;
  double cur_task_iter = (double) task->iterations;

  // Returns true if the task is small if
  // 1. If the number of maximum subtask worth creating is less or equal to 1
  // 2. If the number of iterations times C is smaller than the overhead of task creation
  if (nsubtasks <= 1 || C * cur_task_iter < kappa * nsubtasks)
    return 1;

  return 0;
}

// TODO make this prettier
static inline struct subtask* setup_subtask(parloop_fn fn,
                                            void* args,
                                            const char* name,
                                            volatile int* counter,
                                            int64_t *timer,
                                            int64_t *iter,
                                            int64_t start, int64_t end,
                                            int chunkable,
                                            int64_t chunk_size,
                                            int id)
{
  struct subtask* subtask = malloc(sizeof(struct subtask));
  if (subtask == NULL) {
    assert(!"malloc failed in setup_subtask");
    return  NULL;
  }
  subtask->fn         = fn;
  subtask->args       = args;

  subtask->counter   = counter;
  subtask->task_time = timer;
  subtask->task_iter = iter;

  subtask->start      = start;
  subtask->end        = end;
  subtask->id         = id;
  subtask->chunkable  = chunkable;
  subtask->chunk_size = chunk_size;

  subtask->name       = name;
  return subtask;
}



#endif

// end of scheduler_common.h
