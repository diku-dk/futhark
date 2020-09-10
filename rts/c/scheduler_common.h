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

static volatile sig_atomic_t scheduler_error = 0;

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


static inline int64_t compute_chunk_size(struct subtask* subtask)
{
  double C = (double)*subtask->task_time / (double)*subtask->task_iter;
  assert(C >= 0.0f);
  return max_int64((int64_t)(kappa / (C + DBL_EPSILON)), 1);
}

static inline struct subtask* chunk_subtask(struct worker* worker, struct subtask *subtask)
{
  if (subtask->chunkable) {
    // Do we have information from previous runs avaliable
    if (*subtask->task_iter > 0) {
      subtask->chunk_size = compute_chunk_size(subtask);
      assert(subtask->chunk_size > 0);
    }
    int64_t remaining_iter = subtask->end - subtask->start;
    assert(remaining_iter > 0);
    if (remaining_iter > subtask->chunk_size) {
      struct subtask *new_subtask = malloc(sizeof(struct subtask));
      *new_subtask = *subtask;
      // increment the subtask join counter
      __atomic_fetch_add(subtask->counter, 1, __ATOMIC_RELAXED);
      // Update range parameters
      subtask->end = subtask->start + subtask->chunk_size;
      new_subtask->start = subtask->end;
#if defined(MCCHASELEV)
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
#if defined(MCPROFILE)
  int64_t start = worker->timer;
#endif
  worker->nested++;
  int err = subtask->fn(subtask->args, subtask->start, subtask->end,
                        subtask->chunkable ? worker->tid : subtask->id,
                        worker->tid);
  worker->nested--;
  // Some error occured during some other subtask
  // so we just clean-up and return
  if (scheduler_error != 0) {
    free(subtask);
    return 0;
  }
  if (err != 0) {
    __atomic_store_n(&scheduler_error, err, __ATOMIC_RELAXED);
  }
  // Total sequential time spent
  int64_t time_elapsed = total_now(worker->total, worker->timer);
#if defined(MCPROFILE)
  worker->time_spent_working += get_wall_time_ns() - start;
#endif
  int64_t iter = subtask->end - subtask->start;
  // report measurements
  __atomic_fetch_add(subtask->task_time, time_elapsed, __ATOMIC_RELAXED);
  __atomic_fetch_add(subtask->task_iter, iter, __ATOMIC_RELAXED);
  // We need a fence here, since if the counter is decremented before either
  // of the two above are updated bad things can happen, e.g. if they are stack-allocated
  __atomic_thread_fence(__ATOMIC_SEQ_CST);
  __atomic_fetch_sub(subtask->counter, 1, __ATOMIC_RELAXED);
  free(subtask);
  return 0;
}


static inline int is_small(struct scheduler_task *task, int nthreads, int *nsubtasks)
{
  int64_t time = *task->task_time;
  int64_t iter = *task->task_iter;


  if (task->sched == DYNAMIC || iter == 0) {
    *nsubtasks = nthreads;
    return 0;
  }

  // Estimate the constant C
  double C = (double)time / (double)iter;
  double cur_task_iter = (double) task->iterations;

  // Returns true if the task is small i.e.
  // if the number of iterations times C is smaller than the overhead of task creation
  if (C * cur_task_iter < kappa) {
    *nsubtasks = 1;
    return 1;
  }

  // Else compute how many subtasks this tasks should create
  int64_t min_iter_pr_subtask = max_int64((int64_t)(kappa / (C + DBL_EPSILON)), 1);
  *nsubtasks = (int)min_int64(max_int64(task->iterations / min_iter_pr_subtask, 1), nthreads);

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
