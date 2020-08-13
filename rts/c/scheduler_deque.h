// start of scheduler.h
#ifndef _SCHEDULER_H_
#define _SCHEDULER_H_

#include <float.h>

static volatile sig_atomic_t num_workers;
__thread struct worker* worker_local = NULL;

static inline int is_finished(struct worker *worker) {
  return __atomic_load_n(&worker->dead, __ATOMIC_RELAXED) && empty(&worker->q);
}

int random_other_worker(struct scheduler *scheduler, int my_id)
{
  int my_num_workers = __atomic_load_n(&num_workers, __ATOMIC_RELAXED);
  assert(my_num_workers != 1);
  int i = fast_rand() % (my_num_workers - 1);
  if (i >= my_id) {
    i++;
  }
  assert(i >= 0);
  assert(i < my_num_workers);
  assert(i != my_id);

  return i;
}


static inline struct subtask* chunk_subtask(struct worker* worker, struct subtask *subtask)
{
  int remaining_iter = subtask->end - subtask->start;
  if (subtask->chunkable && remaining_iter > subtask->iterations)
  {
    struct subtask *new_subtask = malloc(sizeof(struct subtask));
    *new_subtask = *subtask;
    __atomic_fetch_add(subtask->counter, 1, __ATOMIC_RELAXED);
    subtask->end = subtask->start + subtask->iterations;
    new_subtask->start = subtask->end;
    push_back(&worker->q, new_subtask);
  }
  return subtask;
}


static inline int chunk_dynamic_subtask(struct subtask* subtask, struct worker *worker)
{
  struct scheduler* scheduler = worker->scheduler;
  long int rem_iter = subtask->end-subtask->start;

  double C = (double)*subtask->total_time / (double)*subtask->total_iter;
  // Should we be careful or nah?
  int min_iter_pr_subtask = (int) (kappa / (C + DBL_EPSILON));
  min_iter_pr_subtask = min_iter_pr_subtask == 0 ? 1 : min_iter_pr_subtask;
  int nsubtasks = rem_iter / min_iter_pr_subtask;
  if (nsubtasks == 0) {
    return rem_iter;
  } else if (nsubtasks > scheduler->num_threads) {
    nsubtasks = scheduler->num_threads;
  }

  int iter_pr_subtask = rem_iter / nsubtasks;
  return iter_pr_subtask;
}


// Try to steal from a random queue
static inline int steal_from_random_worker(struct worker* worker)
{
  int my_id = worker->tid;
  struct scheduler* scheduler = worker->scheduler;
  int k = random_other_worker(scheduler, my_id);
  struct deque *deque_k = &scheduler->workers[k].q;
  if (empty(deque_k)) return 0;
  struct subtask* subtask = steal(deque_k);
  // otherwise try to steal from
  if (subtask == STEAL_RES_EMPTY) {
    // TODO: log
  } else if (subtask == STEAL_RES_ABORT) {
    // TODO: log
  } else {
    assert(subtask != NULL);
    // We stole a task, so we re-compute it's iteration counter
    if (subtask->chunkable && *subtask->total_iter > 0)
    {
      subtask->iterations = chunk_dynamic_subtask(subtask, worker);
    }
    push_back(&worker->q, subtask);
    return 1;
  }
  return 0;
}


static inline int run_subtask(struct worker* worker, struct subtask* subtask)
{
  assert(subtask != NULL);
  assert(worker != NULL);
#ifdef MCPROFILE
  int64_t start = get_wall_time();
#endif
  int err = subtask->fn(subtask->args, subtask->start, subtask->end,
                        subtask->chunkable ? worker->tid : subtask->id,
                        worker->tid, subtask->total_time);
  if (err != 0)
    return err;
#ifdef MCPROFILE
  int64_t end = get_wall_time();
  int64_t time_elapsed = end - start;
  worker->time_spent_working += time_elapsed;
#endif
  int32_t iter = subtask->end - subtask->start;
  // report measurements
  __atomic_fetch_add(subtask->total_iter, iter,         __ATOMIC_RELAXED);
  __atomic_fetch_sub(subtask->counter, 1, __ATOMIC_RELAXED);
  free(subtask);
  return 0;
}

static inline int compute_max_num_subtasks(int nthreads,
                                           struct scheduler_info info,
                                           long int iterations)
{
  if (*info.total_iter == 0) return nthreads;
  double C = (double)*info.total_time / (double)*info.total_iter;
  int min_iter_pr_subtask = (int)(kappa / (C + DBL_EPSILON));
  if (min_iter_pr_subtask == 0) return nthreads; // => kappa < C
  int nsubtasks = iterations / min_iter_pr_subtask;
  return nsubtasks > nthreads ? nthreads : nsubtasks;
}

static inline int is_small(struct scheduler_task *task, int ntasks)
{
  int64_t time = *task->total_time;
  int64_t iter = *task->total_iter;

  if (task->sched == DYNAMIC) return 0;
  if (*task->total_iter == 0) return 0;

  // Estimate the constant C
  double C = (double)time / (double)iter;
  double cur_task_iter = (double) task->iterations;

  if (C * cur_task_iter < kappa * ntasks)
    return 1;

  return 0;
}



static inline void *scheduler_worker(void* arg)
{
  struct worker *worker = (struct worker*) arg;
  worker_local = worker;
  while (!is_finished(worker))
  {
    if (!empty(&worker->q)) {
      struct subtask* subtask = pop_back(&worker->q);
      if (subtask == NULL) continue;
      struct subtask* subtask_new = chunk_subtask(worker, subtask);
      int err = run_subtask(worker, subtask_new);
      /* Only one error can be returned at the time now
         Maybe we can provide a stack like structure for pushing errors onto
         if we wish to backpropagte multiple errors */
      if (err != 0) {
        __atomic_store_n(&scheduler_error, err, __ATOMIC_RELAXED);
      }
    } else if (__atomic_load_n(&num_workers, __ATOMIC_RELAXED) == 1) {
      break;
    } else { // try to steal
      assert(num_workers >= 2);
      while(!is_finished(worker)) {
        if (steal_from_random_worker(worker))
          break;
      }
    }
  }
  assert(empty(&worker->q));
  __atomic_fetch_sub(&num_workers, 1, __ATOMIC_RELAXED);
  if (worker->output_usage)
    output_thread_usage(worker);
  return NULL;
}


static inline int scheduler_execute_parallel(struct scheduler *scheduler,
                                             struct scheduler_subtask *task)
{
#ifdef MCDEBUG
  fprintf(stderr, "[scheduler_parallel] Performing scheduling with scheduling %s\n",
          task->info.sched == STATIC ? "STATIC" : "DYNAMIC");
#endif

  struct worker * worker = worker_local;

  struct scheduler_info info = task->info;
  int iter_pr_subtask = info.iter_pr_subtask;
  int remainder = info.remainder;
  int nsubtasks = info.nsubtasks;
  int64_t *total_time = info.total_time;
  int64_t *total_iter = info.total_iter;
  enum scheduling sched = info.sched;

  volatile int shared_counter = nsubtasks;

  /* Each subtasks can be processed in chunks */
  int chunkable = sched == STATIC ? 0 : 1;
  int iter = 1;

  int start = 0;
  int subtask_id = 0;
  int end = iter_pr_subtask + (int)(remainder != 0);
  for (subtask_id = 0; subtask_id < nsubtasks; subtask_id++) {
    struct subtask *subtask = setup_subtask(task->fn, task->args, task->name,
                                            &shared_counter,
                                            total_time, total_iter,
                                            start, end,
                                            chunkable, iter,
                                            subtask_id);
    assert(subtask != NULL);
    push_back(&scheduler->workers[worker->tid].q, subtask);
#ifdef MCDEBUG
    fprintf(stderr, "[scheduler_task] pushed %d iterations onto %d's q\n", (end - start), worker->tid);
#endif
    // Update range params
    start = end;
    end += iter_pr_subtask + ((subtask_id + 1) < remainder);
  }

  while(shared_counter != 0 && scheduler_error == 0)
  {
    if (!empty(&worker->q)) {
      struct subtask * subtask = pop_back(&worker->q);
      if (subtask == NULL) continue;
      struct subtask* subtask_new = chunk_subtask(worker, subtask);
      int err = run_subtask(worker, subtask_new);
      if (err != 0) {
        return err;
      }
    } else {
      while (shared_counter != 0 && empty(&worker->q) && scheduler_error == 0) {
        steal_from_random_worker(worker);
      }
    }
  }
  return scheduler_error;
}


static inline int scheduler_execute_task(struct scheduler *scheduler,
                                         struct scheduler_subtask *task)
{
#ifdef MCDEBUG
  fprintf(stderr, "[scheduler_execute] starting task %s with %ld iterations \n",
          task->name, task->iterations);
#endif
  assert(scheduler != NULL);
  assert(task != NULL);

  if (task->iterations == 0) {
    return 0;
  }

  if (task->info.nsubtasks == 1) {
#ifdef MCPROFILE
    int64_t start = get_wall_time();
#endif
    int err = task->fn(task->args, 0, task->iterations, 0, worker_local->tid, task->info.total_time);
#ifdef MCPROFILE
    int64_t end = get_wall_time();
    int64_t time_elapsed = end - start;
    worker_local->time_spent_working += time_elapsed;
#endif
    __atomic_fetch_add(task->info.total_iter, task->iterations, __ATOMIC_RELAXED);
    return err;
  }

  return scheduler_execute_parallel(scheduler, task);
}

/* Decide on how schedule the incoming task i.e. how many subtasks and
   to run sequential or (potentially nested) parallel code body */
static inline int scheduler_prepare_task(struct scheduler* scheduler,
                                         struct scheduler_task *task)
{
  assert(task != NULL);
#ifdef MCDEBUG
  fprintf(stderr, "[scheduler_do_task] starting task with %ld iterations\n", task->iterations);
#endif

  struct scheduler_info info;
  info.total_time = task->total_time;
  info.total_iter = task->total_iter;

  int max_num_tasks = task->sched == STATIC ?
    compute_max_num_subtasks(scheduler->num_threads, info, task->iterations):
    scheduler->num_threads;

  // Decide if task should be scheduled sequentially
  if (max_num_tasks <= 1 || is_small(task, max_num_tasks)) {
    info.iter_pr_subtask = task->iterations;
    info.remainder = 0;
    info.nsubtasks = 1;
    return task->seq_fn(task->args, task->iterations, worker_local->tid, info);
  }


  switch (task->sched) {
  case STATIC:
    info.iter_pr_subtask = task->iterations / max_num_tasks;
    info.remainder = task->iterations % max_num_tasks;
    info.nsubtasks = info.iter_pr_subtask == 0 ? info.remainder : ((task->iterations - info.remainder) / info.iter_pr_subtask);
    info.sched = STATIC;
    break;
  case DYNAMIC:
    info.iter_pr_subtask = task->iterations / max_num_tasks;
    info.remainder = task->iterations % max_num_tasks;
    // As any thread can take any subtasks, we are being safe with returning
    // an upper bound on the number of tasks
    info.nsubtasks = info.iter_pr_subtask == 0 ? info.remainder : max_num_tasks;
    info.sched = DYNAMIC;
    break;
  default:
    assert(!"Got unknown scheduling");
  }

  return task->seq_fn(task->args, task->iterations, worker_local->tid, info);
}

#endif
// End of scheduler.h
