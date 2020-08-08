// start of scheduler.h
#ifndef _SCHEDULER_H_
#define _SCHEDULER_H_


static volatile sig_atomic_t free_workers;
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

static inline int is_small(struct scheduler_task *task)
{
  int64_t time = *task->total_time;
  int64_t iter = *task->total_iter;

  if (task->sched == DYNAMIC) return 0;
  if (*task->total_iter == 0) return 0;

  // Estimate the constant C
  double C = (double)time / (double)iter;
  double cur_task_iter = (double) task->iterations;

  if (C * cur_task_iter < kappa)
  {
    return 1;
  }
  return 0;
}

static inline int run_subtask(struct worker* worker, struct subtask* subtask)
{
  assert(subtask != NULL);
  assert(worker != NULL);
  int64_t start = get_wall_time();
  int err = subtask->fn(subtask->args, subtask->start, subtask->end,
                        subtask->chunkable ? worker->tid : subtask->id, worker->tid);
  if (err != 0)
    return err;
  int64_t end = get_wall_time();
  int64_t time_elapsed = end - start;
  worker->time_spent_working += time_elapsed;
  int32_t iter = subtask->end - subtask->start;
  // report time measurements
  __atomic_fetch_add(subtask->total_time, time_elapsed, __ATOMIC_RELAXED);
  __atomic_fetch_add(subtask->total_iter, iter,         __ATOMIC_RELAXED);
  /* __atomic_thread_fence(__ATOMIC_RELEASE); */
  __atomic_fetch_sub(subtask->counter, 1, __ATOMIC_RELAXED);
  free(subtask);
  return 0;
}


static inline struct subtask* split(struct worker* worker, struct subtask *subtask)
{
  int remaining_iter = subtask->end - subtask->start;
  if (subtask->chunkable && remaining_iter > subtask->iterations)
  {
    struct subtask *new_subtask = malloc(sizeof(struct subtask));
    *new_subtask = *subtask;
    __atomic_fetch_add(subtask->counter, 1, __ATOMIC_RELAXED);
    subtask->end = subtask->start + subtask->iterations;
    new_subtask->start = subtask->end;
    /* new_subtask->iterations *= 2; */
    push_back(&worker->q, new_subtask);
  }
  return subtask;
}


void acquire (struct worker *worker)
{
  assert(num_workers >= 2);

  struct scheduler* scheduler = worker->scheduler;
  int my_id = worker->tid;
  while (! is_finished(worker))
  {
    int k = random_other_worker(scheduler, my_id);

    struct deque *deque_k = &scheduler->workers[k].q;
    struct subtask* subtask = steal(deque_k);
    if (subtask == STEAL_RES_EMPTY) {
      // TODO: log
    } else if (subtask == STEAL_RES_ABORT) {
      // TODO: log
    } else if (subtask == STEAL_RES_DEAD) {
      fprintf(stderr, "tid %d tried to steal from dead queue %d\n", my_id, k);
    } else {
      assert(subtask != NULL);
      // We stole a task, so we reset it's iteration counter
      if (subtask->chunkable && *subtask->total_iter > 0)
      {
        double C = (double)*subtask->total_time / (double)*subtask->total_iter;
        subtask->iterations = C > 0.0f ? kappa / C : 1;
        subtask->iterations = subtask->iterations <= 0 ? 1 : subtask->iterations * 10;
      }
      // Split subtask into two (if dynamic)
      struct subtask* subtask_new = split(worker, subtask);
      push_back(&worker->q, subtask_new);
      return;
    }
  }
}

static inline void *scheduler_worker(void* arg)
{
  struct worker *worker = (struct worker*) arg;
  worker_local = worker;
  /* pthread_detach(pthread_self()); */
  while (!is_finished(worker))
  {
    if (! empty(&worker->q)) {
      struct subtask* subtask = pop_back(&worker->q);
      if (subtask == NULL) {
        continue;
      }
      struct subtask* subtask_new = split(worker, subtask);
      int err = run_subtask(worker, subtask_new);
      /* Only one error can be returned at the time now
         Maybe we can provide a stack like structure for pushing errors onto
         if we wish to backpropagte multiple errors */
      if (err != 0) {
        __atomic_store_n(&scheduler_error, err, __ATOMIC_RELAXED);
      }
    } else if (__atomic_load_n(&num_workers, __ATOMIC_RELAXED) == 1) {
      break;
    } else { // steal
      acquire(worker);
    }
  }

  assert(empty(&worker->q));
  __atomic_fetch_sub(&num_workers, 1, __ATOMIC_RELAXED);
  output_thread_usage(worker);
  return NULL;
}


static inline int scheduler_execute_parallel(struct scheduler *scheduler,
                                             struct scheduler_subtask *task)
{
#ifdef MCDEBUG
  fprintf(stderr, "[scheduler_parallel] Performing scheduling with scheduling %s\n", task->info.sched == STATIC ? "STATIC" : "DYNAMIC");
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
  if (chunkable && *total_iter > 0)
  {
    double C = (double)*total_time / (double)*total_iter;
    iter = kappa / C;
    iter = iter <= 0 ? 1 : iter;
  }

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
      if (subtask != NULL) {
        // Do work
        assert(subtask->fn != NULL);
        assert(subtask->args != NULL);
        struct subtask* subtask_new  = split(worker, subtask);
        int err = run_subtask(worker, subtask_new);
        if (err != 0) {
          return err;
        }
      }
    } else {
      struct scheduler* scheduler = worker->scheduler;
      int my_id = worker->tid;
      while (shared_counter != 0 && empty(&worker->q) && scheduler_error == 0)
      {
        int k = random_other_worker(scheduler, my_id);
        if (scheduler->workers[k].dead) {
          continue;
        }
        struct deque *deque_k = &scheduler->workers[k].q;
        struct subtask* subtask = steal(deque_k);
        if (subtask == STEAL_RES_EMPTY) {
          // TODO: log
        } else if (subtask == STEAL_RES_ABORT) {
          // TODO: log
        } else if (subtask == STEAL_RES_DEAD){
          fprintf(stderr, "tid %d tried to steal from dead queue %d\n", my_id, k);
        } else {
          assert(subtask != NULL);
          // We stole a task, so we reset it's iteration counter
          if (subtask->chunkable && *subtask->total_iter > 0)
          {
            double C = (double)*subtask->total_time / (double)*subtask->total_iter;
            subtask->iterations = C > 0.0f ? kappa / C : 1;
            subtask->iterations = subtask->iterations <= 0 ? 1 : subtask->iterations * 10;
          }
          // Split subtask into two (if dynamic)
          struct subtask* subtask_new = split(worker, subtask);
          push_back(&worker->q, subtask_new);
        }
      }
    }
  }
  return scheduler_error;
}


static inline int scheduler_execute_task(struct scheduler *scheduler,
                                         struct scheduler_subtask *task)
{
#ifdef MCDEBUG
  fprintf(stderr, "[scheduler_execute] starting task %s with %ld iterations \n", task->name, task->iterations);
#endif
  assert(scheduler != NULL);
  assert(task != NULL);

  if (task->iterations == 0) {
    return 0;
  }

  if (task->info.nsubtasks == 1) {
    int64_t start = get_wall_time();
    int err = task->fn(task->args, 0, task->iterations, 0, worker_local->tid);
    int64_t end = get_wall_time();
    int64_t time_elapsed = end - start;
    worker_local->time_spent_working += time_elapsed;
    __atomic_fetch_add(task->info.total_time, time_elapsed,     __ATOMIC_RELAXED);
    __atomic_fetch_add(task->info.total_iter, task->iterations, __ATOMIC_RELAXED);
    return err;
  }

  free_workers -= task->info.nsubtasks;
  int err = scheduler_execute_parallel(scheduler, task);
  free_workers += task->info.nsubtasks;
  return err;
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
  info.min_cost = task->min_cost;

  // Decide if task should be scheduled sequentially
  if (is_small(task)) {
    info.iter_pr_subtask = task->iterations;
    info.remainder = 0;
    info.nsubtasks = 1;
    return task->seq_fn(task->args, task->iterations, worker_local->tid, info);
  }

  int max_num_tasks = scheduler->num_threads;
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
    // As any thread can take any subtasks
    // we are being safe with returning
    // an upper bound on the number of tasks
    info.nsubtasks = info.iter_pr_subtask == 0 ? info.remainder : max_num_tasks;
    info.sched = DYNAMIC;
    break;
  default:
    assert(!"Got unknown scheduling");
  }
  int err = task->seq_fn(task->args, task->iterations, worker_local->tid, info);
  return err;
}

#endif
// End of scheduler.h
