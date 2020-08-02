// start of scheduler.h
#ifndef _SCHEDULER_H_
#define _SCHEDULER_H_



static volatile sig_atomic_t num_workers;
static volatile sig_atomic_t free_workers;

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

void acquire (struct worker *worker)
{
  assert(num_workers >= 2);

  struct scheduler* scheduler = worker->scheduler;
  int my_id = worker->tid;
  while (! is_finished(worker)) {
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
      push_back(&worker->q, subtask);
      return;
    }
  }
}

static inline void *scheduler_worker(void* arg)
{
  struct worker *worker = (struct worker*) arg;
  worker_local = worker;
  while (!is_finished(worker))
  {
    if (! empty(&worker->q)) {
      struct subtask* subtask = pop_back(&worker->q);
      if (subtask == NULL) {
        continue;
      }

      int err = subtask->fn(subtask->args, subtask->start, subtask->end, subtask->id);
      /* Only one error can be returned at the time now
         Maybe we can provide a stack like structure for pushing errors onto
         if we wish to backpropagte multiple errors */
      if (err != 0) {
        scheduler_error = err;
      }
      __atomic_fetch_sub(subtask->counter, 1, __ATOMIC_SEQ_CST);
      free(subtask);
    } else if (__atomic_load_n(&num_workers, __ATOMIC_RELAXED) == 1) {
      break;
    } else { // steal
      acquire(worker);
    }
  }

  worker->dead = 1;
  assert(empty(&worker->q));
  __atomic_fetch_sub(&num_workers, 1, __ATOMIC_RELAXED);
  return NULL;
}


static inline int scheduler_parallel(struct scheduler *scheduler,
                                     struct scheduler_subtask *task)
{
#ifdef MCDEBUG
  fprintf(stderr, "[scheduler_parallel] Performing scheduling with granularity %d\n", task->granularity);
#endif

  struct worker * worker = worker_local;
  pthread_mutex_t mutex;
  CHECK_ERR(pthread_mutex_init(&mutex, NULL), "pthread_mutex_init");
  pthread_cond_t cond;
  CHECK_ERR(pthread_cond_init(&cond, NULL), "pthread_cond_init");

  struct scheduler_info info = task->info;
  int iter_pr_subtask = info.iter_pr_subtask;
  int remainder = info.remainder;
  int nsubtasks = info.nsubtasks;

  volatile int shared_counter = nsubtasks;

  /* Each subtasks is processed in chunks */
  int chunks = 0;
  if (task->granularity > 0) {
    chunks = iter_pr_subtask / task->granularity == 0 ? 1 : iter_pr_subtask / task->granularity;
  }

  int start = 0;
  int subtask_id = 0;
  int end = iter_pr_subtask + (int)(remainder != 0);
  for (subtask_id = 0; subtask_id < nsubtasks; subtask_id++) {
    struct subtask *subtask = setup_subtask(task->fn, task->args, task->name,
                                            &mutex, &cond, &shared_counter,
                                            start, end, chunks, subtask_id);
    assert(subtask != NULL);
    push_back(&scheduler->workers[worker->tid].q, subtask);
#ifdef MCDEBUG
    fprintf(stderr, "[scheduler_task] pushed %d iterations onto %d's q\n", (end - start), worker->tid);
#endif
    // Update range params
    start = end;
    end += iter_pr_subtask + ((subtask_id + 1) < remainder);
  }

  while(shared_counter != 0) {
    struct subtask * subtask = pop_back(&worker->q);
    if (subtask != NULL) {
      // Do work
      assert(subtask->fn != NULL);
      assert(subtask->args != NULL);

      int err = subtask->fn(subtask->args, subtask->start, subtask->end, subtask->id);
      if (err != 0) {
        return err;
      }
      __atomic_fetch_sub(&shared_counter, 1, __ATOMIC_RELAXED);
      free(subtask);
    }
  }
  return scheduler_error;
}



static inline int scheduler_execute(struct scheduler *scheduler,
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
    return task->fn(task->args, 0, task->iterations, 0);
  }

  return scheduler_parallel(scheduler, task);
}

/* Decide whether to run sequential  or (potentially nested) parallel code body */
static inline int scheduler_do_task(struct scheduler* scheduler,
                                    struct scheduler_task *task)
{
  assert(task != NULL);
#ifdef MCDEBUG
  fprintf(stderr, "[scheduler_do_task] starting task with %ld iterations\n", task->iterations);
#endif

  struct scheduler_info info;

  /* Run task directly if all other workers are occupied */
  if (free_workers <= 0) {
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
    break;
  case DYNAMIC:
    info.iter_pr_subtask = task->iterations / max_num_tasks;
    info.remainder = task->iterations % max_num_tasks;
    // As any thread can take any subtasks
    // we are being safe with returning
    // an upper bound on the number of tasks
    info.nsubtasks = info.iter_pr_subtask == 0 ? info.remainder : max_num_tasks;
    break;
  default:
    assert(!"Got unknown scheduling");
  }

  free_workers -= info.nsubtasks;
  int err = task->seq_fn(task->args, task->iterations, worker_local->tid, info);
  free_workers += info.nsubtasks;
  return err;
}

#endif
// End of scheduler.h
