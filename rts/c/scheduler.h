// start of scheduler.h
#ifndef _SCHEDULER_H_
#define _SCHEDULER_H_

#if defined(MCJOBQUEUE)


static int dummy_counter = 0;
static int64_t dummy_timer = 0;
static int64_t dummy_iter = 0;

static int dummy_fn(void *args, int64_t start, int64_t end, int subtask_id, int tid) {
  return 0;
}


static inline void wake_up_threads(struct scheduler *scheduler, int start_tid, int end_tid) {

  assert(start_tid >= 1);
  assert(end_tid <= scheduler->num_threads);
  for (int i = start_tid; i < end_tid; i++) {

    struct subtask *subtask = setup_subtask(dummy_fn, NULL, "dummy_fn",
                                            &dummy_counter,
                                            &dummy_timer, &dummy_iter,
                                            0, 0,
                                            0, 0,
                                            0);
    CHECK_ERR(subtask_queue_enqueue(&scheduler->workers[i], subtask), "subtask_queue_enqueue");
  }
}

static inline int is_finished(struct worker *worker) {
  return __atomic_load_n(&worker->dead, __ATOMIC_RELAXED) && subtask_queue_is_empty(&worker->q);
}

// Try to steal from a random queue
static inline int steal_from_random_worker(struct worker* worker)
{
  int my_id = worker->tid;
  struct scheduler* scheduler = worker->scheduler;
  int k = random_other_worker(scheduler, my_id);
  struct worker *worker_k = &scheduler->workers[k];
  struct subtask* subtask =  NULL;
  int retval = subtask_queue_steal(worker_k, &subtask);
  if (retval == 0) {
    subtask_queue_enqueue(worker, subtask);
    return 1;
  }
  return 0;
}

/* Only one error can be returned at the time now
   Maybe we can provide a stack like structure for pushing errors onto
   if we wish to backpropagte multiple errors */
static inline void *scheduler_worker(void* arg)
{
  struct worker *worker = (struct worker*) arg;
  worker_local = worker;
  struct subtask * subtask = NULL;
  while(!is_finished(worker)) {
    if (!subtask_queue_is_empty(&worker->q)) {
      int retval = subtask_queue_dequeue(worker, &subtask, 0);
      if (retval == 0) {
        assert(subtask != NULL);
        struct subtask* subtask_new = chunk_subtask(worker, subtask);
        CHECK_ERR(run_subtask(worker, subtask_new), "run_subtask");
      } // else someone stole our work

    } else if (active_work) {
      /* steal */
      while (!is_finished(worker) && active_work) {
        if (steal_from_random_worker(worker)) {
          break;
        }
      }
    } else {
      // go back to sleep

      int retval = subtask_queue_dequeue(worker, &subtask, 1);
      if (retval == 0) {
        assert(subtask != NULL);
        struct subtask* subtask_new = chunk_subtask(worker, subtask);
        CHECK_ERR(run_subtask(worker, subtask_new), "run_subtask");
      }
    }
  }

  assert(subtask_queue_is_empty(&worker->q));
  __atomic_fetch_sub(&num_workers, 1, __ATOMIC_RELAXED);
  if (worker->output_usage)
    output_thread_usage(worker);
  return NULL;
}



static inline int scheduler_execute_parloop(struct scheduler *scheduler,
                                            struct scheduler_parloop *task,
                                            int64_t *timer)
{

  struct worker * worker = worker_local;

  struct scheduler_info info = task->info;
  int64_t iter_pr_subtask = info.iter_pr_subtask;
  int64_t remainder = info.remainder;
  int nsubtasks = info.nsubtasks;
  volatile int shared_counter = nsubtasks;

  // Shared timer used to sum up all
  // sequential work from each subtask
  int64_t task_timer = 0;
  int64_t task_iter = 0;

  enum scheduling sched = info.sched;
  /* If each subtasks should be processed in chunks */
  int chunkable = sched == STATIC ? 0 : 1;
  int64_t chunk_size = 1; // The initial chunk size when no info is avaliable


  if (info.wake_up_threads || sched == DYNAMIC)
    __atomic_add_fetch(&active_work, nsubtasks, __ATOMIC_RELAXED);

  int subtask_id = 0;
  int64_t start = 0;
  int64_t end = iter_pr_subtask + (int)(remainder != 0);
  for (subtask_id = 0; subtask_id < nsubtasks; subtask_id++) {
    struct subtask *subtask = setup_subtask(task->fn, task->args, task->name,
                                            &shared_counter,
                                            &task_timer, &task_iter,
                                            start, end,
                                            chunkable, chunk_size,
                                            subtask_id);
    assert(subtask != NULL);
    if (worker->nested){
      CHECK_ERR(subtask_queue_enqueue(&scheduler->workers[worker->tid], subtask),
                "subtask_queue_enqueue");
    } else {
      CHECK_ERR(subtask_queue_enqueue(&scheduler->workers[subtask_id%scheduler->num_threads], subtask),
                "subtask_queue_enqueue");
    }
    // Update range params
    start = end;
    end += iter_pr_subtask + ((subtask_id + 1) < remainder);
  }

  if (info.wake_up_threads) {
    wake_up_threads(scheduler, nsubtasks, scheduler->num_threads);
  }

  // Join (wait for subtasks to finish)
  while(shared_counter != 0 && scheduler_error == 0) {
    if (!subtask_queue_is_empty(&worker->q)) {
      struct subtask *subtask = NULL;
      int err = subtask_queue_dequeue(worker, &subtask, 0);
      if (err == 0 ) {
        struct subtask* subtask_new = chunk_subtask(worker, subtask);
        CHECK_ERR(run_subtask(worker, subtask_new), "run_subtask");
      }
    } else {
      while (shared_counter != 0 && subtask_queue_is_empty(&worker->q) && scheduler_error == 0) {
        if (steal_from_random_worker(worker)) {
          struct subtask *subtask = NULL;
          int err = subtask_queue_dequeue(worker, &subtask, 0);
          if (err == 0 ) {
            struct subtask* subtask_new = chunk_subtask(worker, subtask);
            CHECK_ERR(run_subtask(worker, subtask_new), "run_subtask");
          }
        }
      }
    }
  }


  if (info.wake_up_threads || sched == DYNAMIC) {
    __atomic_sub_fetch(&active_work, nsubtasks, __ATOMIC_RELAXED);
  }

  // Write back timing results
  (*timer) += task_timer;
  return scheduler_error;
}

#elif defined(MCCHASELEV)

static inline int is_finished(struct worker *worker) {
  return __atomic_load_n(&worker->dead, __ATOMIC_RELAXED) && empty(&worker->q);
}

static inline void split(struct worker* worker, struct subtask *subtask)
{
  int64_t remaining_iter = subtask->end - subtask->start;
  if (subtask->chunkable && remaining_iter > 1) {
    int64_t half = remaining_iter / 2;
    struct subtask* new_subtask = malloc(sizeof(struct subtask));
    *new_subtask = *subtask;
    new_subtask->start = subtask->end - half;
    subtask->end = new_subtask->start;
    __atomic_fetch_add(subtask->counter, 1, __ATOMIC_RELAXED);
    push_back(&worker->q, new_subtask);
  }
  return;
}

// Try to steal from a random queue
static inline int steal_from_random_worker(struct worker* worker)
{
  struct scheduler* scheduler = worker->scheduler;
  int my_id = worker->tid;
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
    split(worker, subtask);
    push_back(&worker->q, subtask);
    return 1;
  }

  return 0;
}

static inline void *scheduler_worker(void* arg)
{
  struct worker *worker = (struct worker*) arg;
  worker_local = worker;
  while (!is_finished(worker))
  {
    if (active_work == 0) {
      int n_sig;
      CHECK_ERR(sigwait(&scheduler_sig_set, &n_sig), "sigwait");

    } else if (!empty(&worker->q)) {
      struct subtask* subtask = pop_back(&worker->q);
      if (subtask == NULL) continue;
      struct subtask* subtask_new = chunk_subtask(worker, subtask);
      CHECK_ERR(run_subtask(worker, subtask_new), "run_subtask");
      /* Only one error can be returned at the time now
         Maybe we can provide a stack like structure for pushing errors onto
         if we wish to backpropagte multiple errors */
    } else if (__atomic_load_n(&num_workers, __ATOMIC_RELAXED) == 1) {
      break;
    } else { // try to steal
      assert(num_workers >= 2);
      while(!is_finished(worker) && active_work) {
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

static inline void wake_up_all_threads(struct scheduler *scheduler) {
  for (int i = 1; i < scheduler->num_threads; i++) {
    CHECK_ERRNO(pthread_kill(scheduler->workers[i].thread, SIGUSR1), "pthread_kill");
  }
}


static inline int scheduler_execute_parloop(struct scheduler *scheduler,
                                             struct scheduler_parloop *task,
                                             int64_t *timer)
{
  struct worker * worker = worker_local;

  int old_active_work = active_work;
  __atomic_add_fetch(&active_work, 1, __ATOMIC_SEQ_CST);
  if(old_active_work == 0) {
    wake_up_all_threads(scheduler);
  }

  struct scheduler_info info = task->info;
  int64_t iter_pr_subtask = info.iter_pr_subtask;
  int64_t remainder = info.remainder;
  int nsubtasks = info.nsubtasks;
  enum scheduling sched = info.sched;

  volatile int shared_counter = nsubtasks;

  // Shared timer used to sum up all
  // sequential work from each subtask
  int64_t task_timer = 0;
  int64_t task_iter = 0;

  /* Each subtasks can be processed in chunks */
  int chunkable = sched == STATIC ? 0 : 1;
  int64_t iter = 1;

  int subtask_id = 0;
  int64_t start = 0;
  int64_t end = iter_pr_subtask + (int)(remainder != 0);
  for (subtask_id = 0; subtask_id < nsubtasks; subtask_id++) {
    struct subtask *subtask = setup_subtask(task->fn, task->args, task->name,
                                            &shared_counter,
                                            &task_timer, &task_iter,
                                            start, end,
                                            chunkable, iter,
                                            subtask_id);
    assert(subtask != NULL);
    push_back(&worker->q, subtask);

    // Update range params
    start = end;
    end += iter_pr_subtask + ((subtask_id + 1) < remainder);
  }

  // Join wait for subtasks to finish
  while(shared_counter != 0 && scheduler_error == 0) {
    if (!empty(&worker->q)) {
      struct subtask * subtask = pop_back(&worker->q);
      if (subtask == NULL) continue;
      struct subtask* subtask_new = chunk_subtask(worker, subtask);
      CHECK_ERR(run_subtask(worker, subtask_new), "run_subtask");
    } else {
      while (shared_counter != 0 && empty(&worker->q) && scheduler_error == 0) {
        steal_from_random_worker(worker);
      }
    }
  }

  __atomic_sub_fetch(&active_work, 1, __ATOMIC_RELAXED);
  // Write back timing results
  (*timer) += task_timer;

  return scheduler_error;
}

#endif


static inline int scheduler_execute_task(struct scheduler *scheduler,
                                         struct scheduler_parloop *task)
{

  struct worker *worker = worker_local;

  int err = 0;
  if (task->iterations == 0) {
    return err;
  }

  int64_t task_timer = 0;
  /* Execute task sequential or parallel based on decision made earlier */
  if (task->info.nsubtasks == 1) {
    /* int64_t start = get_wall_time_ns(); */
    err = task->fn(task->args, 0, task->iterations, 0, worker->tid);
    /* int64_t end = get_wall_time_ns(); */
    /* task_timer = end - start; */
    /* worker->time_spent_working += task_timer; */
    // Report time measurements
    // TODO the update of both of these should really both be atomic!!
    /* __atomic_fetch_add(task->info.total_time, task_timer, __ATOMIC_RELAXED); */
    /* __atomic_fetch_add(task->info.total_iter, task->iterations, __ATOMIC_RELAXED); */
  }
  else
  {
    // Add "before" time if we already are inside a task
    int64_t time_before = 0;
    if (worker->nested > 0) {
      time_before = total_now(worker->total, worker->timer);
    }

    err = scheduler_execute_parloop(scheduler, task, &task_timer);


    // Report time measurements
    // TODO the update of both of these should really both be atomic!!
    __atomic_fetch_add(task->info.task_time, task_timer, __ATOMIC_RELAXED);
    __atomic_fetch_add(task->info.task_iter, task->iterations, __ATOMIC_RELAXED);

    // Reset timers to account for new timings
    worker->total = time_before + task_timer;
    worker->timer = get_wall_time_ns();
  }


  return err;
}

/* Decide on how schedule the incoming task i.e. how many subtasks and
   to run sequential or (potentially nested) parallel code body */
static inline int scheduler_prepare_task(struct scheduler* scheduler,
                                         struct scheduler_task *task)
{
  assert(task != NULL);

  struct worker *worker = worker_local;
  struct scheduler_info info;
  info.task_time = task->task_time;
  info.task_iter = task->task_iter;

  int nsubtasks;
  // Decide if task should be scheduled sequentially
  if (is_small(task, scheduler->num_threads, &nsubtasks)) {
    info.iter_pr_subtask = task->iterations;
    info.remainder = 0;
    info.nsubtasks = nsubtasks;
    return task->sequential_fn(task->args, task->iterations, worker->tid, info);
  } else {
    info.iter_pr_subtask = task->iterations / nsubtasks;
    info.remainder = task->iterations % nsubtasks;
    info.sched = task->sched;
    switch (task->sched) {
    case STATIC:
      info.nsubtasks = info.iter_pr_subtask == 0 ? info.remainder : ((task->iterations - info.remainder) / info.iter_pr_subtask);
      break;
    case DYNAMIC:
      // As any thread can take any subtasks, we are being safe with using
      // an upper bound on the number of tasks such that the task allocate enough memory
      info.nsubtasks = info.iter_pr_subtask == 0 ? info.remainder : nsubtasks;
      break;
    default:
      assert(!"Got unknown scheduling");
    }

  }

  info.wake_up_threads = 0;
  // We use the nested parallel task function is we can't exchaust all cores
  // using the outer most level
  if (task->canonical_fn != NULL && (info.nsubtasks + active_work) < scheduler->num_threads) {
    if (worker->nested == 0)
      info.wake_up_threads = 1;
    return task->canonical_fn(task->args, task->iterations, worker->tid, info);
  }

  return task->sequential_fn(task->args, task->iterations, worker->tid, info);
}

#endif
// End of scheduler.h
