// start of scheduler.h
#ifndef _SCHEDULER_H_
#define _SCHEDULER_H_


static int dummy_counter = 0;
static int64_t dummy_timer = 0;
static int64_t dummy_iter = 0;

static int dummy_fn(void *args, int64_t start, int64_t end, int subtask_id, int tid) {
  return 0;
}

// Wake up threads, who are blocking by pushing a dummy task
// onto their queue
static inline void wake_up_threads(struct scheduler *scheduler, int start_tid, int end_tid) {

#if defined(MCDEBUG)
  assert(start_tid >= 1);
  assert(end_tid <= scheduler->num_threads);
#endif
  for (int i = start_tid; i < end_tid; i++) {
    struct subtask *subtask = create_subtask(dummy_fn, NULL, "dummy_fn",
                                            &dummy_counter,
                                            &dummy_timer, &dummy_iter,
                                            0, 0,
                                            0, 0,
                                            0);
    CHECK_ERR(subtask_queue_enqueue(&scheduler->workers[i], subtask), "subtask_queue_enqueue");
  }
}

static inline int is_finished(struct worker *worker) {
  return worker->dead && subtask_queue_is_empty(&worker->q);
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


static inline void *scheduler_worker(void* args)
{
  struct worker *worker = (struct worker*) args;
  worker_local = worker;
  struct subtask * subtask = NULL;
  while(!is_finished(worker)) {
    if (!subtask_queue_is_empty(&worker->q)) {
      int retval = subtask_queue_dequeue(worker, &subtask, 0);
      if (retval == 0) {
        assert(subtask != NULL);
        CHECK_ERR(run_subtask(worker, subtask), "run_subtask");
      } // else someone stole our work

    } else if (active_work) { /* steal */
      while (!is_finished(worker) && active_work) {
        if (steal_from_random_worker(worker)) {
          break;
        }
      }
    } else { /* go back to sleep and wait for work */
      int retval = subtask_queue_dequeue(worker, &subtask, 1);
      if (retval == 0) {
        assert(subtask != NULL);
        CHECK_ERR(run_subtask(worker, subtask), "run_subtask");
      }
    }
  }

  assert(subtask_queue_is_empty(&worker->q));
  __atomic_fetch_sub(&num_workers, 1, __ATOMIC_RELAXED);
#if defined(MCPROFILE)
  if (worker->output_usage)
    output_thread_usage(worker);
#endif
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
  volatile int join_counter = nsubtasks;

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

  int64_t start = 0;
  int64_t end = iter_pr_subtask + (int64_t)(remainder != 0);
  for (int subtask_id = 0; subtask_id < nsubtasks; subtask_id++) {
    struct subtask *subtask = create_subtask(task->fn, task->args, task->name,
                                              &join_counter,
                                              &task_timer, &task_iter,
                                              start, end,
                                              chunkable, chunk_size,
                                              subtask_id);
    assert(subtask != NULL);
    if (worker->nested){
      CHECK_ERR(subtask_queue_enqueue(&scheduler->workers[worker->tid], subtask),
                "subtask_queue_enqueue");
    } else {
      CHECK_ERR(subtask_queue_enqueue(&scheduler->workers[subtask_id], subtask),
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
  while(join_counter != 0) {
    if (!subtask_queue_is_empty(&worker->q)) {
      struct subtask *subtask = NULL;
      int err = subtask_queue_dequeue(worker, &subtask, 0);
      if (err == 0 ) {
        CHECK_ERR(run_subtask(worker, subtask), "run_subtask");
      }
    } else {
      if (steal_from_random_worker(worker)) {
        struct subtask *subtask = NULL;
        int err = subtask_queue_dequeue(worker, &subtask, 0);
        if (err == 0) {
          CHECK_ERR(run_subtask(worker, subtask), "run_subtask");
        }
      }
    }
  }


  if (info.wake_up_threads || sched == DYNAMIC) {
    __atomic_sub_fetch(&active_work, nsubtasks, __ATOMIC_RELAXED);
  }

  // Write back timing results of all sequential work
  (*timer) += task_timer;
  return scheduler_error;
}


static inline int scheduler_execute_task(struct scheduler *scheduler,
                                         struct scheduler_parloop *task)
{

  struct worker *worker = worker_local;

  int err = 0;
  if (task->iterations == 0) {
    return err;
  }

  // How much sequential work was performed by the task
  int64_t task_timer = 0;

  /* Execute task sequential or parallel based on decision made earlier */
  if (task->info.nsubtasks == 1) {
    int64_t start = get_wall_time_ns();
    err = task->fn(task->args, 0, task->iterations, 0, worker->tid);
    int64_t end = get_wall_time_ns();
    task_timer = end - start;
    worker->time_spent_working += task_timer;
    // Report time measurements
    // TODO the update of both of these should really be a single atomic!!
    __atomic_fetch_add(task->info.task_time, task_timer, __ATOMIC_RELAXED);
    __atomic_fetch_add(task->info.task_iter, task->iterations, __ATOMIC_RELAXED);
  } else {
    // Add "before" time if we already are inside a task
    int64_t time_before = 0;
    if (worker->nested > 0) {
      time_before = total_now(worker->total, worker->timer);
    }

    err = scheduler_execute_parloop(scheduler, task, &task_timer);

    // Report time measurements
    // TODO the update of both of these should really be a single atomic!!
    __atomic_fetch_add(task->info.task_time, task_timer, __ATOMIC_RELAXED);
    __atomic_fetch_add(task->info.task_iter, task->iterations, __ATOMIC_RELAXED);

    // Update timers to account for new timings
    worker->total = time_before + task_timer;
    worker->timer = get_wall_time_ns();
  }


  return err;
}

/* Decide on how schedule the incoming task i.e. how many subtasks and
   to run sequential or (potentially nested) parallel code body */
static inline int scheduler_prepare_task(struct scheduler* scheduler,
                                         struct scheduler_segop *task)
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
    return task->top_level_fn(task->args, task->iterations, worker->tid, info);
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
  // We only use the nested parallel segop function if we can't exchaust all cores
  // using the outer most level
  if (task->nested_fn != NULL && info.nsubtasks < scheduler->num_threads && info.nsubtasks == task->iterations) {
    if (worker->nested == 0)
      info.wake_up_threads = 1;
    return task->nested_fn(task->args, task->iterations, worker->tid, info);
  }

  return task->top_level_fn(task->args, task->iterations, worker->tid, info);
}

#endif
// End of scheduler.h
