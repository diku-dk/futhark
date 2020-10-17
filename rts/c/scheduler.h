// start of scheduler.h

#include <float.h>

/* Scheduler definitions */
enum scheduling {
  DYNAMIC,
  STATIC
};

/* How a given task should be executed */
/* Filled out by the scheduler
   and passed to the segop function
*/
struct scheduler_info {
  int64_t iter_pr_subtask;
  int64_t remainder;
  int nsubtasks;
  enum scheduling sched;
  int wake_up_threads;

  int64_t *task_time;
  int64_t *task_iter;
};

static const double kappa_default = 5.1f * 1000;

struct scheduler {
  struct worker *workers;
  int num_threads;

  // kappa time unit in nanoseconds
  double kappa;
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
struct scheduler_segop {
  void *args;
  segop_fn top_level_fn;
  segop_fn nested_fn;
  int64_t iterations;
  enum scheduling sched;

  // Pointers to timer and iter associated with the task
  int64_t *task_time;
  int64_t *task_iter;

  // For debugging
  const char* name;
};

// If there is work to steal => active_work > 0
static volatile int active_work = 0;
// Number of alive workers
static volatile sig_atomic_t num_workers;

// Thread local variable worker struct
// Note that, accesses to tls variables are expensive
// Minimize direct references to this variable
__thread struct worker* worker_local = NULL;

/* Only one error can be returned at the time now
   Maybe we can provide a stack like structure for pushing errors onto
   if we wish to backpropagte multiple errors */
static volatile sig_atomic_t scheduler_error = 0;

int64_t total_now(int64_t total, int64_t time) {
  return total + (get_wall_time_ns() - time);
}

int random_other_worker(struct scheduler *scheduler, int my_id) {
  (void)scheduler;
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


static inline int64_t compute_chunk_size(double kappa, struct subtask* subtask)
{
  double C = (double)*subtask->task_time / (double)*subtask->task_iter;
  if (C == 0.0F) C += DBL_EPSILON;
  return max_int64((int64_t)(kappa / C), 1);
}

/* Takes a chunk from subtask and enqueues the remaining iterations onto the worker's queue */
/* A no-op if the subtask is not chunkable */
static inline struct subtask* chunk_subtask(struct worker* worker, struct subtask *subtask)
{
  if (subtask->chunkable) {
    // Do we have information from previous runs avaliable
    if (*subtask->task_iter > 0) {
      subtask->chunk_size = compute_chunk_size(worker->scheduler->kappa, subtask);
      assert(subtask->chunk_size > 0);
    }
    int64_t remaining_iter = subtask->end - subtask->start;
    assert(remaining_iter > 0);
    if (remaining_iter > subtask->chunk_size) {
      struct subtask *new_subtask = malloc(sizeof(struct subtask));
      *new_subtask = *subtask;
      // increment the subtask join counter to account for new subtask
      __atomic_fetch_add(subtask->counter, 1, __ATOMIC_RELAXED);
      // Update range parameters
      subtask->end = subtask->start + subtask->chunk_size;
      new_subtask->start = subtask->end;
      subtask_queue_enqueue(worker, new_subtask);
    }
  }
  return subtask;
}

static inline int run_subtask(struct worker* worker, struct subtask* subtask)
{
  assert(subtask != NULL);
  assert(worker != NULL);

  subtask = chunk_subtask(worker, subtask);
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
    // Even a failed task counts as finished.
    __atomic_fetch_sub(subtask->counter, 1, __ATOMIC_RELAXED);
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
  // These updates should really be done using a single atomic CAS operation
  __atomic_fetch_add(subtask->task_time, time_elapsed, __ATOMIC_RELAXED);
  __atomic_fetch_add(subtask->task_iter, iter, __ATOMIC_RELAXED);
  // We need a fence here, since if the counter is decremented before either
  // of the two above are updated bad things can happen, e.g. if they are stack-allocated
  __atomic_thread_fence(__ATOMIC_SEQ_CST);
  __atomic_fetch_sub(subtask->counter, 1, __ATOMIC_RELAXED);
  free(subtask);
  return 0;
}


static inline int is_small(struct scheduler_segop *task, struct scheduler *scheduler, int *nsubtasks)
{
  int64_t time = *task->task_time;
  int64_t iter = *task->task_iter;

  if (task->sched == DYNAMIC || iter == 0) {
    *nsubtasks = scheduler->num_threads;
    return 0;
  }

  // Estimate the constant C
  double C = (double)time / (double)iter;
  double cur_task_iter = (double) task->iterations;

  // Returns true if the task is small i.e.
  // if the number of iterations times C is smaller
  // than the overhead of subtask creation
  if (C == 0.0F || C * cur_task_iter < scheduler->kappa) {
    *nsubtasks = 1;
    return 1;
  }

  // Else compute how many subtasks this tasks should create
  int64_t min_iter_pr_subtask = max_int64(scheduler->kappa / C, 1);
  *nsubtasks = min_int64(max_int64(task->iterations / min_iter_pr_subtask, 1), scheduler->num_threads);

  return 0;
}

// TODO make this prettier
static inline struct subtask* create_subtask(parloop_fn fn,
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
    assert(!"malloc failed in create_subtask");
    return NULL;
  }
  subtask->fn         = fn;
  subtask->args       = args;

  subtask->counter    = counter;
  subtask->task_time  = timer;
  subtask->task_iter  = iter;

  subtask->start      = start;
  subtask->end        = end;
  subtask->id         = id;
  subtask->chunkable  = chunkable;
  subtask->chunk_size = chunk_size;

  subtask->name       = name;
  return subtask;
}

static int dummy_counter = 0;
static int64_t dummy_timer = 0;
static int64_t dummy_iter = 0;

static int dummy_fn(void *args, int64_t start, int64_t end, int subtask_id, int tid) {
  (void)args;
  (void)start;
  (void)end;
  (void)subtask_id;
  (void)tid;
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
  if (is_small(task, scheduler, &nsubtasks)) {
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

// End of scheduler.h
