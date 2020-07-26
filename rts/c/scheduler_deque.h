// start of scheduler.h
#ifndef _SCHEDULER_H_
#define _SCHEDULER_H_


static sig_atomic_t num_workers;

__thread struct worker* worker_local = NULL;

static int should_exit = 0;

static inline int is_finished() {
  return should_exit && empty(&worker_local->q);
}

int random_other_worker(struct scheduler *scheduler, int my_id) {
  /* int nb_workers = data::perworker::get_nb_workers(); */
  /* assert(nb_workers != 1); */
  /* std::uniform_int_distribution<int> distribution(0, nb_workers - 2); */
  /* int i = distribution(random_number_generators[my_id]); */

  int i = rand() % (scheduler->num_threads - 1);
  if (i == my_id) {
    i++;
  }
  assert(i >= 0);
  /* assert(i < nb_workers); */
  assert(i != my_id);
  return i;
}

void acquire (struct scheduler* scheduler)
{
  /* assert(my_ready.empty() && my_suspended.empty() && my_buffer.empty()); */
  /* assert(data::perworker::get_nb_workers() >= 2); */

  int my_id = worker_local->tid;
  while (! is_finished()) {
    int k = random_other_worker(scheduler, my_id);
    struct deque *deque_k = &scheduler->workers[k].q;
    struct subtask* subtask = steal(deque_k);
    if (subtask == STEAL_RES_EMPTY) {
      // later: log
    } else if (subtask == STEAL_RES_ABORT) {
      // later: log
    } else {
      assert(subtask != NULL);
      subtask->been_stolen = 1;
      pushBottom(&worker_local->q, subtask);
      fprintf(stderr, "tid %d stole a task from %d with id %d and %p \n", my_id, k, subtask->id, subtask);

      return;
    }
  }
}

static inline void *scheduler_worker(void* arg)
{
  struct worker *worker = (struct worker*) arg;
  worker_local = worker;
  while (!is_finished())
  {
    if (! empty(&worker->q)) {
      struct subtask* subtask = popBottom(&worker->q);
      if (subtask == NULL) {
        continue;
      }

      fprintf(stderr, "tid %d running subtask %p with id %d\n", worker_local->tid, subtask, subtask->id);
      int err = subtask->fn(subtask->args, subtask->start, subtask->end, subtask->id);
      CHECK_ERR(pthread_mutex_lock(subtask->mutex), "pthread_mutex_lock");
      /* Only one error can be returned at the time now
         Maybe we can provide a stack like structure for pushing errors onto
         if we wish to backpropagte multiple errors */
      if (err != 0) {
        scheduler_error = err;
      }
      (*subtask->counter)--;
      if (*subtask->counter == 0)
        CHECK_ERR(pthread_cond_broadcast(subtask->cond), "pthread_cond_broadcast");
      CHECK_ERR(pthread_mutex_unlock(subtask->mutex), "pthread_mutex_unlock");
      free(subtask);
    } else { // steal
      acquire(worker_local->scheduler);
    }
  }
  assert(empty(&worker->q));
  num_workers--;
  return NULL;
}


static inline int scheduler_parallel(struct scheduler *scheduler,
                                     struct scheduler_subtask *task,
                                     int *ntask)
{
#ifdef MCDEBUG
  fprintf(stderr, "[scheduler_parallel] Performing scheduling with granularity %d\n", task->granularity);
#endif

  pthread_mutex_t mutex;
  CHECK_ERR(pthread_mutex_init(&mutex, NULL), "pthread_mutex_init");
  pthread_cond_t cond;
  CHECK_ERR(pthread_cond_init(&cond, NULL), "pthread_cond_init");

  int max_num_tasks = scheduler->num_threads;
  int iter_pr_subtask = task->iterations / max_num_tasks;
  int remainder = task->iterations % max_num_tasks;

  int nsubtasks = iter_pr_subtask == 0 ? remainder : ((task->iterations - remainder) / iter_pr_subtask);

  int shared_counter = nsubtasks;

  /* Each subtasks is processed in chunks */
  int chunks = 0;
  if (task->granularity > 0) {
    chunks = iter_pr_subtask / task->granularity == 0 ? 1 : iter_pr_subtask / task->granularity;
  }

  int start = 0;
  int subtask_id = 0;
  int end = iter_pr_subtask + (int)(remainder != 0);
  for (subtask_id = 0; subtask_id < nsubtasks; subtask_id++) {
    struct subtask *subtask = setup_subtask(task->fn, task->args,
                                            &mutex, &cond, &shared_counter,
                                            start, end, chunks, subtask_id);
    assert(subtask != NULL);
    pushBottom(&scheduler->workers[worker_local->tid].q, subtask);
#ifdef MCDEBUG
    fprintf(stderr, "[scheduler_task] pushed %d iterations onto %d's q\n", (end - start), worker_local->tid);
#endif
    // Update range params
    start = end;
    end += iter_pr_subtask + ((subtask_id + 1) < remainder);
  }

  while(1) {
    CHECK_ERR(pthread_mutex_lock(&mutex), "pthread_mutex_lock");
    if (shared_counter == 0) break;
    CHECK_ERR(pthread_mutex_unlock(&mutex), "pthread_mutex_unlock");

    struct subtask * subtask = popBottom(&worker_local->q);
    if (subtask != NULL) {
      // Do work
      assert(subtask->fn != NULL);
      assert(subtask->args != NULL);
      int err = subtask->fn(subtask->args, subtask->start, subtask->end, subtask->id);
      if (err != 0) {
        return err;
      }
      CHECK_ERR(pthread_mutex_lock(subtask->mutex), "pthread_mutex_lock");
      (*subtask->counter)--;
      CHECK_ERR(pthread_mutex_unlock(subtask->mutex), "pthread_mutex_unlock");
      free(subtask);
    }
  }

  // As any thread can take any subtasks
  // we are being safe with returning
  // an upper bound on the number of tasks
  if (ntask != NULL) {
    *ntask = (task->granularity > 0) ? scheduler->num_threads : nsubtasks;
  }

  return scheduler_error;
}



static inline int scheduler_execute(struct scheduler *scheduler,
                                    struct scheduler_subtask *task,
                                    int *ntask)
{
#ifdef MCDEBUG
  fprintf(stderr, "[scheduler_execute] starting task %s with %ld iterations \n", task->name, task->iterations);
#endif
  assert(scheduler != NULL);
  assert(task != NULL);

  if (task->iterations == 0) {
    if (ntask != NULL) *ntask = 0;
    return 0;
  }

  return scheduler_parallel(scheduler, task, ntask);
}



/* Decide whether to run sequential  or (potentially nested) parallel code body */
static inline int scheduler_do_task(struct scheduler* scheduler,
                                    struct scheduler_task *task)
{
  assert(task != NULL);
#ifdef MCDEBUG
  fprintf(stderr, "[scheduler_do_task] starting task with %ld iterations\n", task->iterations);
#endif

  return task->seq_fn(task->args, task->iterations, (worker_local == NULL) ? 0 : worker_local->tid);
}

#endif
// End of scheduler.h
