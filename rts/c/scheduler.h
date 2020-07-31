// start of scheduler.h

#ifndef _SCHEDULER_H_
#define _SCHEDULER_H_

static int free_workers;

__thread struct worker* worker_local = NULL;

static inline void *scheduler_worker(void* arg)
{
  struct worker *worker = (struct worker*) arg;
  worker_local = worker;
  struct subtask *subtask = NULL;
  while(1) {
    if (subtask_queue_dequeue(worker, &subtask) == 0) {
      assert(subtask->fn != NULL);
      assert(subtask->args != NULL);
#ifdef MCPROFILE
      int64_t start = get_wall_time();
#endif
      /* fprintf(stderr, "iterations %ld\n", subtask->end - subtask->start); */
      int err = subtask->fn(subtask->args, subtask->start, subtask->end, subtask->id);
#ifdef MCPROFILE
      int64_t end = get_wall_time();
      worker->time_spent_working += end - start;
#endif
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
      subtask = NULL;
    } else {
#ifdef MCPROFILE
#endif
      output_thread_usage(worker);
      break;
    }
  }
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


  struct scheduler_info info = task->info;
  int iter_pr_subtask = info.iter_pr_subtask;
  int remainder = info.remainder;
  int nsubtasks = info.nsubtasks;
  free_workers -= nsubtasks;

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
    struct subtask *subtask = setup_subtask(task->fn, task->args, task->name,
                                            &mutex, &cond, &shared_counter,
                                            start, end, chunks, subtask_id);
    assert(subtask != NULL);
    CHECK_ERR(subtask_queue_enqueue(&scheduler->workers[subtask_id%scheduler->num_threads], subtask),
              "subtask_queue_enqueue");
#ifdef MCDEBUG
    fprintf(stderr, "[scheduler_task] pushed %d iterations onto %d's q\n", (end - start), subtask_id%scheduler->num_threads);
#endif
    // Update range params
    start = end;
    end += iter_pr_subtask + ((subtask_id + 1) < remainder);
  }

  // Join (wait for subtasks to finish)
  CHECK_ERR(pthread_mutex_lock(&mutex), "pthread_mutex_lock");
  while (shared_counter != 0) {
    CHECK_ERR(pthread_cond_wait(&cond, &mutex), "pthread_cond_wait");
  }

  // As any thread can take any subtasks
  // we are being safe with returning
  // an upper bound on the number of tasks
  if (ntask != NULL) {
    *ntask = (task->granularity > 0) ? scheduler->num_threads : nsubtasks;
  }

  free_workers += nsubtasks;
  return scheduler_error;
}



static inline int scheduler_nested(struct scheduler *scheduler,
                                   struct scheduler_subtask* task,
                                   int *ntask,
                                   struct worker* calling_worker)
{
#ifdef MCDEBUG
  fprintf(stderr, "[scheduler_nested] tid %d\n", calling_worker->tid);
  fprintf(stderr, "[scheduler_nested] granularity %d\n", task->granularity);
  fprintf(stderr, "[scheduler_nested] iterations %ld\n", task->iterations);
  fprintf(stderr, "[scheduler_nested] free workers %d\n", free_workers);
#endif

  pthread_mutex_t mutex;
  CHECK_ERR(pthread_mutex_init(&mutex, NULL), "pthread_mutex_init");
  pthread_cond_t cond;
  CHECK_ERR(pthread_cond_init(&cond, NULL), "pthread_cond_init");

  struct scheduler_info info = task->info;
  int iter_pr_subtask = info.iter_pr_subtask;
  int remainder = info.remainder;
  int nsubtasks = info.nsubtasks;



  int shared_counter = info.nsubtasks;

  int chunks = 0;
  if (task->granularity > 0) {
    chunks = iter_pr_subtask / task->granularity == 0 ? 1 : iter_pr_subtask / task->granularity;
  }

  long int start = 0;
  long int subtask_id = 0;
  int end = iter_pr_subtask + (long int)(remainder != 0);
  for (subtask_id = 0; subtask_id < nsubtasks; subtask_id++) {
    struct subtask *subtask = setup_subtask(task->fn, task->args, task->name,
                                            &mutex, &cond, &shared_counter,
                                            start, end, chunks, subtask_id);
    assert(subtask != NULL);
    CHECK_ERR(subtask_queue_enqueue(calling_worker, subtask), "subtask_queue_enqueue");
#ifdef MCDEBUG
    fprintf(stderr, "[scheduler_nested] pushed %ld iterations onto q %d\n", (end - start), calling_worker->tid);
#endif
    // Update range params
    start = end;
    end += info.iter_pr_subtask + ((subtask_id + 1) < info.remainder);
  }


  struct subtask *subtask = NULL;
  int done = 0;
  while(!done) {
    CHECK_ERR(pthread_mutex_lock(&mutex), "pthread_mutex_lock");
    if (shared_counter == 0) done = 1;
    CHECK_ERR(pthread_mutex_unlock(&mutex), "pthread_mutex_unlock");

    if (calling_worker->q.num_used != 0) {
      int err = subtask_queue_steal(calling_worker, &subtask);
      if (err == 0) {
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
      } else if (err == 1){
        continue;
      } else {
        fprintf(stderr, "[scheduler_nested] Got error %d from \n", err);
        assert(0);
      }
    /* } else { */
    /*   int retval = query_a_subtask(scheduler, calling_worker->tid, calling_worker, &subtask); */

    /*   if (retval == 0) { */
    /*     assert(subtask->fn != NULL); */
    /*     assert(subtask->args != NULL); */
    /*     int err = subtask->fn(subtask->args, subtask->start, subtask->end, subtask->id); */
    /*     if (err != 0) { */
    /*       return err; */
    /*     } */
    /*     CHECK_ERR(pthread_mutex_lock(subtask->mutex), "pthread_mutex_lock"); */
    /*     (*subtask->counter)--; */
    /*     CHECK_ERR(pthread_mutex_unlock(subtask->mutex), "pthread_mutex_unlock"); */
    /*     free(subtask); */
    /*   } else if (retval > 1) { */
    /*     CHECK_ERR(retval, "query_a_subtask"); */
    /*   } */
    /*   // We just stole a subtask, run it before we finish */
    }
  }

  if (ntask != NULL) {
    *ntask = (task->granularity > 0) ? scheduler->num_threads : info.nsubtasks;
  }
  return 0;
}


static inline int do_task_directly(sub_task_fn fn,
                                   void *args,
                                   int iterations)
{
#ifdef MCDEBUG
  fprintf(stderr, "[do_task_directly] doing task directly\n");
#endif
  assert(fn != NULL);
  return fn(args, 0, iterations, 0); // (worker_local == NULL) ? 0 : worker_local->tid);
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

  /* Run task directly if all other workers are occupied */
  if (!free_workers) {
    *ntask = 1;
    return do_task_directly(task->fn, task->args, task->iterations);
  }


  /* This case indicates that we are inside a nested parallel operation */
  /* so we handle this differently */
  if (worker_local != NULL) {
    CHECK_ERR(scheduler_nested(scheduler, task, ntask, worker_local), "scheduler_nested");
    return 0;
  }

  return scheduler_parallel(scheduler, task, ntask);
}

#endif


/* Decide whether to run sequential  or (potentially nested) parallel code body */
static inline int scheduler_do_task(struct scheduler* scheduler,
                                    struct scheduler_task *task)
{
  assert(task != NULL);
#ifdef MCDEBUG
  fprintf(stderr, "[scheduler_do_task] starting task with %ld iterations\n", task->iterations);
#endif

  struct scheduler_info info;

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
    info.nsubtasks = max_num_tasks;
    break;
  default:
    assert(!"Got unknown scheduling");
  }

  return task->seq_fn(task->args, task->iterations, (worker_local == NULL) ? 0 : worker_local->tid, info);
}


// End of scheduler.h
