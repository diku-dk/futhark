// start of scheduler.h

#ifndef _SCHEDULER_H_
#define _SCHEDULER_H_

#define MULTICORE
/* #define MCDEBUG */
#define MCPROFILE

#ifdef _WIN32
#include <windows.h>
#elif __APPLE__
#include <sys/sysctl.h>
// For getting cpu usage of threads
#include <mach/mach.h>
#include <sys/resource.h>
#else // Linux
#include <sys/sysinfo.h>
#include <sys/resource.h>
#endif

#ifdef MCDEBUG
static long int ran_iter, start_iter = 0;
#endif

static int scheduler_error = 0;


/* A wrapper for getting rusage on Linux and MacOS */
/* TODO maybe figure out this for windows */
static inline int getrusage_thread(struct rusage *rusage)
{
  int err = -1;
#ifdef __APPLE__
    thread_basic_info_data_t info = { 0 };
    mach_msg_type_number_t info_count = THREAD_BASIC_INFO_COUNT;
    kern_return_t kern_err;

    kern_err = thread_info(mach_thread_self(),
                           THREAD_BASIC_INFO,
                           (thread_info_t)&info,
                           &info_count);
    if (kern_err == KERN_SUCCESS) {
        memset(rusage, 0, sizeof(struct rusage));
        rusage->ru_utime.tv_sec = info.user_time.seconds;
        rusage->ru_utime.tv_usec = info.user_time.microseconds;
        rusage->ru_stime.tv_sec = info.system_time.seconds;
        rusage->ru_stime.tv_usec = info.system_time.microseconds;
        err = 0;
    } else {
        errno = EINVAL;
    }
#else // Linux
    err = getrusage(RUSAGE_THREAD, rusage);
#endif
    return err;
}

static inline void output_thread_usage(struct worker *worker)
{
  struct rusage usage;
  CHECK_ERRNO(getrusage_thread(&usage), "getrusage_thread");
  struct timeval user_cpu_time = usage.ru_utime;
  struct timeval sys_cpu_time = usage.ru_stime;
  fprintf(stderr, "tid: %2d - work time %llu - user time: %llu us - sys: %10llu us - dequeue: (%8llu/%8llu) = %llu - enqueue: (%llu/%llu) = %llu \n",
          worker->tid,
          worker->time_spent_working,
          (uint64_t)(user_cpu_time.tv_sec * 1000000 + user_cpu_time.tv_usec),
          (uint64_t)(sys_cpu_time.tv_sec * 1000000 + sys_cpu_time.tv_usec),
          worker->q.time_dequeue,
          worker->q.n_dequeues,
          worker->q.time_dequeue / (worker->q.n_dequeues == 0 ? 1 : worker->q.n_dequeues),
          worker->q.time_enqueue,
          worker->q.n_dequeues,
          worker->q.time_enqueue / (worker->q.n_enqueues == 0 ? 1 : worker->q.n_enqueues));
}

/* returns the number of logical cores */
static int num_processors()
{
#ifdef _WIN32
/* https://docs.microsoft.com/en-us/windows/win32/api/sysinfoapi/ns-sysinfoapi-system_info */
    SYSTEM_INFO sysinfo;
    GetSystemInfo(&sysinfo);
    int ncores = sysinfo.dwNumberOfProcessors;
    fprintf(stdout, "Found %d cores on your Windows machine\n Is that correct?\n", ncores);
    return ncores;
#elif __APPLE__
    int ncores;
    size_t ncores_size = sizeof(ncores);
    CHECK_ERRNO(sysctlbyname("hw.logicalcpu", &ncores, &ncores_size, NULL, 0),
                "sysctlbyname (hw.logicalcpu)");
    return ncores;
#else // If Linux
  return get_nprocs();
#endif
}



__thread struct worker* worker_local = NULL;

static inline void *scheduler_worker(void* arg)
{
  struct worker *worker = (struct worker*) arg;
  worker_local = worker;
  struct subtask *subtask = NULL;
  while(1) {
    if (subtask_queue_dequeue(worker, &subtask) == 0) {
      assert(subtask->par_fn != NULL);
      assert(subtask->args != NULL);
#ifdef MCPROFILE
      int64_t start = get_wall_time();
#endif
      int err = subtask->par_fn(subtask->args, subtask->start, subtask->end, worker->tid);
#ifdef MCPROFILE
      int64_t end = get_wall_time();
      worker->time_spent_working += end - start;
#endif

      CHECK_ERR(pthread_mutex_lock(subtask->mutex), "pthread_mutex_lock");
#ifdef MCDEBUG
      ran_iter += (subtask->end-subtask->start);
#endif
      // Only one error can be returned at the time now
      // Maybe we can provide a stack like structure for pushing errors onto
      // if we wish to backpropagte multiple errors
      if (err != 0) {
        scheduler_error = err;
      }
      (*subtask->counter)--;
      CHECK_ERR(pthread_cond_broadcast(subtask->cond), "pthread_cond_broadcast");
      CHECK_ERR(pthread_mutex_unlock(subtask->mutex), "pthread_mutex_unlock");
      free(subtask);
      subtask = NULL;
    } else {
#ifdef MCPROFILE
      output_thread_usage(worker);
#endif
      break;
    }
  }

  return NULL;
}


static inline int scheduler_dynamic(struct scheduler *scheduler,
                                    struct scheduler_task *task,
                                    int *ntask)
{
#ifdef MCDEBUG
  fprintf(stderr, "[scheduler_dynamic] Performing dynamic scheduling\n");
#endif

  pthread_mutex_t mutex;
  CHECK_ERR(pthread_mutex_init(&mutex, NULL), "pthread_mutex_init");
  pthread_cond_t cond;
  CHECK_ERR(pthread_cond_init(&cond, NULL), "pthread_cond_init");

  int max_num_tasks = scheduler->num_threads;

  int subtask_id = 0;
  int iter_pr_subtask = task->iterations / max_num_tasks;
  int remainder = task->iterations % max_num_tasks;

  int nsubtasks = iter_pr_subtask == 0 ? remainder : ((task->iterations - remainder) / iter_pr_subtask);
  int shared_counter = nsubtasks;

  /* Each subtasks is processed in chunks */
  int chunks = iter_pr_subtask / task->granularity == 0 ? 1 : iter_pr_subtask / task->granularity;

  int start = 0;
  int end = iter_pr_subtask + (int)(remainder != 0);
  for (int subtask_id = 0; subtask_id < nsubtasks; subtask_id++) {
    struct subtask *subtask = setup_subtask(task->par_fn, task->args,
                                            &mutex, &cond, &shared_counter,
                                            start, end, chunks);
    assert(subtask != NULL);
    CHECK_ERR(subtask_queue_enqueue(&scheduler->workers[subtask_id%scheduler->num_threads], subtask),
              "subtask_queue_enqueue");
#ifdef MCDEBUG
    fprintf(stderr, "[scheduler_dynamic] pushed %d iterations onto %d's q\n", (end - start), subtask_id%scheduler->num_threads);

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
    *ntask = scheduler->num_threads;
  }

  return scheduler_error;
}



/* Performs static scheduling of a task */
/* Divides the number of iterations into num_threads equal sized chunks */
/* Any remainders is divided evenly out among threads  */
static inline int scheduler_static(struct scheduler *scheduler,
                                   struct scheduler_task *task,
                                   int *ntask)
{
#ifdef MCDEBUG
  fprintf(stderr, "[scheduler_static] Performing static scheduling\n");
#endif

  pthread_mutex_t mutex;
  CHECK_ERR(pthread_mutex_init(&mutex, NULL), "pthread_mutex_init");
  pthread_cond_t cond;
  CHECK_ERR(pthread_cond_init(&cond, NULL), "pthread_cond_init");

  int subtask_id = 0;
  int iter_pr_subtask = task->iterations / scheduler->num_threads;
  int remainder = task->iterations % scheduler->num_threads;


  int nsubtasks = iter_pr_subtask == 0 ? remainder : ((task->iterations - remainder) / iter_pr_subtask);
  int shared_counter = nsubtasks;

  int start = 0;
  int end = iter_pr_subtask + (int)(remainder != 0);

#ifdef MCDEBUG
  fprintf(stderr, "[scheduler_static] nsubtasks %d - remainder %d - iter_pr_subtask %d", nsubtasks, remainder, iter_pr_subtask);
#endif

  for (int subtask_id = 0; subtask_id < nsubtasks; subtask_id++) {
    struct subtask *subtask = setup_subtask(task->par_fn, task->args,
                                            &mutex, &cond, &shared_counter,
                                            start, end, 0);
    assert(subtask != NULL);
    CHECK_ERR(subtask_queue_enqueue(&scheduler->workers[subtask_id], subtask),
              "subtask_queue_enqueue");
#ifdef MCDEBUG
    fprintf(stderr, "[scheduler_static] iter_pr_subtask %d - exp %d\n",
            iter_pr_subtask, subtask_id < remainder);
    fprintf(stderr, "[scheduler_static] pushed start %d - end %d (%d) iterations onto %d's q\n",
            start, end, end-start, subtask_id%scheduler->num_threads);
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

  if (ntask != NULL) {
    *ntask = nsubtasks;
  }

  return scheduler_error;
}


static inline int delegate_work(struct scheduler *scheduler,
                                struct scheduler_task* task,
                                int *ntask,
                                struct worker* calling_worker)
{

#ifdef MCDEBUG
  fprintf(stderr, "[delegate_work] tid %d\n", calling_worker->tid);
  fprintf(stderr, "[delegate_work] granularity %d\n", task->granularity);
  fprintf(stderr, "[delegate_work] iterations %ld\n", task->iterations);
#endif

  pthread_mutex_t mutex;
  CHECK_ERR(pthread_mutex_init(&mutex, NULL), "pthread_mutex_init");
  pthread_cond_t cond;
  CHECK_ERR(pthread_cond_init(&cond, NULL), "pthread_cond_init");

  int shared_counter = 1;

  struct subtask *subtask = setup_subtask(task->par_fn, task->args,
                                          &mutex, &cond, &shared_counter,
                                          0, task->iterations, task->granularity);
  CHECK_ERR(subtask_queue_enqueue(calling_worker, subtask), "subtask_queue_enqueue");

  while(1) {
    CHECK_ERR(pthread_mutex_lock(&mutex), "pthread_mutex_lock");
    /* fprintf(stderr, "tid %d shared counter %d\n", calling_worker->tid, shared_counter); */
    if (shared_counter == 0) break;
    CHECK_ERR(pthread_mutex_unlock(&mutex), "pthread_mutex_unlock");

    subtask = NULL;
    if (subtask_queue_dequeue(calling_worker, &subtask) == 0) {
      if (subtask == NULL) continue;
      // Do work
      assert(subtask->par_fn != NULL);
      assert(subtask->args != NULL);
      int err = subtask->par_fn(subtask->args, subtask->start, subtask->end, calling_worker->tid);
      if (err != 0) {
        return err;
      }
      CHECK_ERR(pthread_mutex_lock(subtask->mutex), "pthread_mutex_lock");
      (*subtask->counter)--;
      CHECK_ERR(pthread_cond_broadcast(subtask->cond), "pthread_cond_broadcast");
      CHECK_ERR(pthread_mutex_unlock(subtask->mutex), "pthread_mutex_unlock");
      free(subtask);
    }
  }

  *ntask = scheduler->num_threads;
  return 0;
}

static inline int do_task_directly(struct scheduler_task *task,
                                   int *ntask)
{
  int err = task->seq_fn(task->args, 0, task->iterations, (worker_local == NULL) ? 0 : worker_local->tid);
  if (err != 0) {
    return err;
  }
  *ntask = 0;
  return 0;
}

static inline int scheduler_do_task(struct scheduler *scheduler,
                                    struct scheduler_task *task,
                                    int *ntask)
{
  assert(scheduler != NULL);
  assert(task != NULL);

#ifdef MCDEBUG
  fprintf(stderr, "[scheduler_do_task] starting task %s with %ld iterations \n", task->name, task->iterations);
#endif
  if (task->iterations == 0) {
    if (ntask != NULL)  *ntask = 0;
    return 0;
  }

  /* Run task directly if below some threshold */
  if (task->iterations < 50) {
    return do_task_directly(task, ntask);
  }

  if (worker_local != NULL) {
    CHECK_ERR(delegate_work(scheduler, task, ntask, worker_local), "delegate_work");
    return 0;
  }

#ifdef MCDEBUG
  start_iter = task->iterations;
  ran_iter = 0;
#endif



  // TODO reevaluate if you really need two functions
  switch(task->granularity)
  {
  case 0:
    return scheduler_static(scheduler, task, ntask);
  default:
    return scheduler_dynamic(scheduler, task, ntask);
  }
}

#endif

// End of scheduler.h
