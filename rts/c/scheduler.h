// start of scheduler.h

#ifndef _SCHEDULER_H_
#define _SCHEDULER_H_

#define MULTICORE
#define MCDEBUG
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

#ifdef MCPROFILE
static long int ran_iter, start_iter = 0;
#endif

static int scheduler_error = 0;
struct scheduler {
  struct worker *workers;
  int num_threads;
};

/* A task for the scheduler to execute */
struct scheduler_task {
  const char* name;
  task_fn fn;
  void* args;
  long int iterations;
  int granularity;
};

struct worker {
  pthread_t thread;
  struct subtask_queue q;
  struct scheduler *scheduler;
  int cur_working;

  int tid;                     /* Just a thread id */
  uint64_t time_spent_working; /* Time spent in tasks functions */
};

/* A wrapper for getting rusage on Linux and MacOS */
/* TODO maybe figure out this for windows */
int getrusage_thread(struct rusage *rusage)
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

void output_thread_usage(struct worker *worker)
{
  struct rusage usage;
  CHECK_ERRNO(getrusage_thread(&usage), "getrusage_thread");
  struct timeval user_cpu_time = usage.ru_utime;
  struct timeval sys_cpu_time = usage.ru_stime;
  fprintf(stderr, "tid: %d - work time %llu - user time: %llu us - sys: %llu us - dequeue: (%llu/%llu) = %llu - enqueue: (%llu/%llu) = %llu \n",
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



/* Ask for a random subtask from another worker */
const int MAX_NUM_TRIES = 10;
static inline int query_a_subtask(struct scheduler* scheduler,
                                  int tid,
                                  struct subtask** subtask)
{
  assert(scheduler != NULL);
  int num_tries = 0;
  while (*subtask == NULL && num_tries < MAX_NUM_TRIES) {
    int worker_idx = rand() % scheduler->num_threads;
    if (worker_idx == tid) continue;
    struct subtask_queue *rand_queue = &scheduler->workers[worker_idx].q;
    assert(rand_queue != NULL);
    CHECK_ERR(subtask_queue_steal(rand_queue, subtask), "subtask_queue_steal");
    num_tries++;
  }
  return 0;
}

static inline void *scheduler_worker(void* arg)
{
  struct worker *worker = (struct worker*) arg;
  struct subtask *subtask = NULL;
  while(1) {
    if (subtask_queue_dequeue(&worker->q, &subtask) == 0) {
      if (subtask == NULL) goto steal;
      assert(subtask->fn != NULL);
      assert(subtask->args != NULL);
#ifdef MCPROFILE
      int64_t start = get_wall_time();
#endif

      worker->cur_working = 1;
      int err = subtask->fn(subtask->args, subtask->start, subtask->end, worker->tid);

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
steal:
      if (subtask_queue_is_empty(&worker->q)) {
        // try to steal some work as we have nothing better to do anyway
        CHECK_ERR(query_a_subtask(worker->scheduler, worker->tid, &subtask), "query_a_subtask");
        if (subtask == NULL) { // We didn't find any work here so just go back to waiting
          continue;
        }
        // else we take the work and put into our own queue
#ifdef MCDEBUG

        fprintf(stderr, "stole work start: %d - end %d\n", subtask->start, subtask->end);
#endif

        CHECK_ERR(subtask_queue_enqueue(&worker->q, subtask), "subtask_queue_enqueue");
      }
      worker->cur_working = 0;
    } else {
      output_thread_usage(worker);
#ifdef MCPROFILE
#endif
      break;
    }
  }
  return NULL;
}

static inline void scheduler_set_queue_profiling(struct scheduler *scheduler, int val)
{
  for (int i = 0; i < scheduler->num_threads; i++) {
    scheduler->workers[i].q.profile = val;
  }
  return;
}


static inline struct worker* get_own_worker_struct(struct scheduler *scheduler)
{
  pthread_t pid = pthread_self();
  for (int i = 0; i < scheduler->num_threads; i++) {
    if (pid == scheduler->workers[i].thread)
    {
      return &scheduler->workers[i];
    }
  }
  return NULL;
}


static inline int scheduler_dynamic(struct scheduler *scheduler,
                                    struct scheduler_task *task,
                                    int *ntask)
{
#ifdef MCDEBUG
  fprintf(stderr, "Performing dynamic scheduling\n");
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
    struct subtask *subtask = setup_subtask(task->fn, task->args,
                                            &mutex, &cond, &shared_counter,
                                            start, end, chunks);
    assert(subtask != NULL);
    CHECK_ERR(subtask_queue_enqueue(&scheduler->workers[subtask_id%scheduler->num_threads].q, subtask),
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

#ifdef MCDEBUG
  assert(ran_iter == start_iter);
#endif

#ifdef MCPROFILE
  scheduler_set_queue_profiling(scheduler, 0);
#endif

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
    struct subtask *subtask = setup_subtask(task->fn, task->args,
                                            &mutex, &cond, &shared_counter,
                                            start, end, 0);
    assert(subtask != NULL);
    CHECK_ERR(subtask_queue_enqueue(&scheduler->workers[subtask_id].q, subtask),
              "subtask_queue_enqueue");
#ifdef MCDEBUG
    fprintf(stderr, "[scheduler_static] iter_pr_subtask %d - exp %d\n", iter_pr_subtask, subtask_id < remainder);
    fprintf(stderr, "[scheduler_static] pushed start %d - end %d (%d) iterations onto %d's q\n", start, end, end-start, subtask_id%scheduler->num_threads);
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

#ifdef MCPROFILE
  scheduler_set_queue_profiling(scheduler, 0);
#endif

  return scheduler_error;

}


static inline int delegate_work(struct scheduler *scheduler,
                                struct scheduler_task* task,
                                int *ntask,
                                struct worker* calling_worker)
{
  int free_workers = 1; // The calling worker is also "free"
  for (int i = 0; i < scheduler->num_threads; i++) {
    if (i == calling_worker->tid) continue;
    // Lets just hope this doesn't change during execution
    if (scheduler->workers[i].cur_working == 0) {
      free_workers++;
    }
  }
#ifdef MCDEBUG
  fprintf(stderr, "[delegate_work] free worker %d\n", free_workers);
#endif

  CHECK_ERR(task->fn(task->args, 0, task->iterations, calling_worker->tid), task->name);

  *ntask = scheduler->num_threads;
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
#ifdef MCPROFILE
  scheduler_set_queue_profiling(scheduler, 1);
#endif

  if (task->iterations == 0) {
    if (ntask != NULL)  *ntask = 0;
    return 0;
  }

  struct worker *worker = get_own_worker_struct(scheduler);
  if (worker != NULL)
  {
    CHECK_ERR(delegate_work(scheduler, task, ntask, worker), "delegate_work");
    return 0;
  }

#ifdef MCPROFILE
  start_iter = task->iterations;
  ran_iter = 0;
#endif

  /* Run task directly if below some threshold */
  /* if (task->iterations < 1000) { */
  /* } */

  // TODO reevaluate if you really need two functions
  switch(task->granularity)
  {
  case 0:
    return scheduler_static(scheduler, task, ntask);
  default:
    return scheduler_dynamic(scheduler, task, ntask);
    return -1;
  }
}

#endif

// End of scheduler.h
