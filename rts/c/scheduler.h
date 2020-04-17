// start of scheduler.h

#ifndef _SCHEDULER_H_
#define _SCHEDULER_H_

#define MULTICORE
/* #define MCDEBUG */

#ifdef _WIN32
#include <windows.h>
#elif __APPLE__
#include <sys/sysctl.h>
// Used for getting cpu usage of threads
#include <mach/mach.h>
#include <sys/resource.h>
#else // Linux
#include <sys/sysinfo.h>
#include <sys/resource.h>
#endif


typedef int (*task_fn)(void* args, int start, int end, int subtask_id);


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
  int is_static; // Whether it's possible to schedule the task as dynamic or not
};


struct worker {
  pthread_t thread;
  struct subtask_queue q;
  struct scheduler *scheduler;

  int tid;                     /* Just a thread id */
  long int time_spent_working; /* Time spent in tasks functions */
};

/* A subtask that can be executed by a thread */
struct subtask {
  task_fn fn;
  void* args;
  int start, end;
  int subtask_id;

  // Shared variables across subtasks
  int *counter; // Counter for ongoing subtasks
  pthread_mutex_t *mutex;
  pthread_cond_t *cond;
};


/* A wrapper for getting rusage on Linux and MacOS */
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
  fprintf(stderr, "tid: %d - work time %ld - user time: %ld us - sys: %ld us \n",
          worker->tid,
          worker->time_spent_working,
          user_cpu_time.tv_sec * 1000000 + user_cpu_time.tv_usec,
          sys_cpu_time.tv_sec * 1000000 + sys_cpu_time.tv_usec);
}

// returns the number of logical cores
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


/* Ask for a random queue from another worker */
static inline struct worker* query_a_subtask_queue(struct scheduler* scheduler, int tid)
{
  assert(scheduler != NULL);
  if (scheduler->num_threads == 1) return &scheduler->workers[0];

  int worker_idx;
  do {
     worker_idx = rand() % scheduler->num_threads;
  } while (worker_idx == tid);
  return &scheduler->workers[worker_idx];
}

static inline void *futhark_worker(void* arg)
{
  struct worker *worker = (struct worker*) arg;
  while(1) {
    struct subtask *subtask;
    if (subtask_queue_dequeue(&worker->q, &subtask) == 0) {
      int64_t start = get_wall_time();
      int err = subtask->fn(subtask->args, subtask->start, subtask->end, subtask->subtask_id);
      int64_t end = get_wall_time();
      worker->time_spent_working += end - start;
      CHECK_ERR(pthread_mutex_lock(subtask->mutex), "pthread_mutex_lock");
      // Only one error can be returned at the time now
      // Maybe we can provide a stack like structure for pushing errors onto
      // if we wish to backpropagte multiple errors
      if (err != 0) {
        scheduler_error = err;
      }
      (*subtask->counter)--;
      CHECK_ERR(pthread_cond_signal(subtask->cond), "pthread_cond_signal");
      CHECK_ERR(pthread_mutex_unlock(subtask->mutex), "pthread_mutex_unlock");
      free(subtask);
      if (subtask_queue_is_empty(&worker->q)) {
        // try to steal some work as we have nothing better to do anyway
        struct worker *rand_worker = query_a_subtask_queue(worker->scheduler, worker->tid);
        if (rand_worker == NULL)  {
          assert(!"failed to find queue \n");
          continue;
        }
        struct subtask *subtask;
        int retval = subtask_queue_steal(&rand_worker->q, &subtask);
        if (retval != 0) { // We didn't find any work here
          // TODO the return val can also mean queue failed
          // so avoid this overloading of return values
          continue;
        }
        // else we take the work and put into our own queue
        CHECK_ERR(subtask_queue_enqueue(&worker->q, subtask), "subtask_queue_enqueue");
      }
    } else {
      output_thread_usage(worker);
#ifdef MCDEBUG
#endif
      break;
    }
  }
  return NULL;
}

static inline struct subtask* setup_subtask(struct scheduler_task* task,
                                            int subtask_id,
                                            pthread_mutex_t *mutex,
                                            pthread_cond_t *cond,
                                            int* counter,
                                            int start, int end)
{
  struct subtask* subtask = malloc(sizeof(struct subtask));
  if (subtask == NULL) {
    assert(!"malloc failed in setup_subtask");
    return  NULL;
  }
  subtask->fn         = task->fn;
  subtask->args       = task->args;
  subtask->subtask_id = subtask_id;
  subtask->mutex      = mutex;
  subtask->cond       = cond;
  subtask->counter    = counter;
  subtask->start      = start;
  subtask->end        = end;
  return subtask;
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

  /* For now just set this to 100 subtasks; */
  /* As we don't have any information about the load balancing yet */
  int max_num_tasks = 100;
  int subtask_id = 0;
  int iter_pr_subtask = task->iterations / max_num_tasks;
  int remainder = task->iterations % max_num_tasks;

  int nsubtasks = iter_pr_subtask == 0 ? 1 : ((task->iterations - remainder) / iter_pr_subtask);
  int shared_counter = nsubtasks;

  int start = 0;
  int end = iter_pr_subtask + remainder;
  for (int subtask_id = 0; subtask_id < nsubtasks; subtask_id++) {
    struct subtask *subtask = setup_subtask(task, subtask_id % scheduler->num_threads,
                                            &mutex, &cond, &shared_counter,
                                            start, end);
    assert(subtask != NULL);
    CHECK_ERR(subtask_queue_enqueue(&scheduler->workers[subtask_id%scheduler->num_threads].q, subtask),
              "subtask_queue_enqueue");
    // Update range params
    start = end;
    end += iter_pr_subtask;
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

/* Performs static scheduling of a task */
/* Divides the number of iterations into num_threads equal sized chunks */
/* (with the first thread taking the remaining iterations that does
   not divide the number of threads) */
static inline int scheduler_static(struct scheduler *scheduler,
                                   struct scheduler_task *task,
                                   int *ntask)
{
  pthread_mutex_t mutex;
  CHECK_ERR(pthread_mutex_init(&mutex, NULL), "pthread_mutex_init");
  pthread_cond_t cond;
  CHECK_ERR(pthread_cond_init(&cond, NULL), "pthread_cond_init");

  int subtask_id = 0;
  int iter_pr_subtask = task->iterations / scheduler->num_threads;
  int remainder = task->iterations % scheduler->num_threads;


  int nsubtasks = iter_pr_subtask == 0 ? 1 : ((task->iterations - remainder) / iter_pr_subtask);
  int shared_counter = nsubtasks;

  int start = 0;
  int end = iter_pr_subtask + remainder;
  for (int subtask_id = 0; subtask_id < nsubtasks; subtask_id++) {
    struct subtask *subtask = setup_subtask(task, subtask_id,
                                            &mutex, &cond, &shared_counter,
                                            start, end);
    assert(subtask != NULL);
    CHECK_ERR(subtask_queue_enqueue(&scheduler->workers[subtask_id].q, subtask), "subtask_queue_enqueue");
    // Update range params
    start = end;
    end += iter_pr_subtask;
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

static inline int scheduler_do_task(struct scheduler *scheduler,
                                    struct scheduler_task *task,
                                    int *ntask)
{

  assert(scheduler != NULL);
  assert(task != NULL);

#ifdef MCDEBUG
  fprintf(stderr, "starting %s\n", task->name);
  fprintf(stderr, "iterations %ld\n", task->iterations);
#endif
  if (task->iterations == 0) {
    if (ntask != NULL)  *ntask = 0;
    return 0;
  }

  switch(task->is_static)
  {
  case 0:
    return scheduler_dynamic(scheduler, task, ntask);
  case 1:
    return scheduler_static(scheduler, task, ntask);
  default:
    return -1;
  }
}

#endif

// End of scheduler.h
