// start of scheduler.h

#ifndef SCHEDULER_H
#define SCHEDULER_H

#ifdef _WIN32
#include <windows.h>
#elif __APPLE__
#include <sys/sysctl.h>
#else
#include <sys/sysinfo.h>
#endif

static inline int check_err(int errval, int sets_errno, const char *fun, int line,
                            const char *msg, ...)
{
  if (errval) {
    char str[256];
    char errnum[10];
    sprintf(errnum, "%d", errval);
    sprintf(str, "ERROR: %s in %s() at line %d with %s\n", msg, fun, line,
            sets_errno ? strerror(errno) : errnum);
    fprintf(stderr, "%s", str);
  }

  return errval;
}

#define CHECK_ERR(err, msg...) check_err(err, 0, __func__, __LINE__, msg)
#define CHECK_ERRNO(err, msg...) check_err(err, 1, __func__, __LINE__, msg)

// returns the number of logical cores
static int num_processors() {
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
    if (sysctlbyname("hw.logicalcpu", &ncores, &ncores_size, NULL, 0)) {
      fprintf(stderr, "[num_processors] failed to find number of cores %s", strerror(errno));
      return -1;
    }
    return ncores;
#else // If Linux
  return get_nprocs();
#endif
}

#define MULTICORE

typedef int (*task_fn)(void*, int, int, int);

struct task {
  task_fn fn;
  void* args;
  int start, end;
  int task_id; // or a subtask id

  int *counter; // Counter ongoing subtasks
  pthread_mutex_t *mutex;
  pthread_cond_t *cond;
};


struct scheduler {
  struct job_queue q;
  pthread_t *threads; // A list of threads
  int num_threads;
};

enum SegOp {
  SegMap,
  SegRed,
};

static inline void *futhark_worker(void* arg) {
  struct scheduler *scheduler = (struct scheduler*) arg;
  while(1) {
    struct task *task;
    if (job_queue_pop(&scheduler->q, (void**)&task) == 0) {
      task->fn(task->args, task->start, task->end, task->task_id);
      CHECK_ERR(pthread_mutex_lock(task->mutex), "pthread_mutex_lock");
      (*task->counter)--;
      CHECK_ERR(pthread_cond_signal(task->cond), "pthread_cond_signal");
      CHECK_ERR(pthread_mutex_unlock(task->mutex), "pthread_mutex_unlock");
      free(task);
    } else {
       break;
    }
  }
  return NULL;
}



static inline struct task* setup_task(task_fn fn, void* task_args, int task_id,
                                      pthread_mutex_t *mutex, pthread_cond_t *cond,
                                      int* counter, int start, int end) {
  // Don't allocate this on heap, use stack!
  struct task* task = malloc(sizeof(struct task));
  if (task == NULL) {
    assert(!"malloc failed in setup_task");
    return  NULL;
  }
  task->fn      = fn;
  task->args    = task_args;
  task->task_id = task_id;
  task->mutex   = mutex;
  task->cond    = cond;
  task->counter = counter;
  task->start   = start;
  task->end     = end;
  return task;
}


static inline int scheduler_do_task(struct scheduler *scheduler,
                                    task_fn fn, void* task_args,
                                    int iterations, int *ntask)
{
  assert(scheduler != NULL);
  if (iterations == 0) {
    if (ntask != NULL)  *ntask = 0;
    return 0;
  }

  pthread_mutex_t mutex;
  CHECK_ERR(pthread_mutex_init(&mutex, NULL), "pthread_mutex_init: \n");
  pthread_cond_t cond;
  CHECK_ERR(pthread_cond_init(&cond, NULL), "pthread_cond_init");

  int task_id = 0;
  int shared_counter = 0;
  int iter_pr_task = iterations / scheduler->num_threads;
  int remainder = iterations % scheduler->num_threads;

  struct task *task = setup_task(fn, task_args, task_id,
                                 &mutex, &cond, &shared_counter,
                                 0, remainder + iter_pr_task);
  task_id++;

  CHECK_ERR(pthread_mutex_lock(&mutex), "pthread_mutex_lock");
  shared_counter++;
  CHECK_ERR(pthread_mutex_unlock(&mutex), "pthread_mutex_unlock");
  CHECK_ERR(job_queue_push(&scheduler->q, (void*)task), "job_queue_push");


  for (int i = remainder + iter_pr_task; i < iterations; i += iter_pr_task)
  {
    struct task *task = setup_task(fn, task_args, task_id,
                                   &mutex, &cond, &shared_counter,
                                   i, i + iter_pr_task);
    task_id++;

    CHECK_ERR(pthread_mutex_lock(&mutex), "pthread_mutex_lock");
    shared_counter++;
    CHECK_ERR(pthread_mutex_unlock(&mutex), "pthread_mutex_unlock");
    CHECK_ERR(job_queue_push(&scheduler->q, (void*)task), "job_queue_push");
  }


  // Join (wait for tasks to finish)
  CHECK_ERR(pthread_mutex_lock(&mutex), "pthread_mutex_lock");
  while (shared_counter != 0) {
    CHECK_ERR(pthread_cond_wait(&cond, &mutex), "pthread_cond_wait");
  }

  if (ntask != NULL) {
    *ntask = task_id;
  }

  return 0;
}


#endif


// End of scheduler.h
