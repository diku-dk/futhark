// start of multicore_defs.h

// Forward declarations
// Scheduler definitions
struct scheduler;
struct scheduler_info;
struct scheduler_subtask;
struct scheduler_task;


struct subtask_queue {
  int capacity;             // Size of the buffer.
  int first;                // Index of the start of the ring buffer.
  int num_used;             // Number of used elements in the buffer.
  struct subtask **buffer;

  pthread_mutex_t mutex;    // Mutex used for synchronisation.
  pthread_cond_t cond;      // Condition variable used for synchronisation.
  int dead;

#if defined(MCPROFILE)
  /* Profiling fields */
  uint64_t time_enqueue;
  uint64_t time_dequeue;
  uint64_t n_dequeues;
  uint64_t n_enqueues;
#endif
};



// Function definitions
typedef int (*segop_fn)(void* args, int64_t iterations, int tid, struct scheduler_info info);
typedef int (*parloop_fn)(void* args, int64_t start, int64_t end, int subtask_id, int tid);


/* A subtask that can be executed by a worker */
struct subtask {
  /* The parloop function */
  parloop_fn fn;
  /* Execution parameters */
  void* args;
  int64_t start, end;
  int id;

  /* Dynamic scheduling parameters */
  int chunkable;
  int64_t chunk_size;

  /* Shared variables across subtasks */
  volatile int *counter; // Counter for ongoing subtasks
  // Shared task timers and iterators
  int64_t *task_time;
  int64_t *task_iter;

  /* For debugging */
  const char *name;
};


struct worker {
  pthread_t thread;
  struct scheduler *scheduler;  /* Reference to the scheduler struct the worker belongs to*/
  struct subtask_queue q;
  int dead;
  int tid;                      /* Just a thread id */

  /* "thread local" time fields used for online algorithm */
  uint64_t timer;
  uint64_t total;
  int nested; /* How nested the current computation is */

  // Profiling fields
  int output_usage;            /* Whether to dump thread usage */
  uint64_t time_spent_working; /* Time spent in parloop functions */
};

static inline void output_worker_usage(struct worker *worker)
{
  struct rusage usage;
  CHECK_ERRNO(getrusage_thread(&usage), "getrusage_thread");
  struct timeval user_cpu_time = usage.ru_utime;
  struct timeval sys_cpu_time = usage.ru_stime;
  fprintf(stderr, "tid: %2d - work time %10llu us - user time: %10llu us - sys: %10llu us\n",
          worker->tid,
          (long long unsigned)worker->time_spent_working / 1000,
          (long long unsigned)(user_cpu_time.tv_sec * 1000000 + user_cpu_time.tv_usec),
          (long long unsigned)(sys_cpu_time.tv_sec * 1000000 + sys_cpu_time.tv_usec));
}

// end of multicore_defs.h
