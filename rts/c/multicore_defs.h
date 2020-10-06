// start of multicore_defs.h

#ifndef MULTICORE_DEFS
#define MULTICORE_DEFS

#include <signal.h>

/* #define MCPROFILE */

// Which queue implementation to use
#define MCJOBQUEUE
// NOTE! MCCHASELEV has been removed from multicore branch
// Switch to multicore_deque branch to use chase-lev deque
/* #define MCCHASELEV */


#if defined(_WIN32)
#include <windows.h>
#elif defined(__APPLE__)
#include <sys/sysctl.h>
// For getting cpu usage of threads
#include <mach/mach.h>
#include <sys/resource.h>
#elif defined(__linux__)
#include <sys/sysinfo.h>
#include <sys/resource.h>
#include <signal.h>
#endif


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

#endif

// end of multicore_defs.h
