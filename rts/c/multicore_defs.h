// start of multicore_defs.h

#ifndef MULTICORE_DEFS
#define MULTICORE_DEFS

#include <signal.h>

/* #define MCPROFILE */

// Which queue implementation to use
#define MCJOBQUEUE
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


  /* Profiling fields */
  uint64_t time_enqueue;
  uint64_t time_dequeue;
  uint64_t n_dequeues;
  uint64_t n_enqueues;
};


struct deque_buffer {
  struct subtask** array;
  int64_t size;
};

struct deque {
  struct deque_buffer *buffer;
  int64_t top, bottom;
  int dead;
};


// Function definitions
typedef int (*task_fn)(void* args, int64_t iterations, int tid, struct scheduler_info info);
typedef int (*parloop_fn)(void* args, int64_t start, int64_t end, int subtask_id, int tid);


/* A subtask that can be executed by a thread */
struct subtask {
  parloop_fn fn; // The parloop function
  // Execution parameters
  void* args;
  int64_t start, end;
  int id;

  // Dynamic scheduling paramters
  int chunkable;
  int64_t chunk_size;

  // Shared variables across subtasks
  volatile int *counter; // Counter for ongoing subtasks

  // Shared task timers and iterators
  int64_t *task_time;
  int64_t *task_iter;

  // For debugging
  const char *name;
};



struct worker {
  pthread_t thread;
#if defined(MCCHASELEV)
  struct deque q;
#elif defined(MCJOBQUEUE)
  struct subtask_queue q;
#endif
  struct scheduler *scheduler;
  int cur_working;
  int dead;
  int output_usage;
  int tid;                     /* Just a thread id */

  uint64_t time_spent_working; /* Time spent in tasks functions */
  // Timing field used for online algorithm
  uint64_t timer;
  uint64_t total;
  int nested;
};

#endif

// end of multicore_defs.h
