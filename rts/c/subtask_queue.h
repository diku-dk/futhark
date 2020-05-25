// start of subtask_queue.h

#ifndef SUBTASK_QUEUE_H
#define SUBTASK_QUEUE_H

#include <pthread.h>
#include <stdlib.h>
#include <assert.h>


typedef int (*task_fn)(void* args, int iterations);
typedef int (*sub_task_fn)(void* args, int start, int end, int subtask_id);

/* A subtask that can be executed by a thread */
struct subtask {
  sub_task_fn par_fn;
  void* args;
  int start, end;
  // How much of a task to take a the time
  // If it's zero , then the subtasks is not stealable
  int chunk;

  // Shared variables across subtasks
  int *counter; // Counter for ongoing subtasks
  pthread_mutex_t *mutex;
  pthread_cond_t *cond;
};



struct scheduler {
  struct worker *workers;
  int num_threads;
};

/* A task for the scheduler to execute */
struct scheduler_task {
  const char* name;
  sub_task_fn par_fn;
  void* args;
  long int iterations;
  int granularity;
};


struct subtask_queue {
  int capacity; // Size of the buffer.
  int first; // Index of the start of the ring buffer.
  int num_used; // Number of used elements in the buffer.
  struct subtask **buffer;

  pthread_mutex_t mutex; // Mutex used for synchronisation.
  pthread_cond_t cond;   // Condition variable used for synchronisation.
  int dead;


  int initialized;

  /* Profiling fields */
  uint64_t time_enqueue;
  uint64_t time_dequeue;
  uint64_t n_dequeues;
  uint64_t n_enqueues;
};


struct worker {
  pthread_t thread;
  struct subtask_queue q;
  struct scheduler *scheduler;
  int cur_working;

  int tid;                     /* Just a thread id */
  uint64_t time_spent_working; /* Time spent in tasks functions */
};


static inline struct subtask* setup_subtask(sub_task_fn par_fn,
                                            void* args,
                                            pthread_mutex_t *mutex,
                                            pthread_cond_t *cond,
                                            int* counter,
                                            int start, int end,
                                            int chunk)
{
  struct subtask* subtask = malloc(sizeof(struct subtask));
  if (subtask == NULL) {
    assert(!"malloc failed in setup_subtask");
    return  NULL;
  }
  subtask->par_fn  = par_fn;
  subtask->args    = args;
  subtask->mutex   = mutex;
  subtask->cond    = cond;
  subtask->counter = counter;
  subtask->start   = start;
  subtask->end     = end;
  subtask->chunk   = chunk;
  return subtask;
}


static inline struct subtask* jobqueue_get_subtask_chunk(struct subtask_queue *subtask_queue, int from_end)
{
  struct subtask *cur_head = subtask_queue->buffer[subtask_queue->first];

  int remaining_iter = cur_head->end - cur_head->start;
  assert(remaining_iter > 0);

  if (cur_head->chunk > 0 && remaining_iter > cur_head->chunk)
  {
    struct subtask *new_subtask = malloc(sizeof(struct subtask));
    *new_subtask = *cur_head;

    // Update ranges on new subtasks
    if (from_end) {
      new_subtask->start = cur_head->end - cur_head->chunk;
      cur_head->end = new_subtask->start;
    } else {
      new_subtask->end = cur_head->start + cur_head->chunk;
      cur_head->start += cur_head->chunk;
    }
    CHECK_ERR(pthread_mutex_lock(cur_head->mutex), "pthread_mutex_lock");
    (*cur_head->counter)++;
    CHECK_ERR(pthread_cond_broadcast(cur_head->cond), "pthread_cond_signal");
    CHECK_ERR(pthread_mutex_unlock(cur_head->mutex), "pthread_mutex_unlock");

    return new_subtask;
  }

  subtask_queue->num_used--;
  subtask_queue->first = (subtask_queue->first + 1) % subtask_queue->capacity;

  return cur_head;
}

// Initialise a job queue with the given capacity.  The queue starts out
// empty.  Returns non-zero on error.
static inline int subtask_queue_init(struct subtask_queue *subtask_queue, int capacity)
{
  assert(subtask_queue != NULL);
  memset(subtask_queue, 0, sizeof(struct subtask_queue));

  subtask_queue->capacity = capacity;
  subtask_queue->buffer = calloc(capacity, sizeof(struct subtask*));
  if (subtask_queue->buffer == NULL) {
    return -1;
  }

  CHECK_ERRNO(pthread_mutex_init(&subtask_queue->mutex, NULL), "pthread_mutex_init");
  CHECK_ERRNO(pthread_cond_init(&subtask_queue->cond, NULL), "pthread_cond_init");

  subtask_queue->initialized = 1;

  return 0;
}

// Destroy the job queue.  Blocks until the queue is empty before it
// is destroyed.
static inline int subtask_queue_destroy(struct subtask_queue *subtask_queue)
{
  assert(subtask_queue != NULL);

  CHECK_ERR(pthread_mutex_lock(&subtask_queue->mutex), "pthread_mutex_lock");

  while (subtask_queue->num_used != 0) {
    CHECK_ERR(pthread_cond_wait(&subtask_queue->cond, &subtask_queue->mutex), "pthread_cond_wait");
  }

  // Queue is now empty.  Let's kill it!
  subtask_queue->dead = 1;
  free(subtask_queue->buffer);
  CHECK_ERR(pthread_cond_broadcast(&subtask_queue->cond), "pthread_cond_broadcast");
  CHECK_ERR(pthread_mutex_unlock(&subtask_queue->mutex), "pthread_mutex_unlock");

  return 0;
}

// Push an element onto the end of the job queue.  Blocks if the
// subtask_queue is full (its size is equal to its capacity).  Returns
// non-zero on error.  It is an error to push a job onto a queue that
// has been destroyed.
static inline int subtask_queue_enqueue(struct worker *worker, struct subtask *subtask )
{
  assert(worker != NULL);
  struct subtask_queue *subtask_queue = &worker->q;
  uint64_t start = get_wall_time();

  CHECK_ERR(pthread_mutex_lock(&subtask_queue->mutex), "pthread_mutex_lock");
  // Wait until there is room in the subtask_queue.
  while (subtask_queue->num_used == subtask_queue->capacity && !subtask_queue->dead) {
    CHECK_ERR(pthread_cond_wait(&subtask_queue->cond, &subtask_queue->mutex), "pthread_cond_wait");
  }

  if (subtask_queue->dead) {
    CHECK_ERR(pthread_mutex_unlock(&subtask_queue->mutex), "pthread_mutex_unlock");
    return -1;
  }

  // If we made it past the loop, there is room in the subtask_queue.
  subtask_queue->buffer[(subtask_queue->first + subtask_queue->num_used) % subtask_queue->capacity] = subtask;
  subtask_queue->num_used++;

  uint64_t end = get_wall_time();
  subtask_queue->time_enqueue += (end - start);
  subtask_queue->n_enqueues++;

  // Broadcast a reader (if any) that there is now an element.
  CHECK_ERR(pthread_cond_broadcast(&subtask_queue->cond), "pthread_cond_broadcast");
  CHECK_ERR(pthread_mutex_unlock(&subtask_queue->mutex), "pthread_mutex_unlock");

  return 0;
}



/* Like subtask_queue_dequeue, but returns immediately if there is no tasks queued,
   as we dont' want to block on another workers queue */
static inline int subtask_queue_steal(struct worker *worker,
                                      struct subtask **subtask)
{
  struct subtask_queue *subtask_queue = &worker->q;
  assert(subtask_queue != NULL);
  if (subtask_queue->initialized != 1)  {
    return 1;
  }

  uint64_t start = get_wall_time();
  CHECK_ERR(pthread_mutex_lock(&subtask_queue->mutex), "pthread_mutex_lock");

  // chunk == 0 indicates that task is not stealable
  // so we just return also
  if (subtask_queue->num_used == 0 || subtask_queue->buffer[subtask_queue->first]->chunk == 0) {
    CHECK_ERR(pthread_cond_broadcast(&subtask_queue->cond), "pthread_cond_broadcast");
    CHECK_ERR(pthread_mutex_unlock(&subtask_queue->mutex), "pthread_mutex_unlock");
    return 1;
  }

  if (subtask_queue->dead) {
    CHECK_ERR(pthread_mutex_unlock(&subtask_queue->mutex), "pthread_mutex_unlock");
    return -1;
  }

  *subtask = jobqueue_get_subtask_chunk(subtask_queue, 1);
  if (*subtask == NULL) {
    CHECK_ERR(pthread_mutex_unlock(&subtask_queue->mutex), "pthred_mutex_unlock");
    return -1;
  }

  uint64_t end = get_wall_time();
  subtask_queue->time_dequeue += (end - start);
  subtask_queue->n_dequeues++;

  // Broadcast a writer (if any) that there is now room for more.
  CHECK_ERR(pthread_cond_broadcast(&subtask_queue->cond), "pthread_cond_broadcast");
  CHECK_ERR(pthread_mutex_unlock(&subtask_queue->mutex), "pthread_mutex_unlock");

  return 0;
}



/* Ask for a random subtask from another worker */
const int MAX_NUM_TRIES = 10;
static inline int query_a_subtask(struct scheduler* scheduler,
                                  int tid,
                                  struct worker *worker)
{
  assert(scheduler != NULL);
  int num_tries = 0;
  struct subtask *subtask = NULL;
  while (subtask == NULL && num_tries < MAX_NUM_TRIES) {
    int worker_idx = rand() % scheduler->num_threads;
    if (worker_idx == tid) continue;
    struct worker *rand_worker = &scheduler->workers[worker_idx];
    assert(rand_worker != NULL);
    int err = subtask_queue_steal(rand_worker, &subtask);
    if (err == 0) { /* we found some work */
      break;
    } else if (err == 1) { /* Queue was not ready or no work found */
      num_tries++;
    } else { /* Queue failed else */
      return err;
    }
  }

  if (subtask != NULL && num_tries != MAX_NUM_TRIES) {
    CHECK_ERR(subtask_queue_enqueue(worker, subtask), "subtask_queue_enqueue");
    return 0;
  }
  return 1;
}


// Pop an element from the front of the job queue.  Blocks if the
// subtask_queue contains zero elements.  Returns non-zero on error.  If
// subtask_queue_destroy() has been called (possibly after the call to
// subtask_queue_pop() blocked), this function will return -1.
// TODO Maybe figure out how to do some kind of interrupt here to periodically
// wake up thread to check for work (or steal)
static inline int subtask_queue_dequeue(struct worker *worker, struct subtask **subtask)
{
  assert(worker != NULL);
  struct subtask_queue *subtask_queue = &worker->q;

  // I don't want to measure time waiting from initial initailization of queue
  // until first task so we use a little hack here
  uint64_t start = get_wall_time();

  CHECK_ERR(pthread_mutex_lock(&subtask_queue->mutex), "pthread_mutex_lock");
  // Try to steal some work while the subtask_queue is empty
  while (subtask_queue->num_used == 0 && !subtask_queue->dead) {
    CHECK_ERR(pthread_mutex_unlock(&subtask_queue->mutex), "pthread_mutex_unlock");
    int retval = query_a_subtask(worker->scheduler, worker->tid, worker);
    CHECK_ERR(pthread_mutex_lock(&subtask_queue->mutex), "pthread_mutex_unlock");
    if (retval == 1) { // we didn't find anything so go back to sleep
      struct timespec ts;
      CHECK_ERR(clock_getres(CLOCK_REALTIME, &ts), "clock_getres");
      ts.tv_nsec += 50000; // wait for 50 ms (ish)
      int err = pthread_cond_timedwait(&subtask_queue->cond, &subtask_queue->mutex, &ts);
      if (err != 0 && err != ETIMEDOUT) {
        assert(!"pthread_cond_timedwait failed \n");
      }
    } else {
      CHECK_ERR(retval, "steal_a_subtask");
    }
  }

  if (subtask_queue->dead) {
    CHECK_ERR(pthread_mutex_unlock(&subtask_queue->mutex), "pthread_mutex_unlock");
    return -1;
  }

  *subtask = jobqueue_get_subtask_chunk(subtask_queue, 0);
  if (*subtask == NULL) {
    assert(!"got NULL ptr");
    CHECK_ERR(pthread_mutex_unlock(&subtask_queue->mutex), "pthred_mutex_unlock");
    return -1;
  }

  uint64_t end = get_wall_time();
  subtask_queue->time_dequeue += (end - start);
  subtask_queue->n_dequeues++;

  // Broadcast a writer (if any) that there is now room for more.
  CHECK_ERR(pthread_cond_broadcast(&subtask_queue->cond), "pthread_cond_broadcast");
  CHECK_ERR(pthread_mutex_unlock(&subtask_queue->mutex), "pthread_mutex_unlock");

  return 0;
}

/* TODO: Do I need to acquire locks here? */
static inline int subtask_queue_is_empty(struct subtask_queue *subtask_queue)
{
  return ((volatile int)subtask_queue->num_used) == 0 && !subtask_queue->dead;
}
#endif

// End of subtask_queue.h
