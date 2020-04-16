// start of subtask_queue.h

#ifndef SUBTASK_QUEUE_H
#define SUBTASK_QUEUE_H

#include <pthread.h>
#include <stdlib.h>
#include <assert.h>


// Forward declare struct subtask;
struct subtask;

struct subtask_queue {
  int capacity; // Size of the buffer.
  int first; // Index of the start of the ring buffer.
  int num_used; // Number of used elements in the buffer.
  struct subtask **buffer;

  pthread_mutex_t mutex; // Mutex used for synchronisation.
  pthread_cond_t cond;   // Condition variable used for synchronisation.

  int dead;
};


// Initialise a job queue with the given capacity.  The queue starts out
// empty.  Returns non-zero on error.
static inline int subtask_queue_init(struct subtask_queue *subtask_queue, int capacity)
{
  subtask_queue->capacity = capacity;
  subtask_queue->first = 0;
  subtask_queue->num_used = 0;
  subtask_queue->dead = 0;
  subtask_queue->buffer = calloc(capacity, sizeof(void*));

  CHECK_ERRNO(pthread_mutex_init(&subtask_queue->mutex, NULL), "pthread_mutex_init");
  CHECK_ERRNO(pthread_cond_init(&subtask_queue->cond, NULL), "pthread_cond_init");

  if (subtask_queue->buffer == NULL) {
    return -1;
  }

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
static inline int subtask_queue_enqueue(struct subtask_queue *subtask_queue, struct subtask *subtask )
{
  assert(subtask_queue != NULL);
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

  // Signal a reader (if any) that there is now an element.
  CHECK_ERR(pthread_cond_broadcast(&subtask_queue->cond), "pthread_cond_broadcast");
  CHECK_ERR(pthread_mutex_unlock(&subtask_queue->mutex), "pthread_mutex_unlock");

  return 0;
}

// Pop an element from the front of the job queue.  Blocks if the
// subtask_queue contains zero elements.  Returns non-zero on error.  If
// subtask_queue_destroy() has been called (possibly after the call to
// subtask_queue_pop() blocked), this function will return -1.
static inline int subtask_queue_dequeue(struct subtask_queue *subtask_queue, struct subtask **subtask)
{
  assert(subtask_queue != NULL);
  CHECK_ERR(pthread_mutex_lock(&subtask_queue->mutex), "pthread_mutex_lock");

  // Wait until the subtask_queue contains an element.
  while (subtask_queue->num_used == 0 && !subtask_queue->dead) {
    CHECK_ERR(pthread_cond_wait(&subtask_queue->cond, &subtask_queue->mutex), "pthread_cond_wait");
  }

  if (subtask_queue->dead) {
    CHECK_ERR(pthread_mutex_unlock(&subtask_queue->mutex), "pthread_mutex_unlock");
    return -1;
  }

  // We now know that num_used is nonzero.
  subtask_queue->num_used--;
  *subtask = subtask_queue->buffer[subtask_queue->first];
  subtask_queue->first = (subtask_queue->first + 1) % subtask_queue->capacity;

  // Signal a writer (if any) that there is now room for more.
  CHECK_ERR(pthread_cond_broadcast(&subtask_queue->cond), "pthread_cond_broadcast");
  CHECK_ERR(pthread_mutex_unlock(&subtask_queue->mutex), "pthread_mutex_unlock");

  return 0;
}

/* TODO: Do I need to acquire locks here? */
static inline int subtask_queue_is_empty(struct subtask_queue *subtask_queue)
{
  return subtask_queue->num_used == 0;
}


/* Like subtask_queue_dequeue, but returns immediately if there is no tasks queued,
   as we dont' want to block on another workers queue */
static inline int subtask_queue_steal(struct subtask_queue *subtask_queue,
                                      struct subtask **subtask)
{
  assert(subtask_queue != NULL);
  CHECK_ERR(pthread_mutex_lock(&subtask_queue->mutex), "pthread_mutex_lock");

  if (subtask_queue_is_empty(subtask_queue)) {
    // Signal a writer (if any) that there is now room for more.
    CHECK_ERR(pthread_cond_broadcast(&subtask_queue->cond), "pthread_cond_broadcast");
    CHECK_ERR(pthread_mutex_unlock(&subtask_queue->mutex), "pthread_mutex_unlock");
    return -1;
  }
  // Wait until the subtask_queue contains an element.
  while (subtask_queue->num_used == 0 && !subtask_queue->dead) {
    CHECK_ERR(pthread_cond_wait(&subtask_queue->cond, &subtask_queue->mutex), "pthread_cond_wait");
  }

  if (subtask_queue->dead) {
    CHECK_ERR(pthread_mutex_unlock(&subtask_queue->mutex), "pthread_mutex_unlock");
    return -1;
  }

  // We now know that num_used is nonzero.
  subtask_queue->num_used--;
  *subtask = subtask_queue->buffer[subtask_queue->first];
  subtask_queue->first = (subtask_queue->first + 1) % subtask_queue->capacity;

  // Signal a writer (if any) that there is now room for more.
  CHECK_ERR(pthread_cond_broadcast(&subtask_queue->cond), "pthread_cond_broadcast");
  CHECK_ERR(pthread_mutex_unlock(&subtask_queue->mutex), "pthread_mutex_unlock");

  return 0;
}



#endif

// End of subtask_queue.h
