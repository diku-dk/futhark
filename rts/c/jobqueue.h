// start of jobqueue.h

#ifndef JOB_QUEUE_H
#define JOB_QUEUE_H

#include <pthread.h>
#include <stdlib.h>
#include <assert.h>

struct job_queue {
  int capacity; // Size of the buffer.
  int first; // Index of the start of the ring buffer.
  int num_used; // Number of used elements in the buffer.
  void **buffer; // Pointer to the memory backing the buffer, which is
                 // conceptually an array of void pointers.

  pthread_mutex_t mutex; // Mutex used for synchronisation.
  pthread_cond_t cond;   // Condition variable used for synchronisation.

  int dead;
};


// Initialise a job queue with the given capacity.  The queue starts out
// empty.  Returns non-zero on error.
static inline int job_queue_init(struct job_queue *job_queue, int capacity) {
  job_queue->capacity = capacity;
  job_queue->first = 0;
  job_queue->num_used = 0;
  job_queue->dead = 0;
  job_queue->buffer = calloc(capacity, sizeof(void*));

  if (pthread_mutex_init(&job_queue->mutex, NULL) != 0) {
    return -1;
  }

  if (pthread_cond_init(&job_queue->cond, NULL) != 0) {
    return -1;
  }

  if (job_queue->buffer == NULL) {
    return -1;
  }

  return 0;
}

// Destroy the job queue.  Blocks until the queue is empty before it
// is destroyed.
static inline int job_queue_destroy(struct job_queue *job_queue) {
  assert(pthread_mutex_lock(&job_queue->mutex) == 0);

  while (job_queue->num_used != 0) {
    pthread_cond_wait(&job_queue->cond, &job_queue->mutex);
  }

  // Queue is now empty.  Let's kill it!
  job_queue->dead = 1;
  free(job_queue->buffer);
  pthread_cond_broadcast(&job_queue->cond);

  assert(pthread_mutex_unlock(&job_queue->mutex) == 0);

  return 0;
}

// Push an element onto the end of the job queue.  Blocks if the
// job_queue is full (its size is equal to its capacity).  Returns
// non-zero on error.  It is an error to push a job onto a queue that
// has been destroyed.
static inline int job_queue_push(struct job_queue *job_queue, void *data) {
  assert(pthread_mutex_lock(&job_queue->mutex) == 0);

  // Wait until there is room in the job_queue.
  while (job_queue->num_used == job_queue->capacity && !job_queue->dead) {
    pthread_cond_wait(&job_queue->cond, &job_queue->mutex);
  }

  if (job_queue->dead) {
    assert(pthread_mutex_unlock(&job_queue->mutex) == 0);
    return -1;
  }

  // If we made it past the loop, there is room in the job_queue.
  job_queue->buffer[(job_queue->first + job_queue->num_used) % job_queue->capacity] = (unsigned char*)data;
  job_queue->num_used++;

  // Signal a reader (if any) that there is now an element.
  pthread_cond_broadcast(&job_queue->cond);

  assert(pthread_mutex_unlock(&job_queue->mutex) == 0);

  return 0;
}

// Pop an element from the front of the job queue.  Blocks if the
// job_queue contains zero elements.  Returns non-zero on error.  If
// job_queue_destroy() has been called (possibly after the call to
// job_queue_pop() blocked), this function will return -1.
static inline int job_queue_pop(struct job_queue *job_queue, void **data) {
  assert(pthread_mutex_lock(&job_queue->mutex) == 0);

  // Wait until the job_queue contains an element.
  while (job_queue->num_used == 0 && !job_queue->dead) {
    pthread_cond_wait(&job_queue->cond, &job_queue->mutex);
  }

  if (job_queue->dead) {
    assert(pthread_mutex_unlock(&job_queue->mutex) == 0);
    return -1;
  }

  // We now know that num_used is nonzero.
  job_queue->num_used--;
  *data = job_queue->buffer[job_queue->first];
  job_queue->first = (job_queue->first + 1) % job_queue->capacity;

  // Signal a writer (if any) that there is now room for more.
  pthread_cond_broadcast(&job_queue->cond);

  assert(pthread_mutex_unlock(&job_queue->mutex) == 0);

  return 0;
}

#endif

// End of jobqueue.h
