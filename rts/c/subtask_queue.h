// start of subtask_queue.h

#ifndef SUBTASK_QUEUE_H
#define SUBTASK_QUEUE_H

/* Doubles the size of the queue */
static inline int subtask_queue_grow_queue(struct subtask_queue *subtask_queue) {

  int new_capacity = 2 * subtask_queue->capacity;
#ifdef MCDEBUG
  fprintf(stderr, "Growing queue to %d\n", subtask_queue->capacity * 2);
#endif

  struct subtask **new_buffer = calloc(new_capacity, sizeof(struct subtask*));
  for (int i = 0; i < subtask_queue->num_used; i++) {
    new_buffer[i] = subtask_queue->buffer[(subtask_queue->first + i) % subtask_queue->capacity];
  }

  free(subtask_queue->buffer);
  subtask_queue->buffer = new_buffer;
  subtask_queue->capacity = new_capacity;
  subtask_queue->first = 0;

  return 0;
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

static inline void dump_queue(struct worker *worker)
{
  struct subtask_queue *subtask_queue = &worker->q;
  CHECK_ERR(pthread_mutex_lock(&subtask_queue->mutex), "pthread_mutex_lock");
  for (int i = 0; i < subtask_queue->num_used; i++) {
    struct subtask * subtask = subtask_queue->buffer[(subtask_queue->first + i) % subtask_queue->capacity];
    printf("queue tid %d with %d task %s\n", worker->tid, i, subtask->name);
  }
  CHECK_ERR(pthread_cond_broadcast(&subtask_queue->cond), "pthread_cond_broadcast");
  CHECK_ERR(pthread_mutex_unlock(&subtask_queue->mutex), "pthread_mutex_unlock");
}

// Push an element onto the end of the job queue.  Blocks if the
// subtask_queue is full (its size is equal to its capacity).  Returns
// non-zero on error.  It is an error to push a job onto a queue that
// has been destroyed.
static inline int subtask_queue_enqueue(struct worker *worker, struct subtask *subtask )
{
  assert(worker != NULL);
  struct subtask_queue *subtask_queue = &worker->q;

#ifdef MCPROFILE
  uint64_t start = get_wall_time();
#endif

  CHECK_ERR(pthread_mutex_lock(&subtask_queue->mutex), "pthread_mutex_lock");
  // Wait until there is room in the subtask_queue.
  while (subtask_queue->num_used == subtask_queue->capacity && !subtask_queue->dead) {
    if (subtask_queue->num_used == subtask_queue->capacity) {
      CHECK_ERR(subtask_queue_grow_queue(subtask_queue), "subtask_queue_grow_queue");
      continue;
    }
    CHECK_ERR(pthread_cond_wait(&subtask_queue->cond, &subtask_queue->mutex), "pthread_cond_wait");
  }

  if (subtask_queue->dead) {
    CHECK_ERR(pthread_mutex_unlock(&subtask_queue->mutex), "pthread_mutex_unlock");
    return -1;
  }

  // If we made it past the loop, there is room in the subtask_queue.
  subtask_queue->buffer[(subtask_queue->first + subtask_queue->num_used) % subtask_queue->capacity] = subtask;
  subtask_queue->num_used++;

#ifdef MCPROFILE
  uint64_t end = get_wall_time();
  subtask_queue->time_enqueue += (end - start);
  subtask_queue->n_enqueues++;
#endif
  // Broadcast a reader (if any) that there is now an element.
  CHECK_ERR(pthread_cond_broadcast(&subtask_queue->cond), "pthread_cond_broadcast");
  CHECK_ERR(pthread_mutex_unlock(&subtask_queue->mutex), "pthread_mutex_unlock");

  return 0;
}


/* Like subtask_queue_dequeue, but with two differences:
   1) the subtask is stolen from the __front__ of the queue
   2) returns immediately if there is no subtasks queued,
      as we dont' want to block on another workers queue and
*/
static inline int subtask_queue_steal(struct worker *worker,
                                      struct subtask **subtask)
{
  struct subtask_queue *subtask_queue = &worker->q;
  assert(subtask_queue != NULL);

#ifdef MCPROFILE
  uint64_t start = get_wall_time();
#endif
  CHECK_ERR(pthread_mutex_lock(&subtask_queue->mutex), "pthread_mutex_lock");

  if (subtask_queue->num_used == 0) {
    CHECK_ERR(pthread_cond_broadcast(&subtask_queue->cond), "pthread_cond_broadcast");
    CHECK_ERR(pthread_mutex_unlock(&subtask_queue->mutex), "pthread_mutex_unlock");
    return 1;
  }

  if (subtask_queue->dead) {
    CHECK_ERR(pthread_mutex_unlock(&subtask_queue->mutex), "pthread_mutex_unlock");
    return -1;
  }

  // Tasks gets stolen from the "front"
  struct subtask *cur_back = subtask_queue->buffer[subtask_queue->first];
  struct subtask *new_subtask = NULL;
  int remaining_iter = cur_back->end - cur_back->start;
  // If subtask is chunkable, we steal half of the iterations
  if (cur_back->chunkable && remaining_iter > 1) {
      int64_t half = remaining_iter / 2;
      new_subtask = malloc(sizeof(struct subtask));
      *new_subtask = *cur_back;
      new_subtask->start = cur_back->end - half;
      cur_back->end = new_subtask->start;
      __atomic_fetch_add(cur_back->counter, 1, __ATOMIC_RELAXED);
  } else {
    new_subtask = cur_back;
    subtask_queue->num_used--;
    subtask_queue->first = (subtask_queue->first + 1) % subtask_queue->capacity;
  }
  *subtask = new_subtask;

  if (*subtask == NULL) {
    CHECK_ERR(pthread_mutex_unlock(&subtask_queue->mutex), "pthred_mutex_unlock");
    return 1;
  }

#ifdef MCPROFILE
  uint64_t end = get_wall_time();
  subtask_queue->time_dequeue += (end - start);
  subtask_queue->n_dequeues++;
#endif

  // Broadcast a writer (if any) that there is now room for more.
  CHECK_ERR(pthread_cond_broadcast(&subtask_queue->cond), "pthread_cond_broadcast");
  CHECK_ERR(pthread_mutex_unlock(&subtask_queue->mutex), "pthread_mutex_unlock");

  return 0;
}


// Pop an element from the back of the job queue.
// Optional argument can be provided to block or not
static inline int subtask_queue_dequeue(struct worker *worker,
                                        struct subtask **subtask, int blocking)
{
  assert(worker != NULL);
  struct subtask_queue *subtask_queue = &worker->q;

#ifdef MCPROFILE
  uint64_t start = get_wall_time();
#endif

  CHECK_ERR(pthread_mutex_lock(&subtask_queue->mutex), "pthread_mutex_lock");
  if (subtask_queue->num_used == 0 && !blocking) {
    CHECK_ERR(pthread_mutex_unlock(&subtask_queue->mutex), "pthread_mutex_unlock");
    return 1;
  }
  // Try to steal some work while the subtask_queue is empty
  while (subtask_queue->num_used == 0 && !subtask_queue->dead) {
    pthread_cond_wait(&subtask_queue->cond, &subtask_queue->mutex);
  }

  if (subtask_queue->dead) {
    CHECK_ERR(pthread_mutex_unlock(&subtask_queue->mutex), "pthread_mutex_unlock");
    return -1;
  }

  // dequeue pops from the back
  *subtask = subtask_queue->buffer[(subtask_queue->first + subtask_queue->num_used - 1) % subtask_queue->capacity];
  subtask_queue->num_used--;

  if (*subtask == NULL) {
    assert(!"got NULL ptr");
    CHECK_ERR(pthread_mutex_unlock(&subtask_queue->mutex), "pthred_mutex_unlock");
    return -1;
  }

#ifdef MCPROFILE
  uint64_t end = get_wall_time();
  subtask_queue->time_dequeue += (end - start);
  subtask_queue->n_dequeues++;
#endif

  // Broadcast a writer (if any) that there is now room for more.
  CHECK_ERR(pthread_cond_broadcast(&subtask_queue->cond), "pthread_cond_broadcast");
  CHECK_ERR(pthread_mutex_unlock(&subtask_queue->mutex), "pthread_mutex_unlock");

  return 0;
}

static inline int subtask_queue_is_empty(struct subtask_queue *subtask_queue)
{
  return subtask_queue->num_used == 0;
}


#endif

// End of subtask_queue.h
