// start of subtask_queue.h

#ifndef SUBTASK_QUEUE_H
#define SUBTASK_QUEUE_H

static inline struct subtask* setup_subtask(sub_task_fn fn,
                                            void* args,
                                            pthread_mutex_t *mutex,
                                            pthread_cond_t *cond,
                                            int* counter,
                                            int start, int end,
                                            int chunk, int id)
{
  struct subtask* subtask = malloc(sizeof(struct subtask));
  if (subtask == NULL) {
    assert(!"malloc failed in setup_subtask");
    return  NULL;
  }
  subtask->fn      = fn;
  subtask->args    = args;
  subtask->mutex   = mutex;
  subtask->cond    = cond;
  subtask->counter = counter;
  subtask->start   = start;
  subtask->end     = end;
  subtask->chunkable = chunk;
  // This should start at the value of minimum work pr. subtask
  subtask->iterations = chunk;
  subtask->id      = id;
  subtask->been_stolen = 0;
  return subtask;
}


/* Doubles the size of the queue */
static inline int subtask_queue_grow_queue(struct subtask_queue *subtask_queue) {

#ifdef MCDEBUG
  fprintf(stderr, "Growing queue to %d\n", subtask_queue->capacity * 2);
#endif

  int new_capacity = 2 * subtask_queue->capacity;
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

static inline struct subtask* jobqueue_get_subtask_chunk(struct worker *worker,
                                                         struct subtask_queue *subtask_queue,
                                                         int stealing)
{

  struct subtask *cur_head;
  if (stealing) {
    cur_head = subtask_queue->buffer[(subtask_queue->first + subtask_queue->num_used - 1) % subtask_queue->capacity];
  } else {
    cur_head = subtask_queue->buffer[subtask_queue->first];
  }

  if (stealing && cur_head->been_stolen) {
    return NULL;
  }

  int remaining_iter = cur_head->end - cur_head->start;
  assert(remaining_iter > 0);

  if (cur_head->chunkable && remaining_iter > cur_head->iterations)
  {
    /* fprintf(stderr, "%d - chuink %d - rem %d\n", pthread_self(), cur_head->chunk, remaining_iter); */
    struct subtask *new_subtask = malloc(sizeof(struct subtask));
    *new_subtask = *cur_head;

    // Update ranges on new subtasks
    if (stealing) {
      /* int new_iter = (remaining_iter > 1) ? remaining_iter / 2 : remaining_iter; */
      /* new_subtask->iterations = 1; */
      new_subtask->start = cur_head->end - cur_head->iterations;
      cur_head->end = new_subtask->start;
    } else {
      new_subtask->end = cur_head->start + cur_head->iterations;
      cur_head->start += cur_head->iterations;
      /* cur_head->iterations *= 2; */
    }
    new_subtask->id = worker->tid;

    CHECK_ERR(pthread_mutex_lock(cur_head->mutex), "pthread_mutex_lock");
    (*cur_head->counter)++;
    CHECK_ERR(pthread_mutex_unlock(cur_head->mutex), "pthread_mutex_unlock");

    return new_subtask;
  }

  if (stealing) {
    subtask_queue->num_used--;
  } else {
    subtask_queue->num_used--;
    subtask_queue->first = (subtask_queue->first + 1) % subtask_queue->capacity;
  }
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

  *subtask = jobqueue_get_subtask_chunk(worker, subtask_queue, 1);
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



/* Ask for a random subtask from another worker */
static inline int query_a_subtask(struct scheduler* scheduler,
                                  int tid,
                                  struct worker *worker,
                                  struct subtask **subtask)
{
  assert(scheduler != NULL);

  int worker_idx = fast_rand() % scheduler->num_threads;
  while (worker_idx == tid) {
    worker_idx = fast_rand() % scheduler->num_threads;
  }
  struct worker *rand_worker = &scheduler->workers[worker_idx];
  assert(rand_worker != NULL);
  int retval = subtask_queue_steal(rand_worker, subtask);
  if (retval == 0) { /* we found some work */
    assert(*subtask != NULL);
    (*subtask)->been_stolen = 1;
    /* CHECK_ERR(subtask_queue_enqueue(worker, subtask), "subtask_queue_enqueue"); */
    return 0;
  } else if (retval == 1) { /* Queue was not ready or no work found */
    return 1;
  } else { /* Queue failed else */
    return retval;
  }
}



// Pop an element from the front of the job queue.  Blocks if the
// subtask_queue contains zero elements.  Returns non-zero on error.  If
// subtask_queue_destroy() has been called (possibly after the call to
// subtask_queue_pop() blocked), this function will return -1.
static inline int subtask_queue_dequeue(struct worker *worker, struct subtask **subtask)
{
  assert(worker != NULL);
  struct subtask_queue *subtask_queue = &worker->q;

#ifdef MCPROFILE
  uint64_t start = get_wall_time();
#endif
  CHECK_ERR(pthread_mutex_lock(&subtask_queue->mutex), "pthread_mutex_lock");
  // Try to steal some work while the subtask_queue is empty
  while (subtask_queue->num_used == 0 && !subtask_queue->dead) {
    CHECK_ERR(pthread_mutex_unlock(&subtask_queue->mutex), "pthread_mutex_unlock");
    int retval = query_a_subtask(worker->scheduler, worker->tid, worker, subtask);
    if (retval == 0) { // We got a task - just return it
      return 0;
    } else if (retval == 1) { // we didn't find anything so go back to sleep
      CHECK_ERR(pthread_mutex_lock(&subtask_queue->mutex), "pthread_mutex_unlock");
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

  *subtask = jobqueue_get_subtask_chunk(worker, subtask_queue, 0);
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

/* TODO: Do I need to acquire locks here? */
static inline int subtask_queue_is_empty(struct subtask_queue *subtask_queue)
{
  return ((volatile int)subtask_queue->num_used) == 0 && !subtask_queue->dead;
}
#endif

// End of subtask_queue.h
