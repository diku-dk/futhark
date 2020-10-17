// Start of chaselev.h

/* Implementation of Chase-lev's concurrent lock-free deque
   from ``Dynamic Circular Work-Stealing Deque`` (2005)
   This implementation was ported from
   https://github.com/deepsea-inria/heartbeat

   !!!
   This implementation leaks memory,
   if the circular array is grown
   as we don't maintain a list of the old buffers.
   However, we can't safely free it either as a stealing thread might
   be reading from it.
   !!!
 */

#if defined(MCCHASELEV)
#include <stdlib.h>
#include <assert.h>
#include <string.h>


static struct subtask* const STEAL_RES_EMPTY = (struct subtask*) 0;
static struct subtask* const STEAL_RES_ABORT = (struct subtask*) 1;

static const int strong = 0;
static const int backoff_nb_cycles = 1l << 10;


static inline struct subtask* cb_get(struct subtask **buf, int64_t capacity, int64_t i)  {
  return (struct subtask*)__atomic_load_n(&buf[i % capacity], __ATOMIC_RELAXED);
}

static inline void cb_put (struct subtask **buf, int64_t capacity, int64_t i, struct subtask* x) {
  __atomic_store_n(&buf[i % capacity], x, __ATOMIC_RELAXED);
}

struct deque_buffer* grow(struct subtask **old_array,
                          int64_t old_capacity,
                          int64_t new_capacity,
                          int64_t b,
                          int64_t t)
{
  struct deque_buffer* new_deque_buffer = malloc(sizeof(struct deque_buffer));
  new_deque_buffer->size = new_capacity;
  new_deque_buffer->array = calloc(new_capacity,  sizeof(struct subtask*));

  for (int64_t i = t; i < b; i++) {
    cb_put(new_deque_buffer->array, new_capacity, i, cb_get(old_array, old_capacity, i));
  }
  return new_deque_buffer;
}

static inline int deque_init(struct deque *q, int64_t capacity) {
  assert(q != NULL);
  memset(q, 0, sizeof(struct deque));

  q->buffer = malloc(sizeof(struct deque_buffer));
  q->buffer->array = calloc(capacity, sizeof(struct subtask*));
  q->buffer->size = capacity;

  q->dead = 0;

  if (q->buffer->array == NULL) {
    return -1;
  }

  if (q->buffer == NULL) {
    return -1;
  }
  return 0;
}

static inline void deque_destroy(struct deque* q)
{
  q->dead = 1;
  free(q->buffer->array);
  free(q->buffer);
}

static inline int cas_top (struct deque *q, int64_t old_val, int64_t new_val) {
  int64_t ov = old_val;
  if(__atomic_compare_exchange_n(&q->top, &ov, new_val, strong,
                                 __ATOMIC_SEQ_CST, __ATOMIC_RELAXED)) {
    return 1;
  }
  spin_for(backoff_nb_cycles);
  return 0;
}


void push_back(struct deque *q, struct subtask*subtask)
{
  assert(subtask != NULL);
  assert(q != NULL);

  int64_t b = __atomic_load_n(&q->bottom, __ATOMIC_RELAXED); // load atomically
  int64_t t = __atomic_load_n(&q->top, __ATOMIC_ACQUIRE);    // load atomically
  struct deque_buffer *buffer = __atomic_load_n(&q->buffer, __ATOMIC_RELAXED);
  if (b-t >= (buffer->size - 1)) {
    // grow_queue
    struct subtask **old_array = buffer->array;
    int64_t old_capacity = __atomic_load_n(&buffer->size, __ATOMIC_RELAXED);
    int64_t new_capacity = old_capacity * 2;
    struct deque_buffer *new_buffer = grow(old_array, old_capacity, new_capacity, b, t);
    __atomic_store_n(&q->buffer, new_buffer, __ATOMIC_RELEASE);
    buffer = __atomic_load_n(&q->buffer, __ATOMIC_RELAXED);
    memset(old_array, 0, sizeof(struct subtask*) * old_capacity);
    // free(old_array);  Not safe!!
  }

  cb_put(buffer->array, buffer->size, b, subtask);
  __atomic_thread_fence(__ATOMIC_RELEASE);
  __atomic_store_n(&q->bottom, b+1, __ATOMIC_RELAXED);
  return;
}


struct subtask * pop_back(struct deque *q)
{
  int64_t b = __atomic_load_n(&q->bottom, __ATOMIC_RELAXED) - 1; // load atomically
  struct deque_buffer *buffer = __atomic_load_n(&q->buffer, __ATOMIC_RELAXED);
  __atomic_store_n(&q->bottom, b, __ATOMIC_RELAXED);
	__atomic_thread_fence(__ATOMIC_SEQ_CST);
  int64_t t = __atomic_load_n(&q->top, __ATOMIC_RELAXED);
  if (b < t) {
    __atomic_store_n(&q->bottom, t, __ATOMIC_RELAXED);
    return NULL;
  }
  struct subtask* item = cb_get(buffer->array, buffer->size, b);
  if (b > t) {
    return item;
  }

  // else there's only one item left
  // Did we win the race?
  if (!cas_top(q, t, t + 1)) {
    item = NULL;
  }
  __atomic_store_n(&q->bottom, t+1, __ATOMIC_RELAXED);
  return item;
}

struct subtask* steal(struct deque *q)
{
  assert(q != NULL);

  int64_t t = __atomic_load_n(&q->top, __ATOMIC_ACQUIRE);    // load atomically
  __atomic_thread_fence(__ATOMIC_SEQ_CST);
  int64_t b = __atomic_load_n(&q->bottom, __ATOMIC_ACQUIRE); // load atomically
  if (t >= b) {
    return STEAL_RES_EMPTY;
  }

  struct deque_buffer *buffer = __atomic_load_n(&q->buffer, __ATOMIC_CONSUME);
  struct subtask* item = cb_get(buffer->array, buffer->size, t);
  if (!cas_top(q, t, t + 1)) {
    return STEAL_RES_ABORT;
  }

  return item;
}


static inline size_t nb_subtasks(struct deque *q)
{
  return (size_t)__atomic_load_n(&q->bottom, __ATOMIC_RELAXED) - __atomic_load_n(&q->top, __ATOMIC_RELAXED);
}

static inline int empty(struct deque *q)
{
  return nb_subtasks(q) < 1;
}

#endif
// end of chaselev.h
