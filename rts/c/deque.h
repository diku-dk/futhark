
#include <stdlib.h>
#include <assert.h>
#include <string.h>



#define QUEUE_EMPTY NULL
#define QUEUE_ABORT NULL
static struct subtask* const STEAL_RES_EMPTY = (struct subtask*) 0;
static struct subtask* const STEAL_RES_ABORT = (struct subtask*) 1;

static const int mem_model = __ATOMIC_SEQ_CST;
static const int strong = 0;


static inline struct subtask* cb_get(struct subtask **buf, int64_t capacity, int64_t i)  {
  return (struct subtask*)__atomic_load_n(&buf[i % capacity], mem_model);
}

static inline void cb_put (struct subtask **buf, int64_t capacity, int64_t i, struct subtask* x) {
  __atomic_store_n(&buf[i % capacity], x, mem_model);
}

static inline int stealable(struct subtask **buf, int64_t capacity, int64_t i) {
  return __atomic_load_n(&buf[i % capacity]->been_stolen, mem_model);
}


struct subtask ** grow(struct subtask **old_buf,
                       int64_t old_capacity,
                       int64_t new_capacity,
                       int64_t b,
                       int64_t t) {
  struct subtask ** new_buf = calloc(new_capacity, sizeof(struct subtask*));
  for (int64_t i = t; i < b; i++) {
    cb_put(new_buf, new_capacity, i, cb_get(old_buf, old_capacity, i));
  }
  return new_buf;
}

static inline int deque_init(struct deque *q, int capacity) {
  assert(q != NULL);
  memset(q, 0, sizeof(struct deque));

  q->size = capacity;
  q->buffer = calloc(capacity, sizeof(struct subtask*));

  if (q->buffer == NULL) {
    return -1;
  }
  return 0;
}

static inline int cas_top (struct deque *q, int64_t old_val, int64_t new_val) {
  int64_t ov = old_val;
  return __atomic_compare_exchange_n(&q->top, &ov, new_val, strong, mem_model, mem_model);
}


static inline void pushBottom(struct deque *q, struct subtask*subtask)
{
  assert(subtask != NULL);
  assert(q != NULL);
  int64_t b = __atomic_load_n(&q->bottom, mem_model); // load atomically
  int64_t t = __atomic_load_n(&q->top, mem_model);    // load atomically
  int64_t size = b - t;
  if (size >= (q->size - 1)) {
    // grow_queue
    struct subtask **old_buf = q->buffer;
    int64_t old_capacity = __atomic_load_n(&q->size, mem_model);
    int64_t new_capacity = __atomic_load_n(&q->size, mem_model) * 2;
    __atomic_store_n(&q->buffer, grow(old_buf, old_capacity, new_capacity, b, t), mem_model);
    __atomic_store_n(&q->size, new_capacity, mem_model);
  }
  cb_put(q->buffer, q->size , b, subtask);
  __atomic_store_n(&q->bottom, b+1, mem_model);
  return;
}

// also called popTop
static inline struct subtask* steal(struct deque *q) {
  assert(q != NULL);
  int64_t b = __atomic_load_n(&q->bottom, mem_model); // load atomically
  int64_t t = __atomic_load_n(&q->top, mem_model);    // load atomically

  if (t >= b) {
    return STEAL_RES_EMPTY;
  }
  if (stealable(q->buffer, q->size, t)) {
    return STEAL_RES_ABORT;
  }
  struct subtask* item = cb_get(q->buffer, __atomic_load_n(&q->size, mem_model), t);
  /* if (item->been_stolen) { */
  /*   return STEAL_RES_ABORT; */
  /* } */

  if (!cas_top(q, t, t + 1)) {
    return STEAL_RES_ABORT;
  }


  return item;
}


static inline struct subtask * popBottom(struct deque *q) {
  int64_t b = __atomic_load_n(&q->bottom, mem_model) - 1; // load atomically
  __atomic_store_n(&q->bottom, b, mem_model);

  int64_t t = __atomic_load_n(&q->top, mem_model);
  if (b < t) {
    __atomic_store_n(&q->bottom, t, mem_model);
    return NULL;
  }
  struct subtask* item = cb_get(q->buffer, __atomic_load_n(&q->size, mem_model), b);
  if (b > t) {
    return item;
  }

  if (!cas_top(q, t, t + 1)) {
    item = NULL;
  }
  __atomic_store_n(&q->bottom, t+1, mem_model);
  return item;
}

size_t nb_threads(struct deque *q) {
  return (size_t)__atomic_load_n(&q->bottom, mem_model) - __atomic_load_n(&q->top, mem_model);
}

int empty(struct deque *q) {
  return nb_threads(q) < 1;
}

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
  subtask->chunkable   = chunk;
  // This should start at the value of minimum work pr. subtask
  subtask->iterations = chunk;
  subtask->been_stolen = 0;
  subtask->id      = id;
  return subtask;
}
