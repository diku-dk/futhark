// start of scheduler.h

#ifndef SCHEDULER_H
#define SCHEDULER_H


#define MULTICORE



typedef int (*task_fn)(void*, int, int);

struct task {
  task_fn fn;
  void* args;
  int start, end;
  pthread_mutex_t *mutex;
  pthread_cond_t *cond;
  int *counter;
};


static inline void *futhark_worker(void* arg) {
  struct futhark_context *ctx = (struct futhark_context*) arg;
  while(1) {
    struct task *task;
    if (job_queue_pop(futhark_context_get_jobqueue(ctx), (void**)&task) == 0) {
       task->fn(task->args, task->start, task->end);
       pthread_mutex_lock(task->mutex);
       (*task->counter)--;
       pthread_cond_signal(task->cond);
       pthread_mutex_unlock(task->mutex);
       free(task);
    } else {
       break;
    }
  }
  return NULL;
}

/* TODO */
/* actually partition operation into subtasks */
/* e.g. based on num_threads or ?? */
static inline int scheduler_do_task(struct futhark_context *ctx, task_fn fn,
                          void* task_args, int iterations)
{
  pthread_mutex_t mutex;
  if (pthread_mutex_init(&mutex, NULL) != 0) {
     fprintf(stderr, "got error from pthread_mutex_init: %s\n", strerror(errno));
     return 1;
  }
  pthread_cond_t cond;
  if (pthread_cond_init(&cond, NULL) != 0) {
     fprintf(stderr, "got error from pthread_cond_init: %s\n", strerror(errno));
     return 1;
  }

  int counter = 1;

  struct task* task = malloc(sizeof(struct task));
  task->fn      = fn;
  task->args    = task_args;
  task->mutex   = &mutex;
  task->cond    = &cond;
  task->counter = &counter;
  task->start   = 0;
  task->end     = iterations;

  job_queue_push(futhark_context_get_jobqueue(ctx), (void*)task);

  // Join (wait for tasks to finish)
  pthread_mutex_lock(&mutex);
  while (counter != 0) {
    pthread_cond_wait(task->cond, task->mutex);
  }

  // destroy mutex/cond here


}


#endif


// End of scheduler.h
