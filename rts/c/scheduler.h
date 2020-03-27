// start of scheduler.h

#ifndef SCHEDULER_H
#define SCHEDULER_H


#define MULTICORE


const int num_threads = 4;

typedef int (*task_fn)(void*, int, int, int);

struct task {
  task_fn fn;
  void* args;
  int start, end;
  int task_id; // or a subtask id

  int *counter; // Counter ongoing subtasks
  pthread_mutex_t *mutex;
  pthread_cond_t *cond;
};


enum OP {
  SegMap,
  SegRed,
};

static inline void *futhark_worker(void* arg) {
  struct job_queue *q = (struct job_queue*) arg;
  while(1) {
    struct task *task;
    if (job_queue_pop(q, (void**)&task) == 0) {
      task->fn(task->args, task->start, task->end, task->task_id);
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



static inline struct task* setup_task(task_fn fn, void* task_args, int task_id,
                                      pthread_mutex_t *mutex, pthread_cond_t *cond,
                                      int* counter, int start, int end) {

  // Don't allocate this on heap, use stack!
    struct task* task = malloc(sizeof(struct task));
    task->fn      = fn;
    task->args    = task_args;
    task->task_id = task_id;
    task->mutex   = mutex;
    task->cond    = cond;
    task->counter = counter;
    task->start   = start;
    task->end     = end;
    return task;
}


static inline int scheduler_do_task(struct futhark_context *ctx,
                                    task_fn fn, void* task_args,
                                    int iterations, int *ntask)
{
  if (iterations == 0) {
    if (ntask != NULL)  *ntask = 0;
    return 0;
  }

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

  int task_id = 0;
  int shared_counter = 0;
  int iter_pr_task = iterations / num_threads;
  int remainder = iterations % num_threads;

  struct task *task = setup_task(fn, task_args, task_id,
                                 &mutex, &cond, &shared_counter,
                                 0, remainder + iter_pr_task);
  task_id++;
  pthread_mutex_lock(&mutex);
  shared_counter++;
  pthread_mutex_unlock(&mutex);
  job_queue_push(futhark_context_get_jobqueue(ctx), (void*)task);


  for (int i = remainder + iter_pr_task; i < iterations; i += iter_pr_task)
  {
    struct task *task = setup_task(fn, task_args, task_id,
                                   &mutex, &cond, &shared_counter,
                                   i, i + iter_pr_task);
    task_id++;

    pthread_mutex_lock(&mutex);
    shared_counter++;
    pthread_mutex_unlock(&mutex);
    job_queue_push(futhark_context_get_jobqueue(ctx), (void*)task);
  }


  // Join (wait for tasks to finish)
  pthread_mutex_lock(&mutex);
  while (shared_counter != 0) {
    pthread_cond_wait(&cond, &mutex);
  }

  if (ntask != NULL) {
    *ntask = task_id;
  }

  return 0;
}


#endif


// End of scheduler.h
