// start of multicore_util.h

/* Multicore Utility functions */

#ifndef _MULTICORE_UTIL_H_
#define _MULTICORE_UTIL_H_

/* A wrapper for getting rusage on Linux and MacOS */
/* TODO maybe figure out this for windows */
static inline int getrusage_thread(struct rusage *rusage)
{
  int err = -1;
#if  defined(__APPLE__)
    thread_basic_info_data_t info = { 0 };
    mach_msg_type_number_t info_count = THREAD_BASIC_INFO_COUNT;
    kern_return_t kern_err;

    kern_err = thread_info(mach_thread_self(),
                           THREAD_BASIC_INFO,
                           (thread_info_t)&info,
                           &info_count);
    if (kern_err == KERN_SUCCESS) {
        memset(rusage, 0, sizeof(struct rusage));
        rusage->ru_utime.tv_sec = info.user_time.seconds;
        rusage->ru_utime.tv_usec = info.user_time.microseconds;
        rusage->ru_stime.tv_sec = info.system_time.seconds;
        rusage->ru_stime.tv_usec = info.system_time.microseconds;
        err = 0;
    } else {
        errno = EINVAL;
    }
#elif defined(__linux__)
    err = getrusage(RUSAGE_THREAD, rusage);
#endif
    return err;
}

/* returns the number of logical cores */
static const int num_processors()
{
#if  defined(_WIN32)
/* https://docs.microsoft.com/en-us/windows/win32/api/sysinfoapi/ns-sysinfoapi-system_info */
    SYSTEM_INFO sysinfo;
    GetSystemInfo(&sysinfo);
    int ncores = sysinfo.dwNumberOfProcessors;
    fprintf(stderr, "Found %d cores on your Windows machine\n Is that correct?\n", ncores);
    return ncores;
#elif defined(__APPLE__)
    int ncores;
    size_t ncores_size = sizeof(ncores);
    CHECK_ERRNO(sysctlbyname("hw.logicalcpu", &ncores, &ncores_size, NULL, 0),
                "sysctlbyname (hw.logicalcpu)");
    return ncores;
#elif defined(__linux__)
  return get_nprocs();
#else
  fprintf(stderr, "operating system not recognised\n");
  return -1;
#endif
}

static inline void output_thread_usage(struct worker *worker)
{
  struct rusage usage;
  CHECK_ERRNO(getrusage_thread(&usage), "getrusage_thread");
  struct timeval user_cpu_time = usage.ru_utime;
  struct timeval sys_cpu_time = usage.ru_stime;
  fprintf(stderr, "tid: %2d - work time %10llu - user time: %10llu us - sys: %10llu us\n",
          worker->tid,
          worker->time_spent_working,
          (uint64_t)(user_cpu_time.tv_sec * 1000000 + user_cpu_time.tv_usec),
          (uint64_t)(sys_cpu_time.tv_sec * 1000000 + sys_cpu_time.tv_usec));
}


static unsigned int g_seed;

// Used to seed the generator.
static inline void fast_srand(unsigned int seed) {
    g_seed = seed;
}

// Compute a pseudorandom integer.
// Output value in range [0, 32767]
static inline unsigned int fast_rand(void) {
    g_seed = (214013*g_seed+2531011);
    return (g_seed>>16)&0x7FFF;
}


#endif
// end of multicore_util.h
