// Start of timing.h.

// The function get_wall_time() returns the wall time in microseconds
// (with an unspecified offset).

#ifdef _WIN32

#include <windows.h>

static int64_t get_wall_time(void) {
  LARGE_INTEGER time,freq;
  assert(QueryPerformanceFrequency(&freq));
  assert(QueryPerformanceCounter(&time));
  return ((double)time.QuadPart / freq.QuadPart) * 1000000;
}

#else
// Assuming POSIX

#include <time.h>
#include <sys/time.h>

static int64_t get_wall_time(void) {
  struct timeval time;
  assert(gettimeofday(&time,NULL) == 0);
  return time.tv_sec * 1000000 + time.tv_usec;
}

static inline uint64_t rdtsc() {
  unsigned int hi, lo;
  __asm__ __volatile__("rdtsc" : "=a"(lo), "=d"(hi));
  return  ((uint64_t) lo) | (((uint64_t) hi) << 32);
}

static int64_t get_wall_time_ns(void) {
  struct timespec time;
  assert(clock_gettime(CLOCK_REALTIME, &time) == 0);
  return time.tv_sec * 1000000000 + time.tv_nsec;
}


// This does not return a consistent CPU frequency
// as cpus might turbo boost or idle
// As such don't use for now
/* double cpu_frequency_ghz = 3.6; */

/* void initialize_cpuinfo() { */
/*   float cpu_frequency_mhz = 0.0; */
/* #ifdef __linux__ */
/*   /\* Get information from /proc/cpuinfo.     * */
/*    * cpu MHz         : <float>             # cpu frequency in MHz */
/*    *\/ */
/*   FILE *cpuinfo_file = fopen("/proc/cpuinfo", "r"); */
/*   char buf[1024]; */
/*   int cache_line_szb; */
/*   if (cpuinfo_file != NULL) { */
/*     while (fgets(buf, sizeof(buf), cpuinfo_file) != 0) { */
/*       sscanf(buf, "cpu MHz : %f", &(cpu_frequency_mhz)); */
/*     } */
/*     fclose (cpuinfo_file); */
/*   } */
/* #endif */
/* #ifdef __APPLE__ */
/*   uint64_t freq = 0; */
/*   size_t size; */
/*   size = sizeof(freq); */
/*   if (sysctlbyname("hw.cpufrequency", &freq, &size, NULL, 0) < 0) { */
/*     perror("sysctl"); */
/*   } */
/*   cpu_frequency_mhz = (float)freq / 1000000.; */
/* #endif */
/*   if (cpu_frequency_mhz == 0.) { */
/*     assert(0); */
/*   } */
/*   cpu_frequency_ghz = (double) (cpu_frequency_mhz / 1000.0); */
/* } */

/* static inline int64_t get_wall_time_ns() { */
/*   return (int64_t)rdtsc()/cpu_frequency_ghz; */
/* } */


#endif

// End of timing.h.
