// start of prelude.cu

#define SCALAR_FUN_ATTR __device__ static inline
#define FUTHARK_FUN_ATTR __device__ static
#define FUTHARK_F64_ENABLED

typedef __int8_t int8_t;
typedef __int16_t int16_t;
typedef __int32_t int32_t;
typedef __int64_t int64_t;
typedef __uint8_t uint8_t;
typedef __uint16_t uint16_t;
typedef __uint32_t uint32_t;
typedef __uint64_t uint64_t;

#define __global
#define __local
#define __private
#define __constant
#define __write_only
#define __read_only

static inline __device__ int get_tblock_id(int d) {
  switch (d) {
  case 0: return blockIdx.x;
  case 1: return blockIdx.y;
  case 2: return blockIdx.z;
  default: return 0;
  }
}

static inline __device__ int get_num_tblocks(int d) {
  switch(d) {
  case 0: return gridDim.x;
  case 1: return gridDim.y;
  case 2: return gridDim.z;
  default: return 0;
  }
}

static inline __device__ int get_global_id(int d) {
  switch (d) {
    case 0: return threadIdx.x + blockIdx.x * blockDim.x;
    case 1: return threadIdx.y + blockIdx.y * blockDim.y;
    case 2: return threadIdx.z + blockIdx.z * blockDim.z;
    default: return 0;
  }
}

static inline __device__ int get_local_id(int d) {
  switch (d) {
    case 0: return threadIdx.x;
    case 1: return threadIdx.y;
    case 2: return threadIdx.z;
    default: return 0;
  }
}

static inline __device__ int get_local_size(int d) {
  switch (d) {
    case 0: return blockDim.x;
    case 1: return blockDim.y;
    case 2: return blockDim.z;
    default: return 0;
  }
}

static inline __device__ int get_global_size(int d) {
  switch (d) {
    case 0: return gridDim.x * blockDim.x;
    case 1: return gridDim.y * blockDim.y;
    case 2: return gridDim.z * blockDim.z;
    default: return 0;
  }
}


#define CLK_LOCAL_MEM_FENCE 1
#define CLK_GLOBAL_MEM_FENCE 2
static inline __device__ void barrier(int x) {
  __syncthreads();
}
static inline __device__ void mem_fence_local() {
  __threadfence_block();
}
static inline __device__ void mem_fence_global() {
  __threadfence();
}

static inline __device__ void barrier_local() {
  __syncthreads();
}

#ifndef NAN
#define NAN (0.0/0.0)
#endif
#ifndef INFINITY
#define INFINITY (1.0/0.0)
#endif
extern volatile __shared__ unsigned char shared_mem[];

#define SHARED_MEM_PARAM
#define FUTHARK_KERNEL extern "C" __global__ __launch_bounds__(MAX_THREADS_PER_BLOCK)
#define FUTHARK_KERNEL_SIZED(a,b,c) extern "C" __global__ __launch_bounds__(a*b*c)

// End of prelude.cu
