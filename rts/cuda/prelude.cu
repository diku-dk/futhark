// start of prelude.cu

#define SCALAR_FUN_ATTR __device__ static inline
#define FUTHARK_FUN_ATTR __device__ static
#define FUTHARK_F64_ENABLED

typedef char int8_t;
typedef short int16_t;
typedef int int32_t;
typedef long long int64_t;
typedef unsigned char uint8_t;
typedef unsigned short uint16_t;
typedef unsigned int uint32_t;
typedef unsigned long long uint64_t;

#define __global
#define __local
#define __private
#define __constant
#define __write_only
#define __read_only

static inline __device__ int get_group_id(int d) {
  switch (d) {
  case 0: return blockIdx.x;
  case 1: return blockIdx.y;
  case 2: return blockIdx.z;
  default: return 0;
  }
}

static inline __device__ int get_num_groups(int d) {
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

#define NAN (0.0/0.0)
#define INFINITY (1.0/0.0)
extern volatile __shared__ unsigned char local_mem[];

#define LOCAL_MEM_PARAM
#define FUTHARK_KERNEL extern "C" __global__ __launch_bounds__(MAX_THREADS_PER_BLOCK)
#define FUTHARK_KERNEL_SIZED(a,b,c) extern "C" __global__ __launch_bounds__(a*b*c)

// End of prelude.cu
