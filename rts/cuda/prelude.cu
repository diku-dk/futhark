// Start of prelude.cu

#define FUTHARK_CUDA
#define FUTHARK_F64_ENABLED

typedef char int8_t;
typedef short int16_t;
typedef int int32_t;
typedef long long int64_t;
typedef unsigned char uint8_t;
typedef unsigned short uint16_t;
typedef unsigned int uint32_t;
typedef unsigned long long uint64_t;
typedef uint8_t uchar;
typedef uint16_t ushort;
typedef uint32_t uint;
typedef uint64_t ulong;
#define __kernel extern "C" __global__ __launch_bounds__(MAX_THREADS_PER_BLOCK)
#define __global
#define __local
#define __private
#define __constant
#define __write_only
#define __read_only

static inline int get_group_id_fn(int block_dim0, int block_dim1, int block_dim2, int d) {
  switch (d) {
    case 0: d = block_dim0; break;
    case 1: d = block_dim1; break;
    case 2: d = block_dim2; break;
  }
  switch (d) {
    case 0: return blockIdx.x;
    case 1: return blockIdx.y;
    case 2: return blockIdx.z;
    default: return 0;
  }
}
#define get_group_id(d) get_group_id_fn(block_dim0, block_dim1, block_dim2, d)

static inline int get_num_groups_fn(int block_dim0, int block_dim1, int block_dim2, int d) {
  switch (d) {
    case 0: d = block_dim0; break;
    case 1: d = block_dim1; break;
    case 2: d = block_dim2; break;
  }
  switch(d) {
    case 0: return gridDim.x;
    case 1: return gridDim.y;
    case 2: return gridDim.z;
    default: return 0;
  }
}
#define get_num_groups(d) get_num_groups_fn(block_dim0, block_dim1, block_dim2, d)

static inline int get_local_id(int d) {
  switch (d) {
    case 0: return threadIdx.x;
    case 1: return threadIdx.y;
    case 2: return threadIdx.z;
    default: return 0;
  }
}

static inline int get_local_size(int d) {
  switch (d) {
    case 0: return blockDim.x;
    case 1: return blockDim.y;
    case 2: return blockDim.z;
    default: return 0;
  }
}

#define CLK_LOCAL_MEM_FENCE 1
#define CLK_GLOBAL_MEM_FENCE 2
static inline void barrier(int x) {
  __syncthreads();
}
static inline void mem_fence_local() {
  __threadfence_block();
}
static inline void mem_fence_global() {
  __threadfence();
}

static inline void barrier_local() {
  __syncthreads();
}

#define NAN (0.0/0.0)
#define INFINITY (1.0/0.0)
extern volatile __shared__ unsigned char shared_mem[];


// End of prelude.cu
