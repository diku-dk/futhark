// Start of prelude.cl

#define SCALAR_FUN_ATTR static inline
#define FUTHARK_FUN_ATTR static

typedef char int8_t;
typedef short int16_t;
typedef int int32_t;
typedef long int64_t;

typedef uchar uint8_t;
typedef ushort uint16_t;
typedef uint uint32_t;
typedef ulong uint64_t;

#define get_tblock_id(d) get_group_id(d)
#define get_num_tblocks(d) get_num_groups(d)

// Clang-based OpenCL implementations need this for 'static' to work.
#ifdef cl_clang_storage_class_specifiers
#pragma OPENCL EXTENSION cl_clang_storage_class_specifiers : enable
#endif
#pragma OPENCL EXTENSION cl_khr_byte_addressable_store : enable

#ifdef FUTHARK_F64_ENABLED
#pragma OPENCL EXTENSION cl_khr_fp64 : enable
#endif

#pragma OPENCL EXTENSION cl_khr_int64_base_atomics : enable
#pragma OPENCL EXTENSION cl_khr_int64_extended_atomics : enable

// NVIDIAs OpenCL does not create device-wide memory fences (see #734), so we
// use inline assembly if we detect we are on an NVIDIA GPU.
#ifdef cl_nv_pragma_unroll
static inline void mem_fence_global() {
  asm("membar.gl;");
}
#else
static inline void mem_fence_global() {
  mem_fence(CLK_LOCAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE);
}
#endif
static inline void mem_fence_local() {
  mem_fence(CLK_LOCAL_MEM_FENCE);
}

static inline void barrier_local() {
  barrier(CLK_LOCAL_MEM_FENCE);
}

// Important for this to be int64_t so it has proper alignment for any type.
#define SHARED_MEM_PARAM __local uint64_t* shared_mem,
#define FUTHARK_KERNEL __kernel
#define FUTHARK_KERNEL_SIZED(a,b,c) __attribute__((reqd_work_group_size(a, b, c))) __kernel

// End of prelude.cl
