#ifdef cl_clang_storage_class_specifiers
#pragma OPENCL EXTENSION cl_clang_storage_class_specifiers : enable
#endif
#pragma OPENCL EXTENSION cl_khr_byte_addressable_store : enable
__kernel void dummy_kernel(__global unsigned char *dummy, int n)
{
    const int thread_gid = get_global_id(0);
    
    if (thread_gid >= n)
        return;
}
typedef char int8_t;
typedef short int16_t;
typedef int int32_t;
typedef long int64_t;
typedef uchar uint8_t;
typedef ushort uint16_t;
typedef uint uint32_t;
typedef ulong uint64_t;
#ifdef cl_nv_pragma_unroll
static inline void mem_fence_global()
{
    asm("membar.gl;");
}
#else
static inline void mem_fence_global()
{
    mem_fence(CLK_LOCAL_MEM_FENCE | CLK_GLOBAL_MEM_FENCE);
}
#endif
static inline void mem_fence_local()
{
    mem_fence(CLK_LOCAL_MEM_FENCE);
}
static inline uint8_t add8(uint8_t x, uint8_t y)
{
    return x + y;
}
static inline uint16_t add16(uint16_t x, uint16_t y)
{
    return x + y;
}
static inline uint32_t add32(uint32_t x, uint32_t y)
{
    return x + y;
}
static inline uint64_t add64(uint64_t x, uint64_t y)
{
    return x + y;
}
static inline uint8_t sub8(uint8_t x, uint8_t y)
{
    return x - y;
}
static inline uint16_t sub16(uint16_t x, uint16_t y)
{
    return x - y;
}
static inline uint32_t sub32(uint32_t x, uint32_t y)
{
    return x - y;
}
static inline uint64_t sub64(uint64_t x, uint64_t y)
{
    return x - y;
}
static inline uint8_t mul8(uint8_t x, uint8_t y)
{
    return x * y;
}
static inline uint16_t mul16(uint16_t x, uint16_t y)
{
    return x * y;
}
static inline uint32_t mul32(uint32_t x, uint32_t y)
{
    return x * y;
}
static inline uint64_t mul64(uint64_t x, uint64_t y)
{
    return x * y;
}
static inline uint8_t udiv8(uint8_t x, uint8_t y)
{
    return x / y;
}
static inline uint16_t udiv16(uint16_t x, uint16_t y)
{
    return x / y;
}
static inline uint32_t udiv32(uint32_t x, uint32_t y)
{
    return x / y;
}
static inline uint64_t udiv64(uint64_t x, uint64_t y)
{
    return x / y;
}
static inline uint8_t umod8(uint8_t x, uint8_t y)
{
    return x % y;
}
static inline uint16_t umod16(uint16_t x, uint16_t y)
{
    return x % y;
}
static inline uint32_t umod32(uint32_t x, uint32_t y)
{
    return x % y;
}
static inline uint64_t umod64(uint64_t x, uint64_t y)
{
    return x % y;
}
static inline int8_t sdiv8(int8_t x, int8_t y)
{
    int8_t q = x / y;
    int8_t r = x % y;
    
    return q - ((r != 0 && r < 0 != y < 0) ? 1 : 0);
}
static inline int16_t sdiv16(int16_t x, int16_t y)
{
    int16_t q = x / y;
    int16_t r = x % y;
    
    return q - ((r != 0 && r < 0 != y < 0) ? 1 : 0);
}
static inline int32_t sdiv32(int32_t x, int32_t y)
{
    int32_t q = x / y;
    int32_t r = x % y;
    
    return q - ((r != 0 && r < 0 != y < 0) ? 1 : 0);
}
static inline int64_t sdiv64(int64_t x, int64_t y)
{
    int64_t q = x / y;
    int64_t r = x % y;
    
    return q - ((r != 0 && r < 0 != y < 0) ? 1 : 0);
}
static inline int8_t smod8(int8_t x, int8_t y)
{
    int8_t r = x % y;
    
    return r + (r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0) ? 0 : y);
}
static inline int16_t smod16(int16_t x, int16_t y)
{
    int16_t r = x % y;
    
    return r + (r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0) ? 0 : y);
}
static inline int32_t smod32(int32_t x, int32_t y)
{
    int32_t r = x % y;
    
    return r + (r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0) ? 0 : y);
}
static inline int64_t smod64(int64_t x, int64_t y)
{
    int64_t r = x % y;
    
    return r + (r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0) ? 0 : y);
}
static inline int8_t squot8(int8_t x, int8_t y)
{
    return x / y;
}
static inline int16_t squot16(int16_t x, int16_t y)
{
    return x / y;
}
static inline int32_t squot32(int32_t x, int32_t y)
{
    return x / y;
}
static inline int64_t squot64(int64_t x, int64_t y)
{
    return x / y;
}
static inline int8_t srem8(int8_t x, int8_t y)
{
    return x % y;
}
static inline int16_t srem16(int16_t x, int16_t y)
{
    return x % y;
}
static inline int32_t srem32(int32_t x, int32_t y)
{
    return x % y;
}
static inline int64_t srem64(int64_t x, int64_t y)
{
    return x % y;
}
static inline int8_t smin8(int8_t x, int8_t y)
{
    return x < y ? x : y;
}
static inline int16_t smin16(int16_t x, int16_t y)
{
    return x < y ? x : y;
}
static inline int32_t smin32(int32_t x, int32_t y)
{
    return x < y ? x : y;
}
static inline int64_t smin64(int64_t x, int64_t y)
{
    return x < y ? x : y;
}
static inline uint8_t umin8(uint8_t x, uint8_t y)
{
    return x < y ? x : y;
}
static inline uint16_t umin16(uint16_t x, uint16_t y)
{
    return x < y ? x : y;
}
static inline uint32_t umin32(uint32_t x, uint32_t y)
{
    return x < y ? x : y;
}
static inline uint64_t umin64(uint64_t x, uint64_t y)
{
    return x < y ? x : y;
}
static inline int8_t smax8(int8_t x, int8_t y)
{
    return x < y ? y : x;
}
static inline int16_t smax16(int16_t x, int16_t y)
{
    return x < y ? y : x;
}
static inline int32_t smax32(int32_t x, int32_t y)
{
    return x < y ? y : x;
}
static inline int64_t smax64(int64_t x, int64_t y)
{
    return x < y ? y : x;
}
static inline uint8_t umax8(uint8_t x, uint8_t y)
{
    return x < y ? y : x;
}
static inline uint16_t umax16(uint16_t x, uint16_t y)
{
    return x < y ? y : x;
}
static inline uint32_t umax32(uint32_t x, uint32_t y)
{
    return x < y ? y : x;
}
static inline uint64_t umax64(uint64_t x, uint64_t y)
{
    return x < y ? y : x;
}
static inline uint8_t shl8(uint8_t x, uint8_t y)
{
    return x << y;
}
static inline uint16_t shl16(uint16_t x, uint16_t y)
{
    return x << y;
}
static inline uint32_t shl32(uint32_t x, uint32_t y)
{
    return x << y;
}
static inline uint64_t shl64(uint64_t x, uint64_t y)
{
    return x << y;
}
static inline uint8_t lshr8(uint8_t x, uint8_t y)
{
    return x >> y;
}
static inline uint16_t lshr16(uint16_t x, uint16_t y)
{
    return x >> y;
}
static inline uint32_t lshr32(uint32_t x, uint32_t y)
{
    return x >> y;
}
static inline uint64_t lshr64(uint64_t x, uint64_t y)
{
    return x >> y;
}
static inline int8_t ashr8(int8_t x, int8_t y)
{
    return x >> y;
}
static inline int16_t ashr16(int16_t x, int16_t y)
{
    return x >> y;
}
static inline int32_t ashr32(int32_t x, int32_t y)
{
    return x >> y;
}
static inline int64_t ashr64(int64_t x, int64_t y)
{
    return x >> y;
}
static inline uint8_t and8(uint8_t x, uint8_t y)
{
    return x & y;
}
static inline uint16_t and16(uint16_t x, uint16_t y)
{
    return x & y;
}
static inline uint32_t and32(uint32_t x, uint32_t y)
{
    return x & y;
}
static inline uint64_t and64(uint64_t x, uint64_t y)
{
    return x & y;
}
static inline uint8_t or8(uint8_t x, uint8_t y)
{
    return x | y;
}
static inline uint16_t or16(uint16_t x, uint16_t y)
{
    return x | y;
}
static inline uint32_t or32(uint32_t x, uint32_t y)
{
    return x | y;
}
static inline uint64_t or64(uint64_t x, uint64_t y)
{
    return x | y;
}
static inline uint8_t xor8(uint8_t x, uint8_t y)
{
    return x ^ y;
}
static inline uint16_t xor16(uint16_t x, uint16_t y)
{
    return x ^ y;
}
static inline uint32_t xor32(uint32_t x, uint32_t y)
{
    return x ^ y;
}
static inline uint64_t xor64(uint64_t x, uint64_t y)
{
    return x ^ y;
}
static inline bool ult8(uint8_t x, uint8_t y)
{
    return x < y;
}
static inline bool ult16(uint16_t x, uint16_t y)
{
    return x < y;
}
static inline bool ult32(uint32_t x, uint32_t y)
{
    return x < y;
}
static inline bool ult64(uint64_t x, uint64_t y)
{
    return x < y;
}
static inline bool ule8(uint8_t x, uint8_t y)
{
    return x <= y;
}
static inline bool ule16(uint16_t x, uint16_t y)
{
    return x <= y;
}
static inline bool ule32(uint32_t x, uint32_t y)
{
    return x <= y;
}
static inline bool ule64(uint64_t x, uint64_t y)
{
    return x <= y;
}
static inline bool slt8(int8_t x, int8_t y)
{
    return x < y;
}
static inline bool slt16(int16_t x, int16_t y)
{
    return x < y;
}
static inline bool slt32(int32_t x, int32_t y)
{
    return x < y;
}
static inline bool slt64(int64_t x, int64_t y)
{
    return x < y;
}
static inline bool sle8(int8_t x, int8_t y)
{
    return x <= y;
}
static inline bool sle16(int16_t x, int16_t y)
{
    return x <= y;
}
static inline bool sle32(int32_t x, int32_t y)
{
    return x <= y;
}
static inline bool sle64(int64_t x, int64_t y)
{
    return x <= y;
}
static inline int8_t pow8(int8_t x, int8_t y)
{
    int8_t res = 1, rem = y;
    
    while (rem != 0) {
        if (rem & 1)
            res *= x;
        rem >>= 1;
        x *= x;
    }
    return res;
}
static inline int16_t pow16(int16_t x, int16_t y)
{
    int16_t res = 1, rem = y;
    
    while (rem != 0) {
        if (rem & 1)
            res *= x;
        rem >>= 1;
        x *= x;
    }
    return res;
}
static inline int32_t pow32(int32_t x, int32_t y)
{
    int32_t res = 1, rem = y;
    
    while (rem != 0) {
        if (rem & 1)
            res *= x;
        rem >>= 1;
        x *= x;
    }
    return res;
}
static inline int64_t pow64(int64_t x, int64_t y)
{
    int64_t res = 1, rem = y;
    
    while (rem != 0) {
        if (rem & 1)
            res *= x;
        rem >>= 1;
        x *= x;
    }
    return res;
}
static inline bool itob_i8_bool(int8_t x)
{
    return x;
}
static inline bool itob_i16_bool(int16_t x)
{
    return x;
}
static inline bool itob_i32_bool(int32_t x)
{
    return x;
}
static inline bool itob_i64_bool(int64_t x)
{
    return x;
}
static inline int8_t btoi_bool_i8(bool x)
{
    return x;
}
static inline int16_t btoi_bool_i16(bool x)
{
    return x;
}
static inline int32_t btoi_bool_i32(bool x)
{
    return x;
}
static inline int64_t btoi_bool_i64(bool x)
{
    return x;
}
#define sext_i8_i8(x) ((int8_t) (int8_t) x)
#define sext_i8_i16(x) ((int16_t) (int8_t) x)
#define sext_i8_i32(x) ((int32_t) (int8_t) x)
#define sext_i8_i64(x) ((int64_t) (int8_t) x)
#define sext_i16_i8(x) ((int8_t) (int16_t) x)
#define sext_i16_i16(x) ((int16_t) (int16_t) x)
#define sext_i16_i32(x) ((int32_t) (int16_t) x)
#define sext_i16_i64(x) ((int64_t) (int16_t) x)
#define sext_i32_i8(x) ((int8_t) (int32_t) x)
#define sext_i32_i16(x) ((int16_t) (int32_t) x)
#define sext_i32_i32(x) ((int32_t) (int32_t) x)
#define sext_i32_i64(x) ((int64_t) (int32_t) x)
#define sext_i64_i8(x) ((int8_t) (int64_t) x)
#define sext_i64_i16(x) ((int16_t) (int64_t) x)
#define sext_i64_i32(x) ((int32_t) (int64_t) x)
#define sext_i64_i64(x) ((int64_t) (int64_t) x)
#define zext_i8_i8(x) ((uint8_t) (uint8_t) x)
#define zext_i8_i16(x) ((uint16_t) (uint8_t) x)
#define zext_i8_i32(x) ((uint32_t) (uint8_t) x)
#define zext_i8_i64(x) ((uint64_t) (uint8_t) x)
#define zext_i16_i8(x) ((uint8_t) (uint16_t) x)
#define zext_i16_i16(x) ((uint16_t) (uint16_t) x)
#define zext_i16_i32(x) ((uint32_t) (uint16_t) x)
#define zext_i16_i64(x) ((uint64_t) (uint16_t) x)
#define zext_i32_i8(x) ((uint8_t) (uint32_t) x)
#define zext_i32_i16(x) ((uint16_t) (uint32_t) x)
#define zext_i32_i32(x) ((uint32_t) (uint32_t) x)
#define zext_i32_i64(x) ((uint64_t) (uint32_t) x)
#define zext_i64_i8(x) ((uint8_t) (uint64_t) x)
#define zext_i64_i16(x) ((uint16_t) (uint64_t) x)
#define zext_i64_i32(x) ((uint32_t) (uint64_t) x)
#define zext_i64_i64(x) ((uint64_t) (uint64_t) x)
#if defined(__OPENCL_VERSION__)
static int32_t futrts_popc8(int8_t x)
{
    return popcount(x);
}
static int32_t futrts_popc16(int16_t x)
{
    return popcount(x);
}
static int32_t futrts_popc32(int32_t x)
{
    return popcount(x);
}
static int32_t futrts_popc64(int64_t x)
{
    return popcount(x);
}
#elif defined(__CUDA_ARCH__)
static int32_t futrts_popc8(int8_t x)
{
    return __popc(zext_i8_i32(x));
}
static int32_t futrts_popc16(int16_t x)
{
    return __popc(zext_i16_i32(x));
}
static int32_t futrts_popc32(int32_t x)
{
    return __popc(x);
}
static int32_t futrts_popc64(int64_t x)
{
    return __popcll(x);
}
#else
static int32_t futrts_popc8(int8_t x)
{
    int c = 0;
    
    for (; x; ++c)
        x &= x - 1;
    return c;
}
static int32_t futrts_popc16(int16_t x)
{
    int c = 0;
    
    for (; x; ++c)
        x &= x - 1;
    return c;
}
static int32_t futrts_popc32(int32_t x)
{
    int c = 0;
    
    for (; x; ++c)
        x &= x - 1;
    return c;
}
static int32_t futrts_popc64(int64_t x)
{
    int c = 0;
    
    for (; x; ++c)
        x &= x - 1;
    return c;
}
#endif
#if defined(__OPENCL_VERSION__)
static uint8_t futrts_mul_hi8(uint8_t a, uint8_t b)
{
    return mul_hi(a, b);
}
static uint16_t futrts_mul_hi16(uint16_t a, uint16_t b)
{
    return mul_hi(a, b);
}
static uint32_t futrts_mul_hi32(uint32_t a, uint32_t b)
{
    return mul_hi(a, b);
}
static uint64_t futrts_mul_hi64(uint64_t a, uint64_t b)
{
    return mul_hi(a, b);
}
#elif defined(__CUDA_ARCH__)
static uint8_t futrts_mul_hi8(uint8_t a, uint8_t b)
{
    uint16_t aa = a;
    uint16_t bb = b;
    
    return aa * bb >> 8;
}
static uint16_t futrts_mul_hi16(uint16_t a, uint16_t b)
{
    uint32_t aa = a;
    uint32_t bb = b;
    
    return aa * bb >> 16;
}
static uint32_t futrts_mul_hi32(uint32_t a, uint32_t b)
{
    return mulhi(a, b);
}
static uint64_t futrts_mul_hi64(uint64_t a, uint64_t b)
{
    return mul64hi(a, b);
}
#else
static uint8_t futrts_mul_hi8(uint8_t a, uint8_t b)
{
    uint16_t aa = a;
    uint16_t bb = b;
    
    return aa * bb >> 8;
}
static uint16_t futrts_mul_hi16(uint16_t a, uint16_t b)
{
    uint32_t aa = a;
    uint32_t bb = b;
    
    return aa * bb >> 16;
}
static uint32_t futrts_mul_hi32(uint32_t a, uint32_t b)
{
    uint64_t aa = a;
    uint64_t bb = b;
    
    return aa * bb >> 32;
}
static uint64_t futrts_mul_hi64(uint64_t a, uint64_t b)
{
    __uint128_t aa = a;
    __uint128_t bb = b;
    
    return aa * bb >> 64;
}
#endif
#if defined(__OPENCL_VERSION__)
static uint8_t futrts_mad_hi8(uint8_t a, uint8_t b, uint8_t c)
{
    return mad_hi(a, b, c);
}
static uint16_t futrts_mad_hi16(uint16_t a, uint16_t b, uint16_t c)
{
    return mad_hi(a, b, c);
}
static uint32_t futrts_mad_hi32(uint32_t a, uint32_t b, uint32_t c)
{
    return mad_hi(a, b, c);
}
static uint64_t futrts_mad_hi64(uint64_t a, uint64_t b, uint64_t c)
{
    return mad_hi(a, b, c);
}
#else
static uint8_t futrts_mad_hi8(uint8_t a, uint8_t b, uint8_t c)
{
    return futrts_mul_hi8(a, b) + c;
}
static uint16_t futrts_mad_hi16(uint16_t a, uint16_t b, uint16_t c)
{
    return futrts_mul_hi16(a, b) + c;
}
static uint32_t futrts_mad_hi32(uint32_t a, uint32_t b, uint32_t c)
{
    return futrts_mul_hi32(a, b) + c;
}
static uint64_t futrts_mad_hi64(uint64_t a, uint64_t b, uint64_t c)
{
    return futrts_mul_hi64(a, b) + c;
}
#endif
#if defined(__OPENCL_VERSION__)
static int32_t futrts_clzz8(int8_t x)
{
    return clz(x);
}
static int32_t futrts_clzz16(int16_t x)
{
    return clz(x);
}
static int32_t futrts_clzz32(int32_t x)
{
    return clz(x);
}
static int32_t futrts_clzz64(int64_t x)
{
    return clz(x);
}
#elif defined(__CUDA_ARCH__)
static int32_t futrts_clzz8(int8_t x)
{
    return __clz(zext_i8_i32(x)) - 24;
}
static int32_t futrts_clzz16(int16_t x)
{
    return __clz(zext_i16_i32(x)) - 16;
}
static int32_t futrts_clzz32(int32_t x)
{
    return __clz(x);
}
static int32_t futrts_clzz64(int64_t x)
{
    return __clzll(x);
}
#else
static int32_t futrts_clzz8(int8_t x)
{
    int n = 0;
    int bits = sizeof(x) * 8;
    
    for (int i = 0; i < bits; i++) {
        if (x < 0)
            break;
        n++;
        x <<= 1;
    }
    return n;
}
static int32_t futrts_clzz16(int16_t x)
{
    int n = 0;
    int bits = sizeof(x) * 8;
    
    for (int i = 0; i < bits; i++) {
        if (x < 0)
            break;
        n++;
        x <<= 1;
    }
    return n;
}
static int32_t futrts_clzz32(int32_t x)
{
    int n = 0;
    int bits = sizeof(x) * 8;
    
    for (int i = 0; i < bits; i++) {
        if (x < 0)
            break;
        n++;
        x <<= 1;
    }
    return n;
}
static int32_t futrts_clzz64(int64_t x)
{
    int n = 0;
    int bits = sizeof(x) * 8;
    
    for (int i = 0; i < bits; i++) {
        if (x < 0)
            break;
        n++;
        x <<= 1;
    }
    return n;
}
#endif
static inline float fdiv32(float x, float y)
{
    return x / y;
}
static inline float fadd32(float x, float y)
{
    return x + y;
}
static inline float fsub32(float x, float y)
{
    return x - y;
}
static inline float fmul32(float x, float y)
{
    return x * y;
}
static inline float fmin32(float x, float y)
{
    return fmin(x, y);
}
static inline float fmax32(float x, float y)
{
    return fmax(x, y);
}
static inline float fpow32(float x, float y)
{
    return pow(x, y);
}
static inline bool cmplt32(float x, float y)
{
    return x < y;
}
static inline bool cmple32(float x, float y)
{
    return x <= y;
}
static inline float sitofp_i8_f32(int8_t x)
{
    return (float) x;
}
static inline float sitofp_i16_f32(int16_t x)
{
    return (float) x;
}
static inline float sitofp_i32_f32(int32_t x)
{
    return (float) x;
}
static inline float sitofp_i64_f32(int64_t x)
{
    return (float) x;
}
static inline float uitofp_i8_f32(uint8_t x)
{
    return (float) x;
}
static inline float uitofp_i16_f32(uint16_t x)
{
    return (float) x;
}
static inline float uitofp_i32_f32(uint32_t x)
{
    return (float) x;
}
static inline float uitofp_i64_f32(uint64_t x)
{
    return (float) x;
}
static inline int8_t fptosi_f32_i8(float x)
{
    return (int8_t) x;
}
static inline int16_t fptosi_f32_i16(float x)
{
    return (int16_t) x;
}
static inline int32_t fptosi_f32_i32(float x)
{
    return (int32_t) x;
}
static inline int64_t fptosi_f32_i64(float x)
{
    return (int64_t) x;
}
static inline uint8_t fptoui_f32_i8(float x)
{
    return (uint8_t) x;
}
static inline uint16_t fptoui_f32_i16(float x)
{
    return (uint16_t) x;
}
static inline uint32_t fptoui_f32_i32(float x)
{
    return (uint32_t) x;
}
static inline uint64_t fptoui_f32_i64(float x)
{
    return (uint64_t) x;
}
static inline float futrts_log32(float x)
{
    return log(x);
}
static inline float futrts_log2_32(float x)
{
    return log2(x);
}
static inline float futrts_log10_32(float x)
{
    return log10(x);
}
static inline float futrts_sqrt32(float x)
{
    return sqrt(x);
}
static inline float futrts_exp32(float x)
{
    return exp(x);
}
static inline float futrts_cos32(float x)
{
    return cos(x);
}
static inline float futrts_sin32(float x)
{
    return sin(x);
}
static inline float futrts_tan32(float x)
{
    return tan(x);
}
static inline float futrts_acos32(float x)
{
    return acos(x);
}
static inline float futrts_asin32(float x)
{
    return asin(x);
}
static inline float futrts_atan32(float x)
{
    return atan(x);
}
static inline float futrts_atan2_32(float x, float y)
{
    return atan2(x, y);
}
static inline float futrts_gamma32(float x)
{
    return tgamma(x);
}
static inline float futrts_lgamma32(float x)
{
    return lgamma(x);
}
static inline bool futrts_isnan32(float x)
{
    return isnan(x);
}
static inline bool futrts_isinf32(float x)
{
    return isinf(x);
}
static inline int32_t futrts_to_bits32(float x)
{
    union {
        float f;
        int32_t t;
    } p;
    
    p.f = x;
    return p.t;
}
static inline float futrts_from_bits32(int32_t x)
{
    union {
        int32_t f;
        float t;
    } p;
    
    p.f = x;
    return p.t;
}
#ifdef __OPENCL_VERSION__
static inline float fmod32(float x, float y)
{
    return fmod(x, y);
}
static inline float futrts_round32(float x)
{
    return rint(x);
}
static inline float futrts_floor32(float x)
{
    return floor(x);
}
static inline float futrts_ceil32(float x)
{
    return ceil(x);
}
static inline float futrts_lerp32(float v0, float v1, float t)
{
    return mix(v0, v1, t);
}
#else
static inline float fmod32(float x, float y)
{
    return fmodf(x, y);
}
static inline float futrts_round32(float x)
{
    return rintf(x);
}
static inline float futrts_floor32(float x)
{
    return floorf(x);
}
static inline float futrts_ceil32(float x)
{
    return ceilf(x);
}
static inline float futrts_lerp32(float v0, float v1, float t)
{
    return v0 + (v1 - v0) * t;
}
#endif
__kernel void my_scan_4163(__global int *global_failure, __local volatile
                           int64_t *warpscan_mem_4215_backing_aligned_0,
                           __local volatile
                           int64_t *block_id_mem_4199_backing_aligned_1,
                           __local volatile
                           int64_t *exchange_mem_4197_backing_aligned_2,
                           int32_t implz2080U_4146, __global
                           unsigned char *input_mem_4165, __global
                           unsigned char *mem_4169, __global
                           unsigned char *mem_4172, __global
                           unsigned char *aggregates_mem_4181, __global
                           unsigned char *incprefix_mem_4183, __global
                           unsigned char *statusflgs_mem_4185, __global
                           unsigned char *dyn_id_mem_4187,
                           int32_t available_local_mem_4189,
                           int32_t eLEMS_PER_THREAD_4190)
{
    #define segscan_group_sizze_4158 (mainzisegscan_group_sizze_4157)
    
    const int block_dim0 = 0;
    const int block_dim1 = 1;
    const int block_dim2 = 2;
    __local volatile char *restrict warpscan_mem_4215_backing_3 =
                          (__local volatile
                           char *) warpscan_mem_4215_backing_aligned_0;
    __local volatile char *restrict block_id_mem_4199_backing_1 =
                          (__local volatile
                           char *) block_id_mem_4199_backing_aligned_1;
    __local volatile char *restrict exchange_mem_4197_backing_0 =
                          (__local volatile
                           char *) exchange_mem_4197_backing_aligned_2;
    
    if (*global_failure >= 0)
        return;
    
    int32_t global_tid_4191;
    int32_t local_tid_4192;
    int32_t group_sizze_4195;
    int32_t wave_sizze_4194;
    int32_t group_tid_4193;
    
    global_tid_4191 = get_global_id(0);
    local_tid_4192 = get_local_id(0);
    group_sizze_4195 = get_local_size(0);
    wave_sizze_4194 = LOCKSTEP_WIDTH;
    group_tid_4193 = get_group_id(0);
    
    int32_t phys_tid_4163 = global_tid_4191;
    int32_t div2_4196 = sdiv32(available_local_mem_4189, 2);
    __local char *exchange_mem_4197;
    
    exchange_mem_4197 = (__local char *) exchange_mem_4197_backing_0;
    
    __local char *block_id_mem_4199;
    
    block_id_mem_4199 = (__local char *) block_id_mem_4199_backing_1;
    if (local_tid_4192 == 0) {
        int32_t reg_dyn_id_4201 = 0;
        
        reg_dyn_id_4201 = atomic_add(&((volatile __global
                                        int *) dyn_id_mem_4187)[0], (int) 1);
        ((__global int8_t *) statusflgs_mem_4185)[reg_dyn_id_4201] = 0;
        ((__local int32_t *) block_id_mem_4199)[0] = reg_dyn_id_4201;
    }
    barrier(CLK_LOCAL_MEM_FENCE);
    
    int32_t wG_ID_4202 = ((__local int32_t *) block_id_mem_4199)[0];
    __private char *chunk_mem_4203;
    __private char chunk_mem_4203_backing_2[mul32((int32_t) sizeof(int64_t),
                                                  eLEMS_PER_THREAD_4190)];
    
    chunk_mem_4203 = chunk_mem_4203_backing_2;
    // Coalesced read, apply map, write result and write to reg
    {
        int32_t block_offset_4205 = mul32(mul32(wG_ID_4202,
                                                segscan_group_sizze_4158),
                                          eLEMS_PER_THREAD_4190);
        
        for (int32_t i_4206 = 0; i_4206 < eLEMS_PER_THREAD_4190; i_4206++) {
            int32_t gtid_4162 = add32(add32(block_offset_4205, mul32(i_4206,
                                                                     segscan_group_sizze_4158)),
                                      local_tid_4192);
            
            if (slt32(add32(add32(block_offset_4205, mul32(i_4206,
                                                           segscan_group_sizze_4158)),
                            local_tid_4192), implz2080U_4146)) {
                int64_t x_4153 = ((__global
                                   int64_t *) input_mem_4165)[gtid_4162];
                int64_t res_4154 = mul64(2, x_4153);
                
                ((__local
                  int64_t *) exchange_mem_4197)[sub32(add32(add32(block_offset_4205,
                                                                  mul32(i_4206,
                                                                        segscan_group_sizze_4158)),
                                                            local_tid_4192),
                                                      block_offset_4205)] =
                    res_4154;
                ((__global int64_t *) mem_4172)[gtid_4162] = res_4154;
            } else {
                ((__local int64_t *) exchange_mem_4197)[local_tid_4192] = 0;
            }
        }
        barrier(CLK_LOCAL_MEM_FENCE);
        
        int32_t loc_offset_4207 = mul32(local_tid_4192, eLEMS_PER_THREAD_4190);
        
        for (int32_t i_4208 = 0; i_4208 < eLEMS_PER_THREAD_4190; i_4208++) {
            ((__private int64_t *) chunk_mem_4203)[i_4208] = ((__local
                                                               int64_t *) exchange_mem_4197)[add32(loc_offset_4207,
                                                                                                   i_4208)];
        }
        barrier(CLK_LOCAL_MEM_FENCE);
    }
    
    int64_t acc_4209 = 0;
    
    acc_4209 = ((__private int64_t *) chunk_mem_4203)[0];
    // Per-Thread Scan
    {
        for (int32_t i_4210 = 0; i_4210 < eLEMS_PER_THREAD_4190; i_4210++) {
            int64_t x_4150;
            int64_t x_4151;
            
            x_4150 = acc_4209;
            x_4151 = ((__private int64_t *) chunk_mem_4203)[i_4210];
            
            int64_t res_4152 = add64(x_4150, x_4151);
            
            acc_4209 = res_4152;
        }
        ((__local int64_t *) exchange_mem_4197)[local_tid_4192] = acc_4209;
        barrier(CLK_LOCAL_MEM_FENCE);
    }
    // Per-Group Scan
    {
        int64_t res_4211 = 0;
        int32_t p1_4212 = 1;
        
        barrier(CLK_LOCAL_MEM_FENCE);
        while (slt32(p1_4212, implz2080U_4146)) {
            if (sle32(p1_4212, local_tid_4192)) {
                int64_t x_4150;
                int64_t x_4151;
                
                x_4150 = ((__local
                           int64_t *) exchange_mem_4197)[local_tid_4192];
                x_4151 = ((__local
                           int64_t *) exchange_mem_4197)[sub32(local_tid_4192,
                                                               p1_4212)];
                
                int64_t res_4152 = add64(x_4150, x_4151);
                
                res_4211 = res_4152;
            }
            barrier(CLK_LOCAL_MEM_FENCE);
            if (sle32(p1_4212, local_tid_4192)) {
                ((__local int64_t *) exchange_mem_4197)[local_tid_4192] =
                    res_4211;
            }
            barrier(CLK_LOCAL_MEM_FENCE);
            p1_4212 *= 2;
        }
        
        int32_t prev_ind_4213;
        
        if (local_tid_4192 == 0) {
            prev_ind_4213 = sub32(segscan_group_sizze_4158, 1);
        } else {
            prev_ind_4213 = sub32(local_tid_4192, 1);
        }
        acc_4209 = ((__local int64_t *) exchange_mem_4197)[prev_ind_4213];
    }
    
    int64_t prefix_4214 = 0;
    
    // Compute prefix from previous blocks
    {
        if (wG_ID_4202 == 0 && local_tid_4192 == 0) {
            ((__global int64_t *) incprefix_mem_4183)[0] = acc_4209;
            mem_fence_global();
            ((__global int8_t *) statusflgs_mem_4185)[0] = 2;
            acc_4209 = 0;
        }
        if (!(wG_ID_4202 == 0 || sle32(wave_sizze_4194, local_tid_4192))) {
            __local char *warpscan_mem_4215;
            
            warpscan_mem_4215 = (__local char *) warpscan_mem_4215_backing_3;
            if (local_tid_4192 == 0) {
                ((__global int64_t *) aggregates_mem_4181)[wG_ID_4202] =
                    acc_4209;
                mem_fence_global();
                ((__global int8_t *) statusflgs_mem_4185)[wG_ID_4202] = 1;
                ((__local int8_t *) warpscan_mem_4215)[0] = ((__global
                                                              int8_t *) statusflgs_mem_4185)[sub32(wG_ID_4202,
                                                                                                   1)];
            }
            mem_fence_global();
            
            int8_t status1_4217 = ((__local int8_t *) warpscan_mem_4215)[0];
            
            if (status1_4217 == 2) {
                if (local_tid_4192 == 0) {
                    prefix_4214 = ((__global
                                    int64_t *) incprefix_mem_4183)[sub32(wG_ID_4202,
                                                                         1)];
                }
            } else {
                int32_t read_offset_4218 = sub32(wG_ID_4202, wave_sizze_4194);
                int32_t loop_stop_4219 = sub32(0, wave_sizze_4194);
                
                while (slt32(loop_stop_4219, read_offset_4218)) {
                    int32_t readi_4220 = add32(read_offset_4218,
                                               local_tid_4192);
                    int64_t aggr_4221 = 0;
                    int8_t flag_4222 = 0;
                    int8_t used_4223 = 0;
                    
                    if (sle32(0, readi_4220)) {
                        flag_4222 = ((__global
                                      int8_t *) statusflgs_mem_4185)[readi_4220];
                        if (flag_4222 == 2) {
                            aggr_4221 = ((__global
                                          int64_t *) incprefix_mem_4183)[readi_4220];
                        }
                        if (flag_4222 == 1) {
                            aggr_4221 = ((__global
                                          int64_t *) aggregates_mem_4181)[readi_4220];
                            used_4223 = 1;
                        }
                    }
                    ((__local int64_t *) exchange_mem_4197)[local_tid_4192] =
                        aggr_4221;
                    
                    int8_t combined_flg_4224 = used_4223 << 2 | flag_4222;
                    
                    ((__local int8_t *) warpscan_mem_4215)[local_tid_4192] =
                        combined_flg_4224;
                    mem_fence_global();
                    
                    int8_t wsmone_4225 = ((__local
                                           int8_t *) warpscan_mem_4215)[sub32(wave_sizze_4194,
                                                                              1)];
                    
                    if (!(wsmone_4225 == 2)) {
                        int32_t p_4226 = 1;
                        
                        while (slt32(p_4226, wave_sizze_4194)) {
                            if (sle32(p_4226, local_tid_4192)) {
                                int64_t agg1_4227 = 0;
                                
                                agg1_4227 = ((__local
                                              int64_t *) exchange_mem_4197)[sub32(local_tid_4192,
                                                                                  p_4226)];
                                
                                int64_t agg2_4228 = 0;
                                
                                agg2_4228 = ((__local
                                              int64_t *) exchange_mem_4197)[local_tid_4192];
                                
                                int8_t usd1_4229;
                                int8_t usd2_4230;
                                int8_t stat1_4231;
                                int8_t stat2_4232;
                                int8_t tmp_4233 = ((__local
                                                    int8_t *) warpscan_mem_4215)[sub32(local_tid_4192,
                                                                                       p_4226)];
                                
                                stat1_4231 = tmp_4233 & 3;
                                usd1_4229 = lshr8(tmp_4233, 2);
                                tmp_4233 = ((__local
                                             int8_t *) warpscan_mem_4215)[local_tid_4192];
                                stat2_4232 = tmp_4233 & 3;
                                usd2_4230 = lshr8(tmp_4233, 2);
                                if (!(stat2_4232 == 1)) {
                                    agg1_4227 = 0;
                                    usd1_4229 = 0;
                                    stat1_4231 = stat2_4232;
                                }
                                usd1_4229 += usd2_4230;
                                usd1_4229 = usd1_4229 << 2;
                                usd1_4229 = usd1_4229 | stat1_4231;
                                ((__local
                                  int8_t *) warpscan_mem_4215)[local_tid_4192] =
                                    usd1_4229;
                                
                                int64_t x_4150;
                                int64_t x_4151;
                                
                                x_4150 = agg1_4227;
                                x_4151 = agg2_4228;
                                
                                int64_t res_4152 = add64(x_4150, x_4151);
                                
                                ((__local
                                  int64_t *) exchange_mem_4197)[local_tid_4192] =
                                    res_4152;
                            }
                            p_4226 *= 2;
                        }
                    }
                    mem_fence_global();
                    if (local_tid_4192 == 0) {
                        int8_t usedflg_val_4234 = ((__local
                                                    int8_t *) warpscan_mem_4215)[sub32(wave_sizze_4194,
                                                                                       1)];
                        
                        flag_4222 = usedflg_val_4234 & 3;
                        if (flag_4222 == 2) {
                            read_offset_4218 = loop_stop_4219;
                        } else {
                            used_4223 = lshr8(usedflg_val_4234, 2);
                            read_offset_4218 -= used_4223;
                        }
                        ((__local int32_t *) block_id_mem_4199)[0] =
                            read_offset_4218;
                        aggr_4221 = ((__local
                                      int64_t *) exchange_mem_4197)[sub32(wave_sizze_4194,
                                                                          1)];
                        
                        int64_t x_4150;
                        int64_t x_4151;
                        
                        x_4150 = aggr_4221;
                        x_4151 = prefix_4214;
                        
                        int64_t res_4152 = add64(x_4150, x_4151);
                        
                        prefix_4214 = res_4152;
                    }
                    mem_fence_global();
                    read_offset_4218 = ((__local
                                         int32_t *) block_id_mem_4199)[0];
                }
            }
            if (local_tid_4192 == 0) {
                int64_t x_4150;
                int64_t x_4151;
                
                x_4150 = prefix_4214;
                x_4151 = acc_4209;
                
                int64_t res_4152 = add64(x_4150, x_4151);
                
                ((__global int64_t *) incprefix_mem_4183)[wG_ID_4202] =
                    res_4152;
                mem_fence_global();
                ((__global int8_t *) statusflgs_mem_4185)[wG_ID_4202] = 2;
                ((__local int64_t *) exchange_mem_4197)[0] = prefix_4214;
                acc_4209 = 0;
            }
        }
        if (!(wG_ID_4202 == 0)) {
            barrier(CLK_LOCAL_MEM_FENCE);
            prefix_4214 = ((__local int64_t *) exchange_mem_4197)[0];
            barrier(CLK_LOCAL_MEM_FENCE);
        }
    }
    // Read and add prefix to every element in this workgroup
    {
        int64_t myacc_4235 = 0;
        int64_t x_4150;
        int64_t x_4151;
        
        x_4150 = prefix_4214;
        x_4151 = acc_4209;
        
        int64_t res_4152 = add64(x_4150, x_4151);
        
        myacc_4235 = res_4152;
        for (int32_t i_4236 = 0; i_4236 < eLEMS_PER_THREAD_4190; i_4236++) {
            int64_t x_4237;
            int64_t x_4238;
            
            x_4237 = myacc_4235;
            x_4238 = ((__private int64_t *) chunk_mem_4203)[i_4236];
            
            int64_t res_4239 = add64(x_4237, x_4238);
            
            ((__local int64_t *) exchange_mem_4197)[add32(mul32(local_tid_4192,
                                                                eLEMS_PER_THREAD_4190),
                                                          i_4236)] = res_4239;
        }
        
        int32_t block_offset_4240 = mul32(mul32(wG_ID_4202,
                                                segscan_group_sizze_4158),
                                          eLEMS_PER_THREAD_4190);
        
        for (int32_t i_4241 = 0; i_4241 < eLEMS_PER_THREAD_4190; i_4241++) {
            if (slt32(add32(add32(block_offset_4240, mul32(i_4241,
                                                           segscan_group_sizze_4158)),
                            local_tid_4192), implz2080U_4146)) {
                ((__global int64_t *) mem_4169)[add32(add32(block_offset_4240,
                                                            mul32(i_4241,
                                                                  segscan_group_sizze_4158)),
                                                      local_tid_4192)] =
                    ((__local
                      int64_t *) exchange_mem_4197)[sub32(add32(add32(block_offset_4240,
                                                                      mul32(i_4241,
                                                                            segscan_group_sizze_4158)),
                                                                local_tid_4192),
                                                          block_offset_4240)];
            }
        }
    }
    
  error_0:
    return;
    #undef segscan_group_sizze_4158
}
