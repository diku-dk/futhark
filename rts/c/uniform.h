// Start of uniform.h

// Uniform versions of all library functions as to
// improve performance in ISPC when in an uniform context.

#if defined(ISPC)

static inline uniform uint8_t add8(uniform uint8_t x, uniform uint8_t y) {
  return x + y;
}

static inline uniform uint16_t add16(uniform uint16_t x, uniform uint16_t y) {
  return x + y;
}

static inline uniform uint32_t add32(uniform uint32_t x, uniform uint32_t y) {
  return x + y;
}

static inline uniform uint64_t add64(uniform uint64_t x, uniform uint64_t y) {
  return x + y;
}

static inline uniform uint8_t sub8(uniform uint8_t x, uniform uint8_t y) {
  return x - y;
}

static inline uniform uint16_t sub16(uniform uint16_t x, uniform uint16_t y) {
  return x - y;
}

static inline uniform uint32_t sub32(uniform uint32_t x, uniform uint32_t y) {
  return x - y;
}

static inline uniform uint64_t sub64(uniform uint64_t x, uniform uint64_t y) {
  return x - y;
}

static inline uniform uint8_t mul8(uniform uint8_t x, uniform uint8_t y) {
  return x * y;
}

static inline uniform uint16_t mul16(uniform uint16_t x, uniform uint16_t y) {
  return x * y;
}

static inline uniform uint32_t mul32(uniform uint32_t x, uniform uint32_t y) {
  return x * y;
}

static inline uniform uint64_t mul64(uniform uint64_t x, uniform uint64_t y) {
  return x * y;
}

static inline uniform uint8_t udiv8(uniform uint8_t x, uniform uint8_t y) {
  return x / y;
}

static inline uniform uint16_t udiv16(uniform uint16_t x, uniform uint16_t y) {
  return x / y;
}

static inline uniform uint32_t udiv32(uniform uint32_t x, uniform uint32_t y) {
  return x / y;
}

static inline uniform uint64_t udiv64(uniform uint64_t x, uniform uint64_t y) {
  return x / y;
}

static inline uniform uint8_t udiv_up8(uniform uint8_t x, uniform uint8_t y) {
  return (x + y - 1) / y;
}

static inline uniform uint16_t udiv_up16(uniform uint16_t x, uniform uint16_t y) {
  return (x + y - 1) / y;
}

static inline uniform uint32_t udiv_up32(uniform uint32_t x, uniform uint32_t y) {
  return (x + y - 1) / y;
}

static inline uniform uint64_t udiv_up64(uniform uint64_t x, uniform uint64_t y) {
  return (x + y - 1) / y;
}

static inline uniform uint8_t umod8(uniform uint8_t x, uniform uint8_t y) {
  return x % y;
}

static inline uniform uint16_t umod16(uniform uint16_t x, uniform uint16_t y) {
  return x % y;
}

static inline uniform uint32_t umod32(uniform uint32_t x, uniform uint32_t y) {
  return x % y;
}

static inline uniform uint64_t umod64(uniform uint64_t x, uniform uint64_t y) {
  return x % y;
}

static inline uniform uint8_t udiv_safe8(uniform uint8_t x, uniform uint8_t y) {
  return y == 0 ? 0 : x / y;
}

static inline uniform uint16_t udiv_safe16(uniform uint16_t x, uniform uint16_t y) {
  return y == 0 ? 0 : x / y;
}

static inline uniform uint32_t udiv_safe32(uniform uint32_t x, uniform uint32_t y) {
  return y == 0 ? 0 : x / y;
}

static inline uniform uint64_t udiv_safe64(uniform uint64_t x, uniform uint64_t y) {
  return y == 0 ? 0 : x / y;
}

static inline uniform uint8_t udiv_up_safe8(uniform uint8_t x, uniform uint8_t y) {
  return y == 0 ? 0 : (x + y - 1) / y;
}

static inline uniform uint16_t udiv_up_safe16(uniform uint16_t x, uniform uint16_t y) {
  return y == 0 ? 0 : (x + y - 1) / y;
}

static inline uniform uint32_t udiv_up_safe32(uniform uint32_t x, uniform uint32_t y) {
  return y == 0 ? 0 : (x + y - 1) / y;
}

static inline uniform uint64_t udiv_up_safe64(uniform uint64_t x, uniform uint64_t y) {
  return y == 0 ? 0 : (x + y - 1) / y;
}

static inline uniform uint8_t umod_safe8(uniform uint8_t x, uniform uint8_t y) {
  return y == 0 ? 0 : x % y;
}

static inline uniform uint16_t umod_safe16(uniform uint16_t x, uniform uint16_t y) {
  return y == 0 ? 0 : x % y;
}

static inline uniform uint32_t umod_safe32(uniform uint32_t x, uniform uint32_t y) {
  return y == 0 ? 0 : x % y;
}

static inline uniform uint64_t umod_safe64(uniform uint64_t x, uniform uint64_t y) {
  return y == 0 ? 0 : x % y;
}

static inline uniform int8_t sdiv8(uniform int8_t x, uniform int8_t y) {
  uniform int8_t q = x / y;
  uniform int8_t r = x % y;

  return q - ((r != 0 && r < 0 != y < 0) ? 1 : 0);
}

static inline uniform int16_t sdiv16(uniform int16_t x, uniform int16_t y) {
  uniform int16_t q = x / y;
  uniform int16_t r = x % y;

  return q - ((r != 0 && r < 0 != y < 0) ? 1 : 0);
}

static inline uniform int32_t sdiv32(uniform int32_t x, uniform int32_t y) {
  uniform int32_t q = x / y;
  uniform int32_t r = x % y;

  return q - ((r != 0 && r < 0 != y < 0) ? 1 : 0);
}

static inline uniform int64_t sdiv64(uniform int64_t x, uniform int64_t y) {
  uniform int64_t q = x / y;
  uniform int64_t r = x % y;

  return q - ((r != 0 && r < 0 != y < 0) ? 1 : 0);
}

static inline uniform int8_t sdiv_up8(uniform int8_t x, uniform int8_t y) {
  return sdiv8(x + y - 1, y);
}

static inline uniform int16_t sdiv_up16(uniform int16_t x, uniform int16_t y) {
  return sdiv16(x + y - 1, y);
}

static inline uniform int32_t sdiv_up32(uniform int32_t x, uniform int32_t y) {
  return sdiv32(x + y - 1, y);
}

static inline uniform int64_t sdiv_up64(uniform int64_t x, uniform int64_t y) {
  return sdiv64(x + y - 1, y);
}

static inline uniform int8_t smod8(uniform int8_t x, uniform int8_t y) {
  uniform int8_t r = x % y;

  return r + (r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0) ? 0 : y);
}

static inline uniform int16_t smod16(uniform int16_t x, uniform int16_t y) {
  uniform int16_t r = x % y;

  return r + (r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0) ? 0 : y);
}

static inline uniform int32_t smod32(uniform int32_t x, uniform int32_t y) {
  uniform int32_t r = x % y;

  return r + (r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0) ? 0 : y);
}

static inline uniform int64_t smod64(uniform int64_t x, uniform int64_t y) {
  uniform int64_t r = x % y;

  return r + (r == 0 || (x > 0 && y > 0) || (x < 0 && y < 0) ? 0 : y);
}

static inline uniform int8_t sdiv_safe8(uniform int8_t x, uniform int8_t y) {
  return y == 0 ? 0 : sdiv8(x, y);
}

static inline uniform int16_t sdiv_safe16(uniform int16_t x, uniform int16_t y) {
  return y == 0 ? 0 : sdiv16(x, y);
}

static inline uniform int32_t sdiv_safe32(uniform int32_t x, uniform int32_t y) {
  return y == 0 ? 0 : sdiv32(x, y);
}

static inline uniform int64_t sdiv_safe64(uniform int64_t x, uniform int64_t y) {
  return y == 0 ? 0 : sdiv64(x, y);
}

static inline uniform int8_t sdiv_up_safe8(uniform int8_t x, uniform int8_t y) {
  return sdiv_safe8(x + y - 1, y);
}

static inline uniform int16_t sdiv_up_safe16(uniform int16_t x, uniform int16_t y) {
  return sdiv_safe16(x + y - 1, y);
}

static inline uniform int32_t sdiv_up_safe32(uniform int32_t x, uniform int32_t y) {
  return sdiv_safe32(x + y - 1, y);
}

static inline uniform int64_t sdiv_up_safe64(uniform int64_t x, uniform int64_t y) {
  return sdiv_safe64(x + y - 1, y);
}

static inline uniform int8_t smod_safe8(uniform int8_t x, uniform int8_t y) {
  return y == 0 ? 0 : smod8(x, y);
}

static inline uniform int16_t smod_safe16(uniform int16_t x, uniform int16_t y) {
  return y == 0 ? 0 : smod16(x, y);
}

static inline uniform int32_t smod_safe32(uniform int32_t x, uniform int32_t y) {
  return y == 0 ? 0 : smod32(x, y);
}

static inline uniform int64_t smod_safe64(uniform int64_t x, uniform int64_t y) {
  return y == 0 ? 0 : smod64(x, y);
}

static inline uniform int8_t squot8(uniform int8_t x, uniform int8_t y) {
  return x / y;
}

static inline uniform int16_t squot16(uniform int16_t x, uniform int16_t y) {
  return x / y;
}

static inline uniform int32_t squot32(uniform int32_t x, uniform int32_t y) {
  return x / y;
}

static inline uniform int64_t squot64(uniform int64_t x, uniform int64_t y) {
  return x / y;
}

static inline uniform int8_t srem8(uniform int8_t x, uniform int8_t y) {
  return x % y;
}

static inline uniform int16_t srem16(uniform int16_t x, uniform int16_t y) {
  return x % y;
}

static inline uniform int32_t srem32(uniform int32_t x, uniform int32_t y) {
  return x % y;
}

static inline uniform int64_t srem64(uniform int64_t x, uniform int64_t y) {
  return x % y;
}

static inline uniform int8_t squot_safe8(uniform int8_t x, uniform int8_t y) {
  return y == 0 ? 0 : x / y;
}

static inline uniform int16_t squot_safe16(uniform int16_t x, uniform int16_t y) {
  return y == 0 ? 0 : x / y;
}

static inline uniform int32_t squot_safe32(uniform int32_t x, uniform int32_t y) {
  return y == 0 ? 0 : x / y;
}

static inline uniform int64_t squot_safe64(uniform int64_t x, uniform int64_t y) {
  return y == 0 ? 0 : x / y;
}

static inline uniform int8_t srem_safe8(uniform int8_t x, uniform int8_t y) {
  return y == 0 ? 0 : x % y;
}

static inline uniform int16_t srem_safe16(uniform int16_t x, uniform int16_t y) {
  return y == 0 ? 0 : x % y;
}

static inline uniform int32_t srem_safe32(uniform int32_t x, uniform int32_t y) {
  return y == 0 ? 0 : x % y;
}

static inline uniform int64_t srem_safe64(uniform int64_t x, uniform int64_t y) {
  return y == 0 ? 0 : x % y;
}

static inline uniform int8_t smin8(uniform int8_t x, uniform int8_t y) {
  return x < y ? x : y;
}

static inline uniform int16_t smin16(uniform int16_t x, uniform int16_t y) {
  return x < y ? x : y;
}

static inline uniform int32_t smin32(uniform int32_t x, uniform int32_t y) {
  return x < y ? x : y;
}

static inline uniform int64_t smin64(uniform int64_t x, uniform int64_t y) {
  return x < y ? x : y;
}

static inline uniform uint8_t umin8(uniform uint8_t x, uniform uint8_t y) {
  return x < y ? x : y;
}

static inline uniform uint16_t umin16(uniform uint16_t x, uniform uint16_t y) {
  return x < y ? x : y;
}

static inline uniform uint32_t umin32(uniform uint32_t x, uniform uint32_t y) {
  return x < y ? x : y;
}

static inline uniform uint64_t umin64(uniform uint64_t x, uniform uint64_t y) {
  return x < y ? x : y;
}

static inline uniform int8_t smax8(uniform int8_t x, uniform int8_t y) {
  return x < y ? y : x;
}

static inline uniform int16_t smax16(uniform int16_t x, uniform int16_t y) {
  return x < y ? y : x;
}

static inline uniform int32_t smax32(uniform int32_t x, uniform int32_t y) {
  return x < y ? y : x;
}

static inline uniform int64_t smax64(uniform int64_t x, uniform int64_t y) {
  return x < y ? y : x;
}

static inline uniform uint8_t umax8(uniform uint8_t x, uniform uint8_t y) {
  return x < y ? y : x;
}

static inline uniform uint16_t umax16(uniform uint16_t x, uniform uint16_t y) {
  return x < y ? y : x;
}

static inline uniform uint32_t umax32(uniform uint32_t x, uniform uint32_t y) {
  return x < y ? y : x;
}

static inline uniform uint64_t umax64(uniform uint64_t x, uniform uint64_t y) {
  return x < y ? y : x;
}

static inline uniform uint8_t shl8(uniform uint8_t x, uniform uint8_t y) {
  return (uniform uint8_t)(x << y);
}

static inline uniform uint16_t shl16(uniform uint16_t x, uniform uint16_t y) {
  return (uniform uint16_t)(x << y);
}

static inline uniform uint32_t shl32(uniform uint32_t x, uniform uint32_t y) {
  return x << y;
}

static inline uniform uint64_t shl64(uniform uint64_t x, uniform uint64_t y) {
  return x << y;
}

static inline uniform uint8_t lshr8(uniform uint8_t x, uniform uint8_t y) {
  return x >> y;
}

static inline uniform uint16_t lshr16(uniform uint16_t x, uniform uint16_t y) {
  return x >> y;
}

static inline uniform uint32_t lshr32(uniform uint32_t x, uniform uint32_t y) {
  return x >> y;
}

static inline uniform uint64_t lshr64(uniform uint64_t x, uniform uint64_t y) {
  return x >> y;
}

static inline uniform int8_t ashr8(uniform int8_t x, uniform int8_t y) {
  return x >> y;
}

static inline uniform int16_t ashr16(uniform int16_t x, uniform int16_t y) {
  return x >> y;
}

static inline uniform int32_t ashr32(uniform int32_t x, uniform int32_t y) {
  return x >> y;
}

static inline uniform int64_t ashr64(uniform int64_t x, uniform int64_t y) {
  return x >> y;
}

static inline uniform uint8_t and8(uniform uint8_t x, uniform uint8_t y) {
  return x & y;
}

static inline uniform uint16_t and16(uniform uint16_t x, uniform uint16_t y) {
  return x & y;
}

static inline uniform uint32_t and32(uniform uint32_t x, uniform uint32_t y) {
  return x & y;
}

static inline uniform uint64_t and64(uniform uint64_t x, uniform uint64_t y) {
  return x & y;
}

static inline uniform uint8_t or8(uniform uint8_t x, uniform uint8_t y) {
  return x | y;
}

static inline uniform uint16_t or16(uniform uint16_t x, uniform uint16_t y) {
  return x | y;
}

static inline uniform uint32_t or32(uniform uint32_t x, uniform uint32_t y) {
  return x | y;
}

static inline uniform uint64_t or64(uniform uint64_t x, uniform uint64_t y) {
  return x | y;
}

static inline uniform uint8_t xor8(uniform uint8_t x, uniform uint8_t y) {
  return x ^ y;
}

static inline uniform uint16_t xor16(uniform uint16_t x, uniform uint16_t y) {
  return x ^ y;
}

static inline uniform uint32_t xor32(uniform uint32_t x, uniform uint32_t y) {
  return x ^ y;
}

static inline uniform uint64_t xor64(uniform uint64_t x, uniform uint64_t y) {
  return x ^ y;
}

static inline uniform bool ult8(uniform uint8_t x, uniform uint8_t y) {
  return x < y;
}

static inline uniform bool ult16(uniform uint16_t x, uniform uint16_t y) {
  return x < y;
}

static inline uniform bool ult32(uniform uint32_t x, uniform uint32_t y) {
  return x < y;
}

static inline uniform bool ult64(uniform uint64_t x, uniform uint64_t y) {
  return x < y;
}

static inline uniform bool ule8(uniform uint8_t x, uniform uint8_t y) {
  return x <= y;
}

static inline uniform bool ule16(uniform uint16_t x, uniform uint16_t y) {
  return x <= y;
}

static inline uniform bool ule32(uniform uint32_t x, uniform uint32_t y) {
  return x <= y;
}

static inline uniform bool ule64(uniform uint64_t x, uniform uint64_t y) {
  return x <= y;
}

static inline uniform bool slt8(uniform int8_t x, uniform int8_t y) {
  return x < y;
}

static inline uniform bool slt16(uniform int16_t x, uniform int16_t y) {
  return x < y;
}

static inline uniform bool slt32(uniform int32_t x, uniform int32_t y) {
  return x < y;
}

static inline uniform bool slt64(uniform int64_t x, uniform int64_t y) {
  return x < y;
}

static inline uniform bool sle8(uniform int8_t x, uniform int8_t y) {
  return x <= y;
}

static inline uniform bool sle16(uniform int16_t x, uniform int16_t y) {
  return x <= y;
}

static inline uniform bool sle32(uniform int32_t x, uniform int32_t y) {
  return x <= y;
}

static inline uniform bool sle64(uniform int64_t x, uniform int64_t y) {
  return x <= y;
}

static inline uniform uint8_t pow8(uniform uint8_t x, uniform uint8_t y) {
  uniform uint8_t res = 1, rem = y;

  while (rem != 0) {
    if (rem & 1)
      res *= x;
    rem >>= 1;
    x *= x;
  }
  return res;
}

static inline uniform uint16_t pow16(uniform uint16_t x, uniform uint16_t y) {
  uniform uint16_t res = 1, rem = y;

  while (rem != 0) {
    if (rem & 1)
      res *= x;
    rem >>= 1;
    x *= x;
  }
  return res;
}

static inline uniform uint32_t pow32(uniform uint32_t x, uniform uint32_t y) {
  uniform uint32_t res = 1, rem = y;

  while (rem != 0) {
    if (rem & 1)
      res *= x;
    rem >>= 1;
    x *= x;
  }
  return res;
}

static inline uniform uint64_t pow64(uniform uint64_t x, uniform uint64_t y) {
  uniform uint64_t res = 1, rem = y;

  while (rem != 0) {
    if (rem & 1)
      res *= x;
    rem >>= 1;
    x *= x;
  }
  return res;
}

static inline uniform bool itob_i8_bool(uniform int8_t x) {
  return x != 0;
}

static inline uniform bool itob_i16_bool(uniform int16_t x) {
  return x != 0;
}

static inline uniform bool itob_i32_bool(uniform int32_t x) {
  return x != 0;
}

static inline uniform bool itob_i64_bool(uniform int64_t x) {
  return x != 0;
}

static inline uniform int8_t btoi_bool_i8(uniform bool x) {
  return x;
}

static inline uniform int16_t btoi_bool_i16(uniform bool x) {
  return x;
}

static inline uniform int32_t btoi_bool_i32(uniform bool x) {
  return x;
}

static inline uniform int64_t btoi_bool_i64(uniform bool x) {
  return x;
}


static uniform int8_t abs8(uniform int8_t x) {
  return (uniform int8_t)abs(x);
}

static uniform int16_t abs16(uniform int16_t x) {
  return (uniform int16_t)abs(x);
}

static uniform int32_t abs32(uniform int32_t x) {
  return abs(x);
}

static uniform int64_t abs64(uniform int64_t x) {
  return abs(x);
}

static uniform int32_t futrts_popc8(uniform uint8_t x) {
  uniform int c = 0;
  for (; x; ++c) { x &= x - 1; }
  return c;
}

static uniform int32_t futrts_popc16(uniform uint16_t x) {
  uniform int c = 0;
  for (; x; ++c) { x &= x - 1; }
  return c;
}

static uniform int32_t futrts_popc32(uniform uint32_t x) {
  uniform int c = 0;
  for (; x; ++c) { x &= x - 1; }
  return c;
}

static uniform int32_t futrts_popc64(uniform uint64_t x) {
  uniform int c = 0;
  for (; x; ++c) { x &= x - 1; }
  return c;
}

static uniform uint8_t futrts_mul_hi8(uniform uint8_t a, uniform uint8_t b) {
  uniform uint16_t aa = a;
  uniform uint16_t bb = b;

  return aa * bb >> 8;
}

static uniform uint16_t futrts_mul_hi16(uniform uint16_t a, uniform uint16_t b) {
  uniform uint32_t aa = a;
  uniform uint32_t bb = b;

  return aa * bb >> 16;
}

static uniform uint32_t futrts_mul_hi32(uniform uint32_t a, uniform uint32_t b) {
  uniform uint64_t aa = a;
  uniform uint64_t bb = b;

  return aa * bb >> 32;
}

static uniform uint64_t futrts_mul_hi64(uniform uint64_t a, uniform uint64_t b) {
  uniform uint64_t ah = a >> 32;
  uniform uint64_t al = a & 0xffffffff;
  uniform uint64_t bh = b >> 32;
  uniform uint64_t bl = b & 0xffffffff;

  uniform uint64_t p1 = al * bl;
  uniform uint64_t p2 = al * bh;
  uniform uint64_t p3 = ah * bl;
  uniform uint64_t p4 = ah * bh;

  uniform uint64_t p1h = p1 >> 32;
  uniform uint64_t p2h = p2 >> 32;
  uniform uint64_t p3h = p3 >> 32;
  uniform uint64_t p2l = p2 & 0xffffffff;
  uniform uint64_t p3l = p3 & 0xffffffff;

  uniform uint64_t l = p1h + p2l  + p3l;
  uniform uint64_t m = (p2 >> 32) + (p3 >> 32);
  uniform uint64_t h = (l >> 32) + m + p4;

  return h;
}

static uniform uint8_t futrts_mad_hi8(uniform uint8_t a, uniform uint8_t b, uniform uint8_t c) {
  return futrts_mul_hi8(a, b) + c;
}

static uniform uint16_t futrts_mad_hi16(uniform uint16_t a, uniform uint16_t b, uniform uint16_t c) {
  return futrts_mul_hi16(a, b) + c;
}

static uniform uint32_t futrts_mad_hi32(uniform uint32_t a, uniform uint32_t b, uniform uint32_t c) {
  return futrts_mul_hi32(a, b) + c;
}

static uniform uint64_t futrts_mad_hi64(uniform uint64_t a, uniform uint64_t b, uniform uint64_t c) {
  return futrts_mul_hi64(a, b) + c;
}

static uniform int32_t futrts_clzz8(uniform int8_t x) {
  return count_leading_zeros((uniform int32_t)(uniform uint8_t)x)-24;
}

static uniform int32_t futrts_clzz16(uniform int16_t x) {
  return count_leading_zeros((uniform int32_t)(uniform uint16_t)x)-16;
}

static uniform int32_t futrts_clzz32(uniform int32_t x) {
  return count_leading_zeros(x);
}

static uniform int32_t futrts_clzz64(uniform int64_t x) {
  return count_leading_zeros(x);
}

static uniform int32_t futrts_ctzz8(uniform int8_t x) {
  return x == 0 ? 8 : count_trailing_zeros((uniform int32_t)x);
}

static uniform int32_t futrts_ctzz16(uniform int16_t x) {
  return x == 0 ? 16 : count_trailing_zeros((uniform int32_t)x);
}

static uniform int32_t futrts_ctzz32(uniform int32_t x) {
  return count_trailing_zeros(x);
}

static uniform int32_t futrts_ctzz64(uniform int64_t x) {
  return count_trailing_zeros(x);
}


static inline uniform float fdiv32(uniform float x, uniform float y) {
  return x / y;
}

static inline uniform float fadd32(uniform float x, uniform float y) {
  return x + y;
}

static inline uniform float fsub32(uniform float x, uniform float y) {
  return x - y;
}

static inline uniform float fmul32(uniform float x, uniform float y) {
  return x * y;
}

static inline uniform bool cmplt32(uniform float x, uniform float y) {
  return x < y;
}

static inline uniform bool cmple32(uniform float x, uniform float y) {
  return x <= y;
}

static inline uniform float sitofp_i8_f32(uniform int8_t x) {
  return (uniform float) x;
}

static inline uniform float sitofp_i16_f32(uniform int16_t x) {
  return (uniform float) x;
}

static inline uniform float sitofp_i32_f32(uniform int32_t x) {
  return (uniform float) x;
}

static inline uniform float sitofp_i64_f32(uniform int64_t x) {
  return (uniform float) x;
}

static inline uniform float uitofp_i8_f32(uniform uint8_t x) {
  return (uniform float) x;
}

static inline uniform float uitofp_i16_f32(uniform uint16_t x) {
  return (uniform float) x;
}

static inline uniform float uitofp_i32_f32(uniform uint32_t x) {
  return (uniform float) x;
}

static inline uniform float uitofp_i64_f32(uniform uint64_t x) {
  return (uniform float) x;
}


static inline uniform float fabs32(uniform float x) {
  return abs(x);
}

static inline uniform float fmax32(uniform float x, uniform float y) {
  return isnan(x) ? y : isnan(y) ? x : max(x, y);
}

static inline uniform float fmin32(uniform float x, uniform float y) {
  return isnan(x) ? y : isnan(y) ? x : min(x, y);
}

static inline uniform float fpow32(uniform float x, uniform float y) {
  return pow(x, y);
}

static inline uniform bool futrts_isnan32(uniform float x) {
  return isnan(x);
}

static inline uniform bool futrts_isinf32(uniform float x) {
  return !isnan(x) && isnan(x - x);
}
static inline uniform bool futrts_isfinite32(uniform float x) {
  return !isnan(x) && !futrts_isinf32(x);
}


static inline uniform int8_t fptosi_f32_i8(uniform float x) {
  if (futrts_isnan32(x) || futrts_isinf32(x)) {
    return 0;
  } else {
    return (uniform int8_t) x;
  }
}

static inline uniform int16_t fptosi_f32_i16(uniform float x) {
  if (futrts_isnan32(x) || futrts_isinf32(x)) {
    return 0;
  } else {
    return (uniform int16_t) x;
  }
}

static inline uniform int32_t fptosi_f32_i32(uniform float x) {
  if (futrts_isnan32(x) || futrts_isinf32(x)) {
    return 0;
  } else {
    return (uniform int32_t) x;
  }
}

static inline uniform int64_t fptosi_f32_i64(uniform float x) {
  if (futrts_isnan32(x) || futrts_isinf32(x)) {
    return 0;
  } else {
    return (uniform int64_t) x;
  };
}

static inline uniform uint8_t fptoui_f32_i8(uniform float x) {
  if (futrts_isnan32(x) || futrts_isinf32(x)) {
    return 0;
  } else {
    return (uniform uint8_t) (uniform int8_t) x;
  }
}

static inline uniform uint16_t fptoui_f32_i16(uniform float x) {
  if (futrts_isnan32(x) || futrts_isinf32(x)) {
    return 0;
  } else {
    return (uniform uint16_t) (uniform int16_t) x;
  }
}

static inline uniform uint32_t fptoui_f32_i32(uniform float x) {
  if (futrts_isnan32(x) || futrts_isinf32(x)) {
    return 0;
  } else {
    return (uniform uint32_t) (uniform int32_t) x;
  }
}

static inline uniform uint64_t fptoui_f32_i64(uniform float x) {
  if (futrts_isnan32(x) || futrts_isinf32(x)) {
    return 0;
  } else {
    return (uniform uint64_t) (uniform int64_t) x;
  }
}


static inline uniform float futrts_log32(uniform float x) {
  return futrts_isfinite32(x) || (futrts_isinf32(x) && x < 0)? log(x) : x;
}

static inline uniform float futrts_log2_32(uniform float x) {
  return futrts_log32(x) / log(2.0f);
}

static inline uniform float futrts_log10_32(uniform float x) {
  return futrts_log32(x) / log(10.0f);
}

static inline uniform float futrts_log1p_32(uniform float x) {
  if(x == -1.0f || (futrts_isinf32(x) && x > 0.0f)) return x / 0.0f;
  uniform float y = 1.0f + x;
  uniform float z = y - 1.0f;
  return log(y) - (z-x)/y;
}

static inline uniform float futrts_sqrt32(uniform float x) {
  return sqrt(x);
}

extern "C" unmasked uniform float cbrtf(uniform float);
static inline uniform float futrts_cbrt32(uniform float x) {
  return cbrtf(x);
}

static inline uniform float futrts_exp32(uniform float x) {
  return exp(x);
}

static inline uniform float futrts_cos32(uniform float x) {
  return cos(x);
}

static inline uniform float futrts_sin32(uniform float x) {
  return sin(x);
}

static inline uniform float futrts_tan32(uniform float x) {
  return tan(x);
}

static inline uniform float futrts_acos32(uniform float x) {
  return acos(x);
}

static inline uniform float futrts_asin32(uniform float x) {
  return asin(x);
}

static inline uniform float futrts_atan32(uniform float x) {
  return atan(x);
}

static inline uniform float futrts_cosh32(uniform float x) {
  return (exp(x)+exp(-x)) / 2.0f;
}

static inline uniform float futrts_sinh32(uniform float x) {
  return (exp(x)-exp(-x)) / 2.0f;
}

static inline uniform float futrts_tanh32(uniform float x) {
  return futrts_sinh32(x)/futrts_cosh32(x);
}

static inline uniform float futrts_acosh32(uniform float x) {
  uniform float f = x+sqrt(x*x-1);
  if(futrts_isfinite32(f)) return log(f);
  return f;
}

static inline uniform float futrts_asinh32(uniform float x) {
  uniform float f = x+sqrt(x*x+1);
  if(futrts_isfinite32(f)) return log(f);
  return f;

}

static inline uniform float futrts_atanh32(uniform float x) {
  uniform float f = (1+x)/(1-x);
  if(futrts_isfinite32(f)) return log(f)/2.0f;
  return f;

}

static inline uniform float futrts_atan2_32(uniform float x, uniform float y) {
  return (x == 0.0f && y == 0.0f) ? 0.0f : atan2(x, y);
}

static inline uniform float futrts_hypot32(uniform float x, uniform float y) {
  if (futrts_isfinite32(x) && futrts_isfinite32(y)) {
    x = abs(x);
    y = abs(y);
    uniform float a;
    uniform float b;
    if (x >= y){
        a = x;
        b = y;
    } else {
        a = y;
        b = x;
    }
    if(b == 0){
      return a;
    }

    uniform int e;
    uniform float an;
    uniform float bn;
    an = frexp (a, &e);
    bn = ldexp (b, - e);
    uniform float cn;
    cn = sqrt (an * an + bn * bn);
    return ldexp (cn, e);
  } else {
    if (futrts_isinf32(x) || futrts_isinf32(y)) return INFINITY;
    else return x + y;
  }

}

extern "C" unmasked uniform float tgammaf(uniform float x);
static inline uniform float futrts_gamma32(uniform float x) {
  return tgammaf(x);
}

extern "C" unmasked uniform float tgammaf(uniform float x);
static inline uniform float futrts_lgamma32(uniform float x) {
  return lgammaf(x);
}

extern "C" unmasked uniform float erff(uniform float);
static inline uniform float futrts_erf32(uniform float x) {
  return erff(x);
}

extern "C" unmasked uniform float erfcf(uniform float);
static inline uniform float futrts_erfc32(uniform float x) {
  return erfcf(x);
}

static inline uniform float fmod32(uniform float x, uniform float y) {
  return x - y * trunc(x/y);
}

static inline uniform float futrts_round32(uniform float x) {
  return round(x);
}

static inline uniform float futrts_floor32(uniform float x) {
  return floor(x);
}

static inline uniform float futrts_ceil32(uniform float x) {
  return ceil(x);
}

static inline uniform float futrts_lerp32(uniform float v0, uniform float v1, uniform float t) {
  return v0 + (v1 - v0) * t;
}

static inline uniform float futrts_mad32(uniform float a, uniform float b, uniform float c) {
  return a * b + c;
}

static inline uniform float futrts_fma32(uniform float a, uniform float b, uniform float c) {
  return a * b + c;
}

static inline uniform int32_t futrts_to_bits32(uniform float x) {
  return intbits(x);
}

static inline uniform float futrts_from_bits32(uniform int32_t x) {
  return floatbits(x);
}

static inline uniform float fsignum32(uniform float x) {
  return futrts_isnan32(x) ? x : (x > 0 ? 1 : 0) - (x < 0 ? 1 : 0);
}

#ifdef FUTHARK_F64_ENABLED

static inline uniform bool futrts_isinf64(uniform float x) {
  return !isnan(x) && isnan(x - x);
}
static inline uniform bool futrts_isfinite64(uniform float x) {
  return !isnan(x) && !futrts_isinf64(x);
}

static inline uniform double fdiv64(uniform double x, uniform double y) {
  return x / y;
}

static inline uniform double fadd64(uniform double x, uniform double y) {
  return x + y;
}

static inline uniform double fsub64(uniform double x, uniform double y) {
  return x - y;
}

static inline uniform double fmul64(uniform double x, uniform double y) {
  return x * y;
}

static inline uniform bool cmplt64(uniform double x, uniform double y) {
  return x < y;
}

static inline uniform bool cmple64(uniform double x, uniform double y) {
  return x <= y;
}

static inline uniform double sitofp_i8_f64(uniform int8_t x) {
  return (uniform double) x;
}

static inline uniform double sitofp_i16_f64(uniform int16_t x) {
  return (uniform double) x;
}

static inline uniform double sitofp_i32_f64(uniform int32_t x) {
  return (uniform double) x;
}

static inline uniform double sitofp_i64_f64(uniform int64_t x) {
  return (uniform double) x;
}

static inline uniform double uitofp_i8_f64(uniform uint8_t x) {
  return (uniform double) x;
}

static inline uniform double uitofp_i16_f64(uniform uint16_t x) {
  return (uniform double) x;
}

static inline uniform double uitofp_i32_f64(uniform uint32_t x) {
  return (uniform double) x;
}

static inline uniform double uitofp_i64_f64(uniform uint64_t x) {
  return (uniform double) x;
}

static inline uniform double fabs64(uniform double x) {
  return abs(x);
}

static inline uniform double fmax64(uniform double x, uniform double y) {
  return isnan(x) ? y : isnan(y) ? x : max(x, y);
}

static inline uniform double fmin64(uniform double x, uniform double y) {
  return isnan(x) ? y : isnan(y) ? x : min(x, y);
}

static inline uniform double fpow64(uniform double x, uniform double y) {
  return pow(x, y);
}

static inline uniform double futrts_log64(uniform double x) {
  return futrts_isfinite64(x) || (futrts_isinf64(x) && x < 0)? log(x) : x;
}

static inline uniform double futrts_log2_64(uniform double x) {
  return futrts_log64(x)/log(2.0d);
}

static inline uniform double futrts_log10_64(uniform double x) {
  return futrts_log64(x)/log(10.0d);
}

static inline uniform double futrts_log1p_64(uniform double x) {
  if(x == -1.0d || (futrts_isinf64(x) && x > 0.0d)) return x / 0.0d;
  uniform double y = 1.0d + x;
  uniform double z = y - 1.0d;
  return log(y) - (z-x)/y;
}

static inline uniform double futrts_sqrt64(uniform double x) {
  return sqrt(x);
}

extern "C" unmasked uniform double cbrt(uniform double);
static inline uniform double futrts_cbrt64(uniform double x) {
  return cbrt(x);
}

static inline uniform double futrts_exp64(uniform double x) {
  return exp(x);
}

static inline uniform double futrts_cos64(uniform double x) {
  return cos(x);
}

static inline uniform double futrts_sin64(uniform double x) {
  return sin(x);
}

static inline uniform double futrts_tan64(uniform double x) {
  return tan(x);
}

static inline uniform double futrts_acos64(uniform double x) {
  return acos(x);
}

static inline uniform double futrts_asin64(uniform double x) {
  return asin(x);
}

static inline uniform double futrts_atan64(uniform double x) {
  return atan(x);
}

static inline uniform double futrts_cosh64(uniform double x) {
  return (exp(x)+exp(-x)) / 2.0d;
}

static inline uniform double futrts_sinh64(uniform double x) {
  return (exp(x)-exp(-x)) / 2.0d;
}

static inline uniform double futrts_tanh64(uniform double x) {
  return futrts_sinh64(x)/futrts_cosh64(x);
}

static inline uniform double futrts_acosh64(uniform double x) {
  uniform double f = x+sqrt(x*x-1.0d);
  if(futrts_isfinite64(f)) return log(f);
  return f;
}

static inline uniform double futrts_asinh64(uniform double x) {
  uniform double f = x+sqrt(x*x+1.0d);
  if(futrts_isfinite64(f)) return log(f);
  return f;
}

static inline uniform double futrts_atanh64(uniform double x) {
  uniform double f = (1.0d+x)/(1.0d-x);
  if(futrts_isfinite64(f)) return log(f)/2.0d;
  return f;

}

static inline uniform double futrts_atan2_64(uniform double x, uniform double y) {
  return atan2(x, y);
}

extern "C" unmasked uniform double hypot(uniform double x, uniform double y);
static inline uniform double futrts_hypot64(uniform double x, uniform double y) {
  return hypot(x, y);
}

extern "C" unmasked uniform double tgamma(uniform double x);
static inline uniform double futrts_gamma64(uniform double x) {
  return tgamma(x);
}

extern "C" unmasked uniform double lgamma(uniform double x);
static inline uniform double futrts_lgamma64(uniform double x) {
  return lgamma(x);
}

extern "C" unmasked uniform double erf(uniform double);
static inline uniform double futrts_erf64(uniform double x) {
  return erf(x);
}

extern "C" unmasked uniform double erfc(uniform double);
static inline uniform double futrts_erfc64(uniform double x) {
  return erfc(x);
}

static inline uniform double futrts_fma64(uniform double a, uniform double b, uniform double c) {
  return a * b + c;
}

static inline uniform double futrts_round64(uniform double x) {
  return round(x);
}

static inline uniform double futrts_ceil64(uniform double x) {
  return ceil(x);
}

static inline uniform double futrts_floor64(uniform double x) {
  return floor(x);
}

static inline uniform bool futrts_isnan64(uniform double x) {
  return isnan(x);
}

static inline uniform int8_t fptosi_f64_i8(uniform double x) {
  if (futrts_isnan64(x) || futrts_isinf64(x)) {
    return 0;
  } else {
    return (uniform int8_t) x;
  }
}

static inline uniform int16_t fptosi_f64_i16(uniform double x) {
  if (futrts_isnan64(x) || futrts_isinf64(x)) {
    return 0;
  } else {
    return (uniform int16_t) x;
  }
}

static inline uniform int32_t fptosi_f64_i32(uniform double x) {
  if (futrts_isnan64(x) || futrts_isinf64(x)) {
    return 0;
  } else {
    return (uniform int32_t) x;
  }
}

static inline uniform int64_t fptosi_f64_i64(uniform double x) {
  if (futrts_isnan64(x) || futrts_isinf64(x)) {
    return 0;
  } else {
    return (uniform int64_t) x;
  }
}

static inline uniform uint8_t fptoui_f64_i8(uniform double x) {
  if (futrts_isnan64(x) || futrts_isinf64(x)) {
    return 0;
  } else {
    return (uniform uint8_t) (uniform int8_t) x;
  }
}

static inline uniform uint16_t fptoui_f64_i16(uniform double x) {
  if (futrts_isnan64(x) || futrts_isinf64(x)) {
    return 0;
  } else {
    return (uniform uint16_t) (uniform int16_t) x;
  }
}

static inline uniform uint32_t fptoui_f64_i32(uniform double x) {
  if (futrts_isnan64(x) || futrts_isinf64(x)) {
    return 0;
  } else {
    return (uniform uint32_t) (uniform int32_t) x;
  }
}

static inline uniform uint64_t fptoui_f64_i64(uniform double x) {
  if (futrts_isnan64(x) || futrts_isinf64(x)) {
    return 0;
  } else {
    return (uniform uint64_t) (uniform int64_t) x;
  }
}

static inline uniform bool ftob_f64_bool(uniform double x) {
  return x != 0.0;
}

static inline uniform double btof_bool_f64(uniform bool x) {
  return x ? 1.0 : 0.0;
}

static inline uniform bool ftob_f32_bool(uniform float x) {
  return x != 0;
}

static inline uniform float btof_bool_f32(uniform bool x) {
  return x ? 1 : 0;
}

static inline uniform int64_t futrts_to_bits64(uniform double x) {
  return *((uniform int64_t* uniform)&x);
}

static inline uniform double futrts_from_bits64(uniform int64_t x) {
  return *((uniform double* uniform)&x);
}

static inline uniform double fmod64(uniform double x, uniform double y) {
  return x - y * trunc(x/y);
}

static inline uniform double fsignum64(uniform double x) {
  return futrts_isnan64(x) ? x : (x > 0 ? 1.0d : 0.0d) - (x < 0 ? 1.0d : 0.0d);
}

static inline uniform double futrts_lerp64(uniform double v0, uniform double v1, uniform double t) {
  return v0 + (v1 - v0) * t;
}

static inline uniform double futrts_mad64(uniform double a, uniform double b, uniform double c) {
  return a * b + c;
}

static inline uniform float fpconv_f32_f32(uniform float x) {
  return (uniform float) x;
}

static inline uniform double fpconv_f32_f64(uniform float x) {
  return (uniform double) x;
}

static inline uniform float fpconv_f64_f32(uniform double x) {
  return (uniform float) x;
}

static inline uniform double fpconv_f64_f64(uniform double x) {
  return (uniform double) x;
}

static inline uniform double fpconv_f16_f64(uniform f16 x) {
  return (uniform double) x;
}

static inline uniform f16 fpconv_f64_f16(uniform double x) {
  return (uniform f16) ((uniform float)x);
}

#endif


static inline uniform f16 fadd16(uniform f16 x, uniform f16 y) {
  return x + y;
}

static inline uniform f16 fsub16(uniform f16 x, uniform f16 y) {
  return x - y;
}

static inline uniform f16 fmul16(uniform f16 x, uniform f16 y) {
  return x * y;
}

static inline uniform bool cmplt16(uniform f16 x, uniform f16 y) {
  return x < y;
}

static inline uniform bool cmple16(uniform f16 x, uniform f16 y) {
  return x <= y;
}

static inline uniform f16 sitofp_i8_f16(uniform int8_t x) {
  return (uniform f16) x;
}

static inline uniform f16 sitofp_i16_f16(uniform int16_t x) {
  return (uniform f16) x;
}

static inline uniform f16 sitofp_i32_f16(uniform int32_t x) {
  return (uniform f16) x;
}

static inline uniform f16 sitofp_i64_f16(uniform int64_t x) {
  return (uniform f16) x;
}

static inline uniform f16 uitofp_i8_f16(uniform uint8_t x) {
  return (uniform f16) x;
}

static inline uniform f16 uitofp_i16_f16(uniform uint16_t x) {
  return (uniform f16) x;
}

static inline uniform f16 uitofp_i32_f16(uniform uint32_t x) {
  return (uniform f16) x;
}

static inline uniform f16 uitofp_i64_f16(uniform uint64_t x) {
  return (uniform f16) x;
}

static inline uniform int8_t fptosi_f16_i8(uniform f16 x) {
  return (uniform int8_t) (uniform float) x;
}

static inline uniform int16_t fptosi_f16_i16(uniform f16 x) {
  return (uniform int16_t) x;
}

static inline uniform int32_t fptosi_f16_i32(uniform f16 x) {
  return (uniform int32_t) x;
}

static inline uniform int64_t fptosi_f16_i64(uniform f16 x) {
  return (uniform int64_t) x;
}

static inline uniform uint8_t fptoui_f16_i8(uniform f16 x) {
  return (uniform uint8_t) (uniform float) x;
}

static inline uniform uint16_t fptoui_f16_i16(uniform f16 x) {
  return (uniform uint16_t) x;
}

static inline uniform uint32_t fptoui_f16_i32(uniform f16 x) {
  return (uniform uint32_t) x;
}

static inline uniform uint64_t fptoui_f16_i64(uniform f16 x) {
  return (uniform uint64_t) x;
}

static inline uniform f16 fabs16(uniform f16 x) {
  return abs(x);
}

static inline uniform bool futrts_isnan16(uniform f16 x) {
  return isnan((uniform float)x);
}

static inline uniform f16 fmax16(uniform f16 x, uniform f16 y) {
  return futrts_isnan16(x) ? y : futrts_isnan16(y) ? x : max(x, y);
}

static inline uniform f16 fmin16(uniform f16 x, uniform f16 y) {
  return min(x, y);
}

static inline uniform f16 fpow16(uniform f16 x, uniform f16 y) {
  return pow(x, y);
}

static inline uniform bool futrts_isinf16(uniform float x) {
  return !futrts_isnan16(x) && futrts_isnan16(x - x);
}
static inline uniform bool futrts_isfinite16(uniform float x) {
  return !futrts_isnan16(x) && !futrts_isinf16(x);
}


static inline uniform f16 futrts_log16(uniform f16 x) {
  return futrts_isfinite16(x) || (futrts_isinf16(x) && x < 0)? log(x) : x;
}

static inline uniform f16 futrts_log2_16(uniform f16 x) {
  return futrts_log16(x) / log(2.0f16);
}

static inline uniform f16 futrts_log10_16(uniform f16 x) {
  return futrts_log16(x) / log(10.0f16);
}

static inline uniform f16 futrts_log1p_16(uniform f16 x) {
  if(x == -1.0f16 || (futrts_isinf16(x) && x > 0.0f16)) return x / 0.0f16;
  uniform f16 y = 1.0f16 + x;
  uniform f16 z = y - 1.0f16;
  return log(y) - (z-x)/y;
}

static inline uniform f16 futrts_sqrt16(uniform f16 x) {
  return (uniform f16)sqrt((uniform float)x);
}

extern "C" unmasked uniform float cbrtf(uniform float);
static inline uniform f16 futrts_cbrt16(uniform f16 x) {
  return (uniform f16)cbrtf((uniform float)x);
}

static inline uniform f16 futrts_exp16(uniform f16 x) {
  return exp(x);
}

static inline uniform f16 futrts_cos16(uniform f16 x) {
  return (uniform f16)cos((uniform float)x);
}

static inline uniform f16 futrts_sin16(uniform f16 x) {
  return (uniform f16)sin((uniform float)x);
}

static inline uniform f16 futrts_tan16(uniform f16 x) {
  return (uniform f16)tan((uniform float)x);
}

static inline uniform f16 futrts_acos16(uniform f16 x) {
  return (uniform f16)acos((uniform float)x);
}

static inline uniform f16 futrts_asin16(uniform f16 x) {
  return (uniform f16)asin((uniform float)x);
}

static inline uniform f16 futrts_atan16(uniform f16 x) {
  return (uniform f16)atan((uniform float)x);
}

static inline uniform f16 futrts_cosh16(uniform f16 x) {
  return (exp(x)+exp(-x)) / 2.0f16;
}

static inline uniform f16 futrts_sinh16(uniform f16 x) {
  return (exp(x)-exp(-x)) / 2.0f16;
}

static inline uniform f16 futrts_tanh16(uniform f16 x) {
  return futrts_sinh16(x)/futrts_cosh16(x);
}

static inline uniform f16 futrts_acosh16(uniform f16 x) {
  uniform f16 f = x+(uniform f16)sqrt((uniform float)(x*x-1));
  if(futrts_isfinite16(f)) return log(f);
  return f;
}

static inline uniform f16 futrts_asinh16(uniform f16 x) {
  uniform f16 f = x+(uniform f16)sqrt((uniform float)(x*x+1));
  if(futrts_isfinite16(f)) return log(f);
  return f;
}

static inline uniform f16 futrts_atanh16(uniform f16 x) {
  uniform f16 f = (1+x)/(1-x);
  if(futrts_isfinite16(f)) return log(f)/2.0f16;
  return f;
}

static inline uniform f16 futrts_atan2_16(uniform f16 x, uniform f16 y) {
  return (uniform f16)atan2((uniform float)x, (uniform float)y);
}

static inline uniform f16 futrts_hypot16(uniform f16 x, uniform f16 y) {
  return (uniform f16)futrts_hypot32((uniform float)x, (uniform float)y);
}

extern "C" unmasked uniform float tgammaf(uniform float x);
static inline uniform f16 futrts_gamma16(uniform f16 x) {
  return (uniform f16)tgammaf((uniform float)x);
}

extern "C" unmasked uniform float lgammaf(uniform float x);
static inline uniform f16 futrts_lgamma16(uniform f16 x) {
  return (uniform f16)lgammaf((uniform float)x);
}

extern "C" unmasked uniform float erff(uniform float);
static inline uniform f16 futrts_erf32(uniform f16 x) {
  return (uniform f16)erff((uniform float)x);
}

extern "C" unmasked uniform float erfcf(uniform float);
static inline uniform f16 futrts_erfc32(uniform f16 x) {
  return (uniform f16)erfcf((uniform float)x);
}

static inline uniform f16 fmod16(uniform f16 x, uniform f16 y) {
  return x - y * (uniform f16)trunc((uniform float) (x/y));
}

static inline uniform f16 futrts_round16(uniform f16 x) {
  return (uniform f16)round((uniform float)x);
}

static inline uniform f16 futrts_floor16(uniform f16 x) {
  return (uniform f16)floor((uniform float)x);
}

static inline uniform f16 futrts_ceil16(uniform f16 x) {
  return (uniform f16)ceil((uniform float)x);
}

static inline uniform f16 futrts_lerp16(uniform f16 v0, uniform f16 v1, uniform f16 t) {
  return v0 + (v1 - v0) * t;
}

static inline uniform f16 futrts_mad16(uniform f16 a, uniform f16 b, uniform f16 c) {
  return a * b + c;
}

static inline uniform f16 futrts_fma16(uniform f16 a, uniform f16 b, uniform f16 c) {
  return a * b + c;
}

static inline uniform int16_t fptobits_f16_i16(uniform f16 x) {
  return *((uniform int16_t *)&x);
}

static inline uniform f16 bitstofp_i16_f16(uniform int16_t x) {
  return *((uniform f16 *)&x);
}

static inline uniform float fpconv_f16_f16(uniform f16 x) {
  return x;
}

static inline uniform float fpconv_f16_f32(uniform f16 x) {
  return x;
}

static inline uniform f16 fpconv_f32_f16(uniform float x) {
  return (uniform f16) x;
}
#endif

// End of uniform.h.
