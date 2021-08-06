-- | Basic mathematical modules and functions.

import "soacs"

-- | Describes types of values that can be created from the primitive
-- numeric types (and bool).
module type from_prim = {
  type t

  val i8: i8 -> t
  val i16: i16 -> t
  val i32: i32 -> t
  val i64: i64 -> t

  val u8: u8 -> t
  val u16: u16 -> t
  val u32: u32 -> t
  val u64: u64 -> t

  val f16: f16 -> t
  val f32: f32 -> t
  val f64: f64 -> t

  val bool: bool -> t
}

-- | A basic numeric module type that can be implemented for both
-- integers and rational numbers.
module type numeric = {
  include from_prim

  val +: t -> t -> t
  val -: t -> t -> t
  val *: t -> t -> t
  val /: t -> t -> t
  val %: t -> t -> t
  val **: t -> t -> t

  val to_i64: t -> i64

  val ==: t -> t -> bool
  val <: t -> t -> bool
  val >: t -> t -> bool
  val <=: t -> t -> bool
  val >=: t -> t -> bool
  val !=: t -> t -> bool

  -- | Arithmetic negation (use `!` for bitwise negation).
  val neg: t -> t
  val max: t -> t -> t
  val min: t -> t -> t

  val abs: t -> t

  -- | Sign function.  Produces -1, 0, or 1 if the argument is
  -- respectively less than, equal to, or greater than zero.
  val sgn: t -> t

  -- | The most positive representable number.
  val highest: t

  -- | The least positive representable number (most negative for
  -- signed types).
  val lowest: t

  -- | Returns zero on empty input.
  val sum [n]: [n]t -> t

  -- | Returns one on empty input.
  val product [n]: [n]t -> t

  -- | Returns `lowest` on empty input.
  val maximum [n]: [n]t -> t
  -- | Returns `highest` on empty input.
  val minimum [n]: [n]t -> t
}

-- | An extension of `numeric`@mtype that provides facilities that are
-- only meaningful for integral types.
module type integral = {
  include numeric

  -- | Like `/`@term, but rounds towards zero.  This only matters when
  -- one of the operands is negative.  May be more efficient.
  val //: t -> t -> t
  -- | Like `%`@term, but rounds towards zero.  This only matters when
  -- one of the operands is negative.  May be more efficient.
  val %%: t -> t -> t

  val &: t -> t -> t
  val |: t -> t -> t
  val ^: t -> t -> t

  -- | Bitwise negation.
  val not: t -> t

  val <<: t -> t -> t
  val >>: t -> t -> t
  val >>>: t -> t -> t

  val num_bits: i32
  val get_bit: i32 -> t -> i32
  val set_bit: i32 -> t -> i32 -> t

  -- | Count number of one bits.
  val popc: t -> i32

  -- | Computes `x * y` and returns the high half of the product of x
  -- and y.
  val mul_hi: (x: t) -> (y: t) -> t

  -- | Computes `mul_hi a b + c`, but perhaps in a more efficient way,
  -- depending on the target platform.
  val mad_hi: (a: t) -> (b: t) -> (c: t) -> t

  -- | Count number of zero bits preceding the most significant set
  -- bit.  Returns the number of bits in the type if the argument is
  -- zero.
  val clz: t -> i32

  -- | Count number of trailing zero bits following the least
  -- significant set bit.  Returns the number of bits in the type if
  -- the argument is zero.
  val ctz: t -> i32
}

-- | Numbers that model real numbers to some degree.
module type real = {
  include numeric

  -- | Multiplicative inverse.
  val recip: t -> t

  val from_fraction: i64 -> i64 -> t
  val to_i64: t -> i64
  val to_f64: t -> f64

  val sqrt: t -> t
  val exp: t -> t

  val sin: t -> t
  val cos: t -> t
  val tan: t -> t

  val asin: t -> t
  val acos: t -> t
  val atan: t -> t

  val sinh: t -> t
  val cosh: t -> t
  val tanh: t -> t

  val asinh: t -> t
  val acosh: t -> t
  val atanh: t -> t

  val atan2: t -> t -> t

  val hypot: t -> t -> t

  val gamma: t -> t
  val lgamma: t -> t
  -- | Linear interpolation.  The third argument must be in the range
  -- `[0,1]` or the results are unspecified.
  val lerp: t -> t -> t -> t

  -- | Natural logarithm.
  val log: t -> t
  -- | Base-2 logarithm.
  val log2: t -> t
  -- | Base-10 logarithm.
  val log10: t -> t

  val ceil : t -> t
  val floor : t -> t
  val trunc : t -> t

  -- | Computes `a*b+c`.  Depending on the compiler backend, this may
  -- be fused into a single operation that is faster but less
  -- accurate.  Do not confuse it with `fma`@term.
  val mad : (a: t) -> (b: t) -> (c: t) -> t

  -- | Computes `a*b+c`, with `a*b` being rounded with infinite
  -- precision.  Rounding of intermediate products shall not
  -- occur. Edge case behavior is per the IEEE 754-2008 standard.
  val fma : (a: t) -> (b: t) -> (c: t) -> t

  -- | Round to the nearest integer, with alfway cases rounded to the
  -- nearest even integer.  Note that this differs from `round()` in
  -- C, but matches more modern languages.
  val round : t -> t

  val isinf: t -> bool
  val isnan: t -> bool

  val inf: t
  val nan: t

  val pi: t
  val e: t
}

-- | An extension of `real`@mtype that further gives access to the
-- bitwise representation of the underlying number.  It is presumed
-- that this will be some form of IEEE float.
module type float = {
  include real

  -- | An unsigned integer type containing the same number of bits as
  -- 't'.
  type int_t

  val from_bits: int_t -> t
  val to_bits: t -> int_t

  val num_bits: i32
  val get_bit: i32 -> t -> i32
  val set_bit: i32 -> t -> i32 -> t

  -- | The difference between 1.0 and the next larger representable
  -- number.
  val epsilon: t
}

-- | Boolean numbers.  When converting from a number to `bool`, 0 is
-- considered `false` and any other value is `true`.
module bool: from_prim with t = bool = {
  type t = bool

  let i8  = intrinsics.itob_i8_bool
  let i16 = intrinsics.itob_i16_bool
  let i32 = intrinsics.itob_i32_bool
  let i64 = intrinsics.itob_i64_bool

  let u8  (x: u8)  = intrinsics.itob_i8_bool (intrinsics.sign_i8 x)
  let u16 (x: u16) = intrinsics.itob_i16_bool (intrinsics.sign_i16 x)
  let u32 (x: u32) = intrinsics.itob_i32_bool (intrinsics.sign_i32 x)
  let u64 (x: u64) = intrinsics.itob_i64_bool (intrinsics.sign_i64 x)

  let f16 (x: f16) = x != 0f16
  let f32 (x: f32) = x != 0f32
  let f64 (x: f64) = x != 0f64

  let bool (x: bool) = x
}

module i8: (integral with t = i8) = {
  type t = i8

  let (x: i8) + (y: i8) = intrinsics.add8 (x, y)
  let (x: i8) - (y: i8) = intrinsics.sub8 (x, y)
  let (x: i8) * (y: i8) = intrinsics.mul8 (x, y)
  let (x: i8) / (y: i8) = intrinsics.sdiv8 (x, y)
  let (x: i8) ** (y: i8) = intrinsics.pow8 (x, y)
  let (x: i8) % (y: i8) = intrinsics.smod8 (x, y)
  let (x: i8) // (y: i8) = intrinsics.squot8 (x, y)
  let (x: i8) %% (y: i8) = intrinsics.srem8 (x, y)

  let (x: i8) & (y: i8) = intrinsics.and8 (x, y)
  let (x: i8) | (y: i8) = intrinsics.or8 (x, y)
  let (x: i8) ^ (y: i8) = intrinsics.xor8 (x, y)
  let not (x: i8) = intrinsics.complement8 x

  let (x: i8) << (y: i8) = intrinsics.shl8 (x, y)
  let (x: i8) >> (y: i8) = intrinsics.ashr8 (x, y)
  let (x: i8) >>> (y: i8) = intrinsics.lshr8 (x, y)

  let i8  (x: i8)  = intrinsics.sext_i8_i8 x
  let i16 (x: i16) = intrinsics.sext_i16_i8 x
  let i32 (x: i32) = intrinsics.sext_i32_i8 x
  let i64 (x: i64) = intrinsics.sext_i64_i8 x

  let u8  (x: u8)  = intrinsics.zext_i8_i8 (intrinsics.sign_i8 x)
  let u16 (x: u16) = intrinsics.zext_i16_i8 (intrinsics.sign_i16 x)
  let u32 (x: u32) = intrinsics.zext_i32_i8 (intrinsics.sign_i32 x)
  let u64 (x: u64) = intrinsics.zext_i64_i8 (intrinsics.sign_i64 x)

  let f16 (x: f16) = intrinsics.fptosi_f16_i8 x
  let f32 (x: f32) = intrinsics.fptosi_f32_i8 x
  let f64 (x: f64) = intrinsics.fptosi_f64_i8 x

  let bool = intrinsics.btoi_bool_i8

  let to_i32(x: i8) = intrinsics.sext_i8_i32 x
  let to_i64(x: i8) = intrinsics.sext_i8_i64 x

  let (x: i8) == (y: i8) = intrinsics.eq_i8 (x, y)
  let (x: i8) < (y: i8) = intrinsics.slt8 (x, y)
  let (x: i8) > (y: i8) = intrinsics.slt8 (y, x)
  let (x: i8) <= (y: i8) = intrinsics.sle8 (x, y)
  let (x: i8) >= (y: i8) = intrinsics.sle8 (y, x)
  let (x: i8) != (y: i8) = !(x == y)

  let sgn (x: i8) = intrinsics.ssignum8 x
  let abs (x: i8) = intrinsics.abs8 x

  let neg (x: t) = -x
  let max (x: t) (y: t) = intrinsics.smax8 (x, y)
  let min (x: t) (y: t) = intrinsics.smin8 (x, y)

  let highest = 127i8
  let lowest = highest + 1i8

  let num_bits = 8i32
  let get_bit (bit: i32) (x: t) = to_i32 ((x >> i32 bit) & i32 1)
  let set_bit (bit: i32) (x: t) (b: i32) =
    ((x & i32 (!(1 intrinsics.<< bit))) | i32 (b intrinsics.<< bit))
  let popc = intrinsics.popc8
  let mul_hi a b = intrinsics.mul_hi8 (i8 a, i8 b)
  let mad_hi a b c = intrinsics.mad_hi8 (i8 a, i8 b, i8 c)
  let clz = intrinsics.clz8
  let ctz = intrinsics.ctz8

  let sum = reduce (+) (i32 0)
  let product = reduce (*) (i32 1)
  let maximum = reduce max lowest
  let minimum = reduce min highest
}

module i16: (integral with t = i16) = {
  type t = i16

  let (x: i16) + (y: i16) = intrinsics.add16 (x, y)
  let (x: i16) - (y: i16) = intrinsics.sub16 (x, y)
  let (x: i16) * (y: i16) = intrinsics.mul16 (x, y)
  let (x: i16) / (y: i16) = intrinsics.sdiv16 (x, y)
  let (x: i16) ** (y: i16) = intrinsics.pow16 (x, y)
  let (x: i16) % (y: i16) = intrinsics.smod16 (x, y)
  let (x: i16) // (y: i16) = intrinsics.squot16 (x, y)
  let (x: i16) %% (y: i16) = intrinsics.srem16 (x, y)

  let (x: i16) & (y: i16) = intrinsics.and16 (x, y)
  let (x: i16) | (y: i16) = intrinsics.or16 (x, y)
  let (x: i16) ^ (y: i16) = intrinsics.xor16 (x, y)
  let not (x: i16) = intrinsics.complement16 x

  let (x: i16) << (y: i16) = intrinsics.shl16 (x, y)
  let (x: i16) >> (y: i16) = intrinsics.ashr16 (x, y)
  let (x: i16) >>> (y: i16) = intrinsics.lshr16 (x, y)

  let i8  (x: i8)  = intrinsics.sext_i8_i16 x
  let i16 (x: i16) = intrinsics.sext_i16_i16 x
  let i32 (x: i32) = intrinsics.sext_i32_i16 x
  let i64 (x: i64) = intrinsics.sext_i64_i16 x

  let u8  (x: u8)  = intrinsics.zext_i8_i16 (intrinsics.sign_i8 x)
  let u16 (x: u16) = intrinsics.zext_i16_i16 (intrinsics.sign_i16 x)
  let u32 (x: u32) = intrinsics.zext_i32_i16 (intrinsics.sign_i32 x)
  let u64 (x: u64) = intrinsics.zext_i64_i16 (intrinsics.sign_i64 x)

  let f16 (x: f16) = intrinsics.fptosi_f16_i16 x
  let f32 (x: f32) = intrinsics.fptosi_f32_i16 x
  let f64 (x: f64) = intrinsics.fptosi_f64_i16 x

  let bool = intrinsics.btoi_bool_i16

  let to_i32(x: i16) = intrinsics.sext_i16_i32 x
  let to_i64(x: i16) = intrinsics.sext_i16_i64 x

  let (x: i16) == (y: i16) = intrinsics.eq_i16 (x, y)
  let (x: i16) < (y: i16) = intrinsics.slt16 (x, y)
  let (x: i16) > (y: i16) = intrinsics.slt16 (y, x)
  let (x: i16) <= (y: i16) = intrinsics.sle16 (x, y)
  let (x: i16) >= (y: i16) = intrinsics.sle16 (y, x)
  let (x: i16) != (y: i16) = !(x == y)

  let sgn (x: i16) = intrinsics.ssignum16 x
  let abs (x: i16) = intrinsics.abs16 x

  let neg (x: t) = -x
  let max (x: t) (y: t) = intrinsics.smax16 (x, y)
  let min (x: t) (y: t) = intrinsics.smin16 (x, y)

  let highest = 32767i16
  let lowest = highest + 1i16

  let num_bits = 16i32
  let get_bit (bit: i32) (x: t) = to_i32 ((x >> i32 bit) & i32 1)
  let set_bit (bit: i32) (x: t) (b: i32) =
    ((x & i32 (!(1 intrinsics.<< bit))) | i32 (b intrinsics.<< bit))
  let popc = intrinsics.popc16
  let mul_hi a b = intrinsics.mul_hi16 (i16 a, i16 b)
  let mad_hi a b c = intrinsics.mad_hi16 (i16 a, i16 b, i16 c)
  let clz = intrinsics.clz16
  let ctz = intrinsics.ctz16

  let sum = reduce (+) (i32 0)
  let product = reduce (*) (i32 1)
  let maximum = reduce max lowest
  let minimum = reduce min highest
}

module i32: (integral with t = i32) = {
  type t = i32

  let sign (x: u32) = intrinsics.sign_i32 x
  let unsign (x: i32) = intrinsics.unsign_i32 x

  let (x: i32) + (y: i32) = intrinsics.add32 (x, y)
  let (x: i32) - (y: i32) = intrinsics.sub32 (x, y)
  let (x: i32) * (y: i32) = intrinsics.mul32 (x, y)
  let (x: i32) / (y: i32) = intrinsics.sdiv32 (x, y)
  let (x: i32) ** (y: i32) = intrinsics.pow32 (x, y)
  let (x: i32) % (y: i32) = intrinsics.smod32 (x, y)
  let (x: i32) // (y: i32) = intrinsics.squot32 (x, y)
  let (x: i32) %% (y: i32) = intrinsics.srem32 (x, y)

  let (x: i32) & (y: i32) = intrinsics.and32 (x, y)
  let (x: i32) | (y: i32) = intrinsics.or32 (x, y)
  let (x: i32) ^ (y: i32) = intrinsics.xor32 (x, y)
  let not (x: i32) = intrinsics.complement32 x

  let (x: i32) << (y: i32) = intrinsics.shl32 (x, y)
  let (x: i32) >> (y: i32) = intrinsics.ashr32 (x, y)
  let (x: i32) >>> (y: i32) = intrinsics.lshr32 (x, y)

  let i8  (x: i8)  = intrinsics.sext_i8_i32 x
  let i16 (x: i16) = intrinsics.sext_i16_i32 x
  let i32 (x: i32) = intrinsics.sext_i32_i32 x
  let i64 (x: i64) = intrinsics.sext_i64_i32 x

  let u8  (x: u8)  = intrinsics.zext_i8_i32 (intrinsics.sign_i8 x)
  let u16 (x: u16) = intrinsics.zext_i16_i32 (intrinsics.sign_i16 x)
  let u32 (x: u32) = intrinsics.zext_i32_i32 (intrinsics.sign_i32 x)
  let u64 (x: u64) = intrinsics.zext_i64_i32 (intrinsics.sign_i64 x)

  let f16 (x: f16) = intrinsics.fptosi_f16_i32 x
  let f32 (x: f32) = intrinsics.fptosi_f32_i32 x
  let f64 (x: f64) = intrinsics.fptosi_f64_i32 x

  let bool = intrinsics.btoi_bool_i32

  let to_i32(x: i32) = intrinsics.sext_i32_i32 x
  let to_i64(x: i32) = intrinsics.sext_i32_i64 x

  let (x: i32) == (y: i32) = intrinsics.eq_i32 (x, y)
  let (x: i32) < (y: i32) = intrinsics.slt32 (x, y)
  let (x: i32) > (y: i32) = intrinsics.slt32 (y, x)
  let (x: i32) <= (y: i32) = intrinsics.sle32 (x, y)
  let (x: i32) >= (y: i32) = intrinsics.sle32 (y, x)
  let (x: i32) != (y: i32) = !(x == y)

  let sgn (x: i32) = intrinsics.ssignum32 x
  let abs (x: i32) = intrinsics.abs32 x

  let neg (x: t) = -x
  let max (x: t) (y: t) = intrinsics.smax32 (x, y)
  let min (x: t) (y: t) = intrinsics.smin32 (x, y)

  let highest = 2147483647i32
  let lowest = highest + 1

  let num_bits = 32i32
  let get_bit (bit: i32) (x: t) = to_i32 ((x >> i32 bit) & i32 1)
  let set_bit (bit: i32) (x: t) (b: i32) =
    ((x & i32 (!(1 intrinsics.<< bit))) | i32 (b intrinsics.<< bit))
  let popc = intrinsics.popc32
  let mul_hi a b = intrinsics.mul_hi32 (i32 a, i32 b)
  let mad_hi a b c = intrinsics.mad_hi32 (i32 a, i32 b, i32 c)
  let clz = intrinsics.clz32
  let ctz = intrinsics.ctz32

  let sum = reduce (+) (i32 0)
  let product = reduce (*) (i32 1)
  let maximum = reduce max lowest
  let minimum = reduce min highest
}

module i64: (integral with t = i64) = {
  type t = i64

  let sign (x: u64) = intrinsics.sign_i64 x
  let unsign (x: i64) = intrinsics.unsign_i64 x

  let (x: i64) + (y: i64) = intrinsics.add64 (x, y)
  let (x: i64) - (y: i64) = intrinsics.sub64 (x, y)
  let (x: i64) * (y: i64) = intrinsics.mul64 (x, y)
  let (x: i64) / (y: i64) = intrinsics.sdiv64 (x, y)
  let (x: i64) ** (y: i64) = intrinsics.pow64 (x, y)
  let (x: i64) % (y: i64) = intrinsics.smod64 (x, y)
  let (x: i64) // (y: i64) = intrinsics.squot64 (x, y)
  let (x: i64) %% (y: i64) = intrinsics.srem64 (x, y)

  let (x: i64) & (y: i64) = intrinsics.and64 (x, y)
  let (x: i64) | (y: i64) = intrinsics.or64 (x, y)
  let (x: i64) ^ (y: i64) = intrinsics.xor64 (x, y)
  let not (x: i64) = intrinsics.complement64 x

  let (x: i64) << (y: i64) = intrinsics.shl64 (x, y)
  let (x: i64) >> (y: i64) = intrinsics.ashr64 (x, y)
  let (x: i64) >>> (y: i64) = intrinsics.lshr64 (x, y)

  let i8  (x: i8)  = intrinsics.sext_i8_i64 x
  let i16 (x: i16) = intrinsics.sext_i16_i64 x
  let i32 (x: i32) = intrinsics.sext_i32_i64 x
  let i64 (x: i64) = intrinsics.sext_i64_i64 x

  let u8  (x: u8)  = intrinsics.zext_i8_i64 (intrinsics.sign_i8 x)
  let u16 (x: u16) = intrinsics.zext_i16_i64 (intrinsics.sign_i16 x)
  let u32 (x: u32) = intrinsics.zext_i32_i64 (intrinsics.sign_i32 x)
  let u64 (x: u64) = intrinsics.zext_i64_i64 (intrinsics.sign_i64 x)

  let f16 (x: f16) = intrinsics.fptosi_f16_i64 x
  let f32 (x: f32) = intrinsics.fptosi_f32_i64 x
  let f64 (x: f64) = intrinsics.fptosi_f64_i64 x

  let bool = intrinsics.btoi_bool_i64

  let to_i32(x: i64) = intrinsics.sext_i64_i32 x
  let to_i64(x: i64) = intrinsics.sext_i64_i64 x

  let (x: i64) == (y: i64) = intrinsics.eq_i64 (x, y)
  let (x: i64) < (y: i64) = intrinsics.slt64 (x, y)
  let (x: i64) > (y: i64) = intrinsics.slt64 (y, x)
  let (x: i64) <= (y: i64) = intrinsics.sle64 (x, y)
  let (x: i64) >= (y: i64) = intrinsics.sle64 (y, x)
  let (x: i64) != (y: i64) = !(x == y)

  let sgn (x: i64) = intrinsics.ssignum64 x
  let abs (x: i64) = intrinsics.abs64 x

  let neg (x: t) = -x
  let max (x: t) (y: t) = intrinsics.smax64 (x, y)
  let min (x: t) (y: t) = intrinsics.smin64 (x, y)

  let highest = 9223372036854775807i64
  let lowest = highest + 1i64

  let num_bits = 64i32
  let get_bit (bit: i32) (x: t) = to_i32 ((x >> i32 bit) & i32 1)
  let set_bit (bit: i32) (x: t) (b: i32) =
    ((x & i32 (!(1 intrinsics.<< bit))) | intrinsics.zext_i32_i64 (b intrinsics.<< bit))
  let popc = intrinsics.popc64
  let mul_hi a b = intrinsics.mul_hi64 (i64 a, i64 b)
  let mad_hi a b c = intrinsics.mad_hi64 (i64 a, i64 b, i64 c)
  let clz = intrinsics.clz64
  let ctz = intrinsics.ctz64

  let sum = reduce (+) (i32 0)
  let product = reduce (*) (i32 1)
  let maximum = reduce max lowest
  let minimum = reduce min highest
}

module u8: (integral with t = u8) = {
  type t = u8

  let sign (x: u8) = intrinsics.sign_i8 x
  let unsign (x: i8) = intrinsics.unsign_i8 x

  let (x: u8) + (y: u8) = unsign (intrinsics.add8 (sign x, sign y))
  let (x: u8) - (y: u8) = unsign (intrinsics.sub8 (sign x, sign y))
  let (x: u8) * (y: u8) = unsign (intrinsics.mul8 (sign x, sign y))
  let (x: u8) / (y: u8) = unsign (intrinsics.udiv8 (sign x, sign y))
  let (x: u8) ** (y: u8) = unsign (intrinsics.pow8 (sign x, sign y))
  let (x: u8) % (y: u8) = unsign (intrinsics.umod8 (sign x, sign y))
  let (x: u8) // (y: u8) = unsign (intrinsics.udiv8 (sign x, sign y))
  let (x: u8) %% (y: u8) = unsign (intrinsics.umod8 (sign x, sign y))

  let (x: u8) & (y: u8) = unsign (intrinsics.and8 (sign x, sign y))
  let (x: u8) | (y: u8) = unsign (intrinsics.or8 (sign x, sign y))
  let (x: u8) ^ (y: u8) = unsign (intrinsics.xor8 (sign x, sign y))
  let not (x: u8) = unsign (intrinsics.complement8 (sign x))

  let (x: u8) << (y: u8) = unsign (intrinsics.shl8 (sign x, sign y))
  let (x: u8) >> (y: u8) = unsign (intrinsics.ashr8 (sign x, sign y))
  let (x: u8) >>> (y: u8) = unsign (intrinsics.lshr8 (sign x, sign y))

  let u8  (x: u8)  = unsign (i8.u8 x)
  let u16 (x: u16) = unsign (i8.u16 x)
  let u32 (x: u32) = unsign (i8.u32 x)
  let u64 (x: u64) = unsign (i8.u64 x)

  let i8  (x: i8)  = unsign (intrinsics.zext_i8_i8 x)
  let i16 (x: i16) = unsign (intrinsics.zext_i16_i8 x)
  let i32 (x: i32) = unsign (intrinsics.zext_i32_i8 x)
  let i64 (x: i64) = unsign (intrinsics.zext_i64_i8 x)

  let f16 (x: f16) = unsign (intrinsics.fptoui_f16_i8 x)
  let f32 (x: f32) = unsign (intrinsics.fptoui_f32_i8 x)
  let f64 (x: f64) = unsign (intrinsics.fptoui_f64_i8 x)

  let bool x = unsign (intrinsics.btoi_bool_i8 x)

  let to_i32(x: u8) = intrinsics.zext_i8_i32 (sign x)
  let to_i64(x: u8) = intrinsics.zext_i8_i64 (sign x)

  let (x: u8) == (y: u8) = intrinsics.eq_i8 (sign x, sign y)
  let (x: u8) < (y: u8) = intrinsics.ult8 (sign x, sign y)
  let (x: u8) > (y: u8) = intrinsics.ult8 (sign y, sign x)
  let (x: u8) <= (y: u8) = intrinsics.ule8 (sign x, sign y)
  let (x: u8) >= (y: u8) = intrinsics.ule8 (sign y, sign x)
  let (x: u8) != (y: u8) = !(x == y)

  let sgn (x: u8) = unsign (intrinsics.usignum8 (sign x))
  let abs (x: u8) = x

  let neg (x: t) = -x
  let max (x: t) (y: t) = unsign (intrinsics.umax8 (sign x, sign y))
  let min (x: t) (y: t) = unsign (intrinsics.umin8 (sign x, sign y))

  let highest = 255u8
  let lowest = 0u8

  let num_bits = 8i32
  let get_bit (bit: i32) (x: t) = to_i32 ((x >> i32 bit) & i32 1)
  let set_bit (bit: i32) (x: t) (b: i32) =
    ((x & i32 (!(1 intrinsics.<< bit))) | i32 (b intrinsics.<< bit))
  let popc x = intrinsics.popc8 (sign x)
  let mul_hi a b = unsign (intrinsics.mul_hi8 (sign a, sign b))
  let mad_hi a b c = unsign (intrinsics.mad_hi8 (sign a, sign b, sign c))
  let clz x = intrinsics.clz8 (sign x)
  let ctz x = intrinsics.ctz8 (sign x)

  let sum = reduce (+) (i32 0)
  let product = reduce (*) (i32 1)
  let maximum = reduce max lowest
  let minimum = reduce min highest
}

module u16: (integral with t = u16) = {
  type t = u16

  let sign (x: u16) = intrinsics.sign_i16 x
  let unsign (x: i16) = intrinsics.unsign_i16 x

  let (x: u16) + (y: u16) = unsign (intrinsics.add16 (sign x, sign y))
  let (x: u16) - (y: u16) = unsign (intrinsics.sub16 (sign x, sign y))
  let (x: u16) * (y: u16) = unsign (intrinsics.mul16 (sign x, sign y))
  let (x: u16) / (y: u16) = unsign (intrinsics.udiv16 (sign x, sign y))
  let (x: u16) ** (y: u16) = unsign (intrinsics.pow16 (sign x, sign y))
  let (x: u16) % (y: u16) = unsign (intrinsics.umod16 (sign x, sign y))
  let (x: u16) // (y: u16) = unsign (intrinsics.udiv16 (sign x, sign y))
  let (x: u16) %% (y: u16) = unsign (intrinsics.umod16 (sign x, sign y))

  let (x: u16) & (y: u16) = unsign (intrinsics.and16 (sign x, sign y))
  let (x: u16) | (y: u16) = unsign (intrinsics.or16 (sign x, sign y))
  let (x: u16) ^ (y: u16) = unsign (intrinsics.xor16 (sign x, sign y))
  let not (x: u16) = unsign (intrinsics.complement16 (sign x))

  let (x: u16) << (y: u16) = unsign (intrinsics.shl16 (sign x, sign y))
  let (x: u16) >> (y: u16) = unsign (intrinsics.ashr16 (sign x, sign y))
  let (x: u16) >>> (y: u16) = unsign (intrinsics.lshr16 (sign x, sign y))

  let u8  (x: u8)  = unsign (i16.u8 x)
  let u16 (x: u16) = unsign (i16.u16 x)
  let u32 (x: u32) = unsign (i16.u32 x)
  let u64 (x: u64) = unsign (i16.u64 x)

  let i8  (x: i8)  = unsign (intrinsics.zext_i8_i16 x)
  let i16 (x: i16) = unsign (intrinsics.zext_i16_i16 x)
  let i32 (x: i32) = unsign (intrinsics.zext_i32_i16 x)
  let i64 (x: i64) = unsign (intrinsics.zext_i64_i16 x)

  let f16 (x: f16) = unsign (intrinsics.fptoui_f16_i16 x)
  let f32 (x: f32) = unsign (intrinsics.fptoui_f32_i16 x)
  let f64 (x: f64) = unsign (intrinsics.fptoui_f64_i16 x)

  let bool x = unsign (intrinsics.btoi_bool_i16 x)

  let to_i32(x: u16) = intrinsics.zext_i16_i32 (sign x)
  let to_i64(x: u16) = intrinsics.zext_i16_i64 (sign x)

  let (x: u16) == (y: u16) = intrinsics.eq_i16 (sign x, sign y)
  let (x: u16) < (y: u16) = intrinsics.ult16 (sign x, sign y)
  let (x: u16) > (y: u16) = intrinsics.ult16 (sign y, sign x)
  let (x: u16) <= (y: u16) = intrinsics.ule16 (sign x, sign y)
  let (x: u16) >= (y: u16) = intrinsics.ule16 (sign y, sign x)
  let (x: u16) != (y: u16) = !(x == y)

  let sgn (x: u16) = unsign (intrinsics.usignum16 (sign x))
  let abs (x: u16) = x

  let neg (x: t) = -x
  let max (x: t) (y: t) = unsign (intrinsics.umax16 (sign x, sign y))
  let min (x: t) (y: t) = unsign (intrinsics.umin16 (sign x, sign y))

  let highest = 65535u16
  let lowest = 0u16

  let num_bits = 16i32
  let get_bit (bit: i32) (x: t) = to_i32 ((x >> i32 bit) & i32 1)
  let set_bit (bit: i32) (x: t) (b: i32) =
    ((x & i32 (!(1 intrinsics.<< bit))) | i32 (b intrinsics.<< bit))
  let popc x = intrinsics.popc16 (sign x)
  let mul_hi a b = unsign (intrinsics.mul_hi16 (sign a, sign b))
  let mad_hi a b c = unsign (intrinsics.mad_hi16 (sign a, sign b, sign c))
  let clz x = intrinsics.clz16 (sign x)
  let ctz x = intrinsics.ctz16 (sign x)

  let sum = reduce (+) (i32 0)
  let product = reduce (*) (i32 1)
  let maximum = reduce max lowest
  let minimum = reduce min highest
}

module u32: (integral with t = u32) = {
  type t = u32

  let sign (x: u32) = intrinsics.sign_i32 x
  let unsign (x: i32) = intrinsics.unsign_i32 x

  let (x: u32) + (y: u32) = unsign (intrinsics.add32 (sign x, sign y))
  let (x: u32) - (y: u32) = unsign (intrinsics.sub32 (sign x, sign y))
  let (x: u32) * (y: u32) = unsign (intrinsics.mul32 (sign x, sign y))
  let (x: u32) / (y: u32) = unsign (intrinsics.udiv32 (sign x, sign y))
  let (x: u32) ** (y: u32) = unsign (intrinsics.pow32 (sign x, sign y))
  let (x: u32) % (y: u32) = unsign (intrinsics.umod32 (sign x, sign y))
  let (x: u32) // (y: u32) = unsign (intrinsics.udiv32 (sign x, sign y))
  let (x: u32) %% (y: u32) = unsign (intrinsics.umod32 (sign x, sign y))

  let (x: u32) & (y: u32) = unsign (intrinsics.and32 (sign x, sign y))
  let (x: u32) | (y: u32) = unsign (intrinsics.or32 (sign x, sign y))
  let (x: u32) ^ (y: u32) = unsign (intrinsics.xor32 (sign x, sign y))
  let not (x: u32) = unsign (intrinsics.complement32 (sign x))

  let (x: u32) << (y: u32) = unsign (intrinsics.shl32 (sign x, sign y))
  let (x: u32) >> (y: u32) = unsign (intrinsics.ashr32 (sign x, sign y))
  let (x: u32) >>> (y: u32) = unsign (intrinsics.lshr32 (sign x, sign y))

  let u8  (x: u8)  = unsign (i32.u8 x)
  let u16 (x: u16) = unsign (i32.u16 x)
  let u32 (x: u32) = unsign (i32.u32 x)
  let u64 (x: u64) = unsign (i32.u64 x)

  let i8  (x: i8)  = unsign (intrinsics.zext_i8_i32 x)
  let i16 (x: i16) = unsign (intrinsics.zext_i16_i32 x)
  let i32 (x: i32) = unsign (intrinsics.zext_i32_i32 x)
  let i64 (x: i64) = unsign (intrinsics.zext_i64_i32 x)

  let f16 (x: f16) = unsign (intrinsics.fptoui_f16_i32 x)
  let f32 (x: f32) = unsign (intrinsics.fptoui_f32_i32 x)
  let f64 (x: f64) = unsign (intrinsics.fptoui_f64_i32 x)

  let bool x = unsign (intrinsics.btoi_bool_i32 x)

  let to_i32(x: u32) = intrinsics.zext_i32_i32 (sign x)
  let to_i64(x: u32) = intrinsics.zext_i32_i64 (sign x)

  let (x: u32) == (y: u32) = intrinsics.eq_i32 (sign x, sign y)
  let (x: u32) < (y: u32) = intrinsics.ult32 (sign x, sign y)
  let (x: u32) > (y: u32) = intrinsics.ult32 (sign y, sign x)
  let (x: u32) <= (y: u32) = intrinsics.ule32 (sign x, sign y)
  let (x: u32) >= (y: u32) = intrinsics.ule32 (sign y, sign x)
  let (x: u32) != (y: u32) = !(x == y)

  let sgn (x: u32) = unsign (intrinsics.usignum32 (sign x))
  let abs (x: u32) = x

  let highest = 4294967295u32
  let lowest = highest + 1u32

  let neg (x: t) = -x
  let max (x: t) (y: t) = unsign (intrinsics.umax32 (sign x, sign y))
  let min (x: t) (y: t) = unsign (intrinsics.umin32 (sign x, sign y))

  let num_bits = 32i32
  let get_bit (bit: i32) (x: t) = to_i32 ((x >> i32 bit) & i32 1)
  let set_bit (bit: i32) (x: t) (b: i32) =
    ((x & i32 (!(1 intrinsics.<< bit))) | i32 (b intrinsics.<< bit))
  let popc x = intrinsics.popc32 (sign x)
  let mul_hi a b = unsign (intrinsics.mul_hi32 (sign a, sign b))
  let mad_hi a b c = unsign (intrinsics.mad_hi32 (sign a, sign b, sign c))
  let clz x = intrinsics.clz32 (sign x)
  let ctz x = intrinsics.ctz32 (sign x)

  let sum = reduce (+) (i32 0)
  let product = reduce (*) (i32 1)
  let maximum = reduce max lowest
  let minimum = reduce min highest
}

module u64: (integral with t = u64) = {
  type t = u64

  let sign (x: u64) = intrinsics.sign_i64 x
  let unsign (x: i64) = intrinsics.unsign_i64 x

  let (x: u64) + (y: u64) = unsign (intrinsics.add64 (sign x, sign y))
  let (x: u64) - (y: u64) = unsign (intrinsics.sub64 (sign x, sign y))
  let (x: u64) * (y: u64) = unsign (intrinsics.mul64 (sign x, sign y))
  let (x: u64) / (y: u64) = unsign (intrinsics.udiv64 (sign x, sign y))
  let (x: u64) ** (y: u64) = unsign (intrinsics.pow64 (sign x, sign y))
  let (x: u64) % (y: u64) = unsign (intrinsics.umod64 (sign x, sign y))
  let (x: u64) // (y: u64) = unsign (intrinsics.udiv64 (sign x, sign y))
  let (x: u64) %% (y: u64) = unsign (intrinsics.umod64 (sign x, sign y))

  let (x: u64) & (y: u64) = unsign (intrinsics.and64 (sign x, sign y))
  let (x: u64) | (y: u64) = unsign (intrinsics.or64 (sign x, sign y))
  let (x: u64) ^ (y: u64) = unsign (intrinsics.xor64 (sign x, sign y))
  let not (x: u64) = unsign (intrinsics.complement64 (sign x))

  let (x: u64) << (y: u64) = unsign (intrinsics.shl64 (sign x, sign y))
  let (x: u64) >> (y: u64) = unsign (intrinsics.ashr64 (sign x, sign y))
  let (x: u64) >>> (y: u64) = unsign (intrinsics.lshr64 (sign x, sign y))

  let u8  (x: u8)  = unsign (i64.u8 x)
  let u16 (x: u16) = unsign (i64.u16 x)
  let u32 (x: u32) = unsign (i64.u32 x)
  let u64 (x: u64) = unsign (i64.u64 x)

  let i8 (x: i8)   = unsign (intrinsics.zext_i8_i64 x)
  let i16 (x: i16) = unsign (intrinsics.zext_i16_i64 x)
  let i32 (x: i32) = unsign (intrinsics.zext_i32_i64 x)
  let i64 (x: i64) = unsign (intrinsics.zext_i64_i64 x)

  let f16 (x: f16) = unsign (intrinsics.fptoui_f16_i64 x)
  let f32 (x: f32) = unsign (intrinsics.fptoui_f32_i64 x)
  let f64 (x: f64) = unsign (intrinsics.fptoui_f64_i64 x)

  let bool x = unsign (intrinsics.btoi_bool_i64 x)

  let to_i32(x: u64) = intrinsics.zext_i64_i32 (sign x)
  let to_i64(x: u64) = intrinsics.zext_i64_i64 (sign x)

  let (x: u64) == (y: u64) = intrinsics.eq_i64 (sign x, sign y)
  let (x: u64) < (y: u64) = intrinsics.ult64 (sign x, sign y)
  let (x: u64) > (y: u64) = intrinsics.ult64 (sign y, sign x)
  let (x: u64) <= (y: u64) = intrinsics.ule64 (sign x, sign y)
  let (x: u64) >= (y: u64) = intrinsics.ule64 (sign y, sign x)
  let (x: u64) != (y: u64) = !(x == y)

  let sgn (x: u64) = unsign (intrinsics.usignum64 (sign x))
  let abs (x: u64) = x

  let neg (x: t) = -x
  let max (x: t) (y: t) = unsign (intrinsics.umax64 (sign x, sign y))
  let min (x: t) (y: t) = unsign (intrinsics.umin64 (sign x, sign y))

  let highest = 18446744073709551615u64
  let lowest = highest + 1u64

  let num_bits = 64i32
  let get_bit (bit: i32) (x: t) = to_i32 ((x >> i32 bit) & i32 1)
  let set_bit (bit: i32) (x: t) (b: i32) =
    ((x & i32 (!(1 intrinsics.<< bit))) | i32 (b intrinsics.<< bit))
  let popc x = intrinsics.popc64 (sign x)
  let mul_hi a b = unsign (intrinsics.mul_hi64 (sign a, sign b))
  let mad_hi a b c = unsign (intrinsics.mad_hi64 (sign a, sign b, sign c))
  let clz x = intrinsics.clz64 (sign x)
  let ctz x = intrinsics.ctz64 (sign x)

  let sum = reduce (+) (i32 0)
  let product = reduce (*) (i32 1)
  let maximum = reduce max lowest
  let minimum = reduce min highest
}

module f64: (float with t = f64 with int_t = u64) = {
  type t = f64
  type int_t = u64

  module i64m = i64
  module u64m = u64

  let (x: f64) + (y: f64) = intrinsics.fadd64 (x, y)
  let (x: f64) - (y: f64) = intrinsics.fsub64 (x, y)
  let (x: f64) * (y: f64) = intrinsics.fmul64 (x, y)
  let (x: f64) / (y: f64) = intrinsics.fdiv64 (x, y)
  let (x: f64) % (y: f64) = intrinsics.fmod64 (x, y)
  let (x: f64) ** (y: f64) = intrinsics.fpow64 (x, y)

  let u8  (x: u8)  = intrinsics.uitofp_i8_f64  (i8.u8 x)
  let u16 (x: u16) = intrinsics.uitofp_i16_f64 (i16.u16 x)
  let u32 (x: u32) = intrinsics.uitofp_i32_f64 (i32.u32 x)
  let u64 (x: u64) = intrinsics.uitofp_i64_f64 (i64.u64 x)

  let i8 (x: i8) = intrinsics.sitofp_i8_f64 x
  let i16 (x: i16) = intrinsics.sitofp_i16_f64 x
  let i32 (x: i32) = intrinsics.sitofp_i32_f64 x
  let i64 (x: i64) = intrinsics.sitofp_i64_f64 x

  let f16 (x: f16) = intrinsics.fpconv_f16_f64 x
  let f32 (x: f32) = intrinsics.fpconv_f32_f64 x
  let f64 (x: f64) = intrinsics.fpconv_f64_f64 x

  let bool (x: bool) = if x then 1f64 else 0f64

  let from_fraction (x: i64) (y: i64) = i64 x / i64 y
  let to_i64 (x: f64) = intrinsics.fptosi_f64_i64 x
  let to_f64 (x: f64) = x

  let (x: f64) == (y: f64) = intrinsics.eq_f64 (x, y)
  let (x: f64) < (y: f64) = intrinsics.lt64 (x, y)
  let (x: f64) > (y: f64) = intrinsics.lt64 (y, x)
  let (x: f64) <= (y: f64) = intrinsics.le64 (x, y)
  let (x: f64) >= (y: f64) = intrinsics.le64 (y, x)
  let (x: f64) != (y: f64) = !(x == y)

  let neg (x: t) = -x
  let recip (x: t) = 1/x
  let max (x: t) (y: t) = intrinsics.fmax64 (x, y)
  let min (x: t) (y: t) = intrinsics.fmin64 (x, y)

  let sgn (x: f64) = intrinsics.fsignum64 x
  let abs (x: f64) = intrinsics.fabs64 x

  let sqrt (x: f64) = intrinsics.sqrt64 x

  let log (x: f64) = intrinsics.log64 x
  let log2 (x: f64) = intrinsics.log2_64 x
  let log10 (x: f64) = intrinsics.log10_64 x
  let exp (x: f64) = intrinsics.exp64 x
  let sin (x: f64) = intrinsics.sin64 x
  let cos (x: f64) = intrinsics.cos64 x
  let tan (x: f64) = intrinsics.tan64 x
  let acos (x: f64) = intrinsics.acos64 x
  let asin (x: f64) = intrinsics.asin64 x
  let atan (x: f64) = intrinsics.atan64 x
  let sinh (x: f64) = intrinsics.sinh64 x
  let cosh (x: f64) = intrinsics.cosh64 x
  let tanh (x: f64) = intrinsics.tanh64 x
  let acosh (x: f64) = intrinsics.acosh64 x
  let asinh (x: f64) = intrinsics.asinh64 x
  let atanh (x: f64) = intrinsics.atanh64 x
  let atan2 (x: f64) (y: f64) = intrinsics.atan2_64 (x, y)
  let hypot (x: f64) (y: f64) = intrinsics.hypot64 (x, y)
  let gamma = intrinsics.gamma64
  let lgamma = intrinsics.lgamma64

  let lerp v0 v1 t = intrinsics.lerp64 (v0,v1,t)
  let fma a b c = intrinsics.fma64 (a,b,c)
  let mad a b c = intrinsics.mad64 (a,b,c)

  let ceil = intrinsics.ceil64
  let floor = intrinsics.floor64
  let trunc (x: f64) : f64 = i64 (i64m.f64 x)

  let round = intrinsics.round64

  let to_bits (x: f64): u64 = u64m.i64 (intrinsics.to_bits64 x)
  let from_bits (x: u64): f64 = intrinsics.from_bits64 (intrinsics.sign_i64 x)

  let num_bits = 64i32
  let get_bit (bit: i32) (x: t) = u64m.get_bit bit (to_bits x)
  let set_bit (bit: i32) (x: t) (b: i32) = from_bits (u64m.set_bit bit (to_bits x) b)

  let isinf (x: f64) = intrinsics.isinf64 x
  let isnan (x: f64) = intrinsics.isnan64 x

  let inf = 1f64 / 0f64
  let nan = 0f64 / 0f64

  let highest = inf
  let lowest = -inf
  let epsilon = 2.220446049250313e-16f64

  let pi = 3.1415926535897932384626433832795028841971693993751058209749445923078164062f64
  let e = 2.718281828459045235360287471352662497757247093699959574966967627724076630353f64

  let sum = reduce (+) (i32 0)
  let product = reduce (*) (i32 1)
  let maximum = reduce max lowest
  let minimum = reduce min highest
}

module f32: (float with t = f32 with int_t = u32) = {
  type t = f32
  type int_t = u32

  module i32m = i32
  module u32m = u32
  module f64m = f64

  let (x: f32) + (y: f32) = intrinsics.fadd32 (x, y)
  let (x: f32) - (y: f32) = intrinsics.fsub32 (x, y)
  let (x: f32) * (y: f32) = intrinsics.fmul32 (x, y)
  let (x: f32) / (y: f32) = intrinsics.fdiv32 (x, y)
  let (x: f32) % (y: f32) = intrinsics.fmod32 (x, y)
  let (x: f32) ** (y: f32) = intrinsics.fpow32 (x, y)

  let u8  (x: u8)  = intrinsics.uitofp_i8_f32  (i8.u8 x)
  let u16 (x: u16) = intrinsics.uitofp_i16_f32 (i16.u16 x)
  let u32 (x: u32) = intrinsics.uitofp_i32_f32 (i32.u32 x)
  let u64 (x: u64) = intrinsics.uitofp_i64_f32 (i64.u64 x)

  let i8 (x: i8) = intrinsics.sitofp_i8_f32 x
  let i16 (x: i16) = intrinsics.sitofp_i16_f32 x
  let i32 (x: i32) = intrinsics.sitofp_i32_f32 x
  let i64 (x: i64) = intrinsics.sitofp_i64_f32 x

  let f16 (x: f16) = intrinsics.fpconv_f16_f32 x
  let f32 (x: f32) = intrinsics.fpconv_f32_f32 x
  let f64 (x: f64) = intrinsics.fpconv_f64_f32 x

  let bool (x: bool) = if x then 1f32 else 0f32

  let from_fraction (x: i64) (y: i64) = i64 x / i64 y
  let to_i64 (x: f32) = intrinsics.fptosi_f32_i64 x
  let to_f64 (x: f32) = intrinsics.fpconv_f32_f64 x

  let (x: f32) == (y: f32) = intrinsics.eq_f32 (x, y)
  let (x: f32) < (y: f32) = intrinsics.lt32 (x, y)
  let (x: f32) > (y: f32) = intrinsics.lt32 (y, x)
  let (x: f32) <= (y: f32) = intrinsics.le32 (x, y)
  let (x: f32) >= (y: f32) = intrinsics.le32 (y, x)
  let (x: f32) != (y: f32) = !(x == y)

  let neg (x: t) = -x
  let recip (x: t) = 1/x
  let max (x: t) (y: t) = intrinsics.fmax32 (x, y)
  let min (x: t) (y: t) = intrinsics.fmin32 (x, y)

  let sgn (x: f32) = intrinsics.fsignum32 x
  let abs (x: f32) = intrinsics.fabs32 x

  let sqrt (x: f32) = intrinsics.sqrt32 x

  let log (x: f32) = intrinsics.log32 x
  let log2 (x: f32) = intrinsics.log2_32 x
  let log10 (x: f32) = intrinsics.log10_32 x
  let exp (x: f32) = intrinsics.exp32 x
  let sin (x: f32) = intrinsics.sin32 x
  let cos (x: f32) = intrinsics.cos32 x
  let tan (x: f32) = intrinsics.tan32 x
  let acos (x: f32) = intrinsics.acos32 x
  let asin (x: f32) = intrinsics.asin32 x
  let atan (x: f32) = intrinsics.atan32 x
  let sinh (x: f32) = intrinsics.sinh32 x
  let cosh (x: f32) = intrinsics.cosh32 x
  let tanh (x: f32) = intrinsics.tanh32 x
  let acosh (x: f32) = intrinsics.acosh32 x
  let asinh (x: f32) = intrinsics.asinh32 x
  let atanh (x: f32) = intrinsics.atanh32 x
  let atan2 (x: f32) (y: f32) = intrinsics.atan2_32 (x, y)
  let hypot (x: f32) (y: f32) = intrinsics.hypot32 (x, y)
  let gamma = intrinsics.gamma32
  let lgamma = intrinsics.lgamma32

  let lerp v0 v1 t = intrinsics.lerp32 (v0,v1,t)
  let fma a b c = intrinsics.fma32 (a,b,c)
  let mad a b c = intrinsics.mad32 (a,b,c)

  let ceil = intrinsics.ceil32
  let floor = intrinsics.floor32
  let trunc (x: f32) : f32 = i32 (i32m.f32 x)

  let round = intrinsics.round32

  let to_bits (x: f32): u32 = u32m.i32 (intrinsics.to_bits32 x)
  let from_bits (x: u32): f32 = intrinsics.from_bits32 (intrinsics.sign_i32 x)

  let num_bits = 32i32
  let get_bit (bit: i32) (x: t) = u32m.get_bit bit (to_bits x)
  let set_bit (bit: i32) (x: t) (b: i32) = from_bits (u32m.set_bit bit (to_bits x) b)

  let isinf (x: f32) = intrinsics.isinf32 x
  let isnan (x: f32) = intrinsics.isnan32 x

  let inf = 1f32 / 0f32
  let nan = 0f32 / 0f32

  let highest = inf
  let lowest = -inf
  let epsilon = 1.1920929e-7f32

  let pi = f64 f64m.pi
  let e = f64 f64m.e

  let sum = reduce (+) (i32 0)
  let product = reduce (*) (i32 1)
  let maximum = reduce max lowest
  let minimum = reduce min highest
}

-- | Emulated with single precision on systems that do not natively
-- support half precision.  This means you might get more accurate
-- results than on real systems, but it is also likely to be
-- significantly slower than just using `f32` in the first place.
module f16: (float with t = f16 with int_t = u16) = {
  type t = f16
  type int_t = u16

  module i16m = i16
  module u16m = u16
  module f64m = f64

  let (x: f16) + (y: f16) = intrinsics.fadd16 (x, y)
  let (x: f16) - (y: f16) = intrinsics.fsub16 (x, y)
  let (x: f16) * (y: f16) = intrinsics.fmul16 (x, y)
  let (x: f16) / (y: f16) = intrinsics.fdiv16 (x, y)
  let (x: f16) % (y: f16) = intrinsics.fmod16 (x, y)
  let (x: f16) ** (y: f16) = intrinsics.fpow16 (x, y)

  let u8  (x: u8)  = intrinsics.uitofp_i8_f16  (i8.u8 x)
  let u16 (x: u16) = intrinsics.uitofp_i16_f16 (i16.u16 x)
  let u32 (x: u32) = intrinsics.uitofp_i32_f16 (i32.u32 x)
  let u64 (x: u64) = intrinsics.uitofp_i64_f16 (i64.u64 x)

  let i8 (x: i8) = intrinsics.sitofp_i8_f16 x
  let i16 (x: i16) = intrinsics.sitofp_i16_f16 x
  let i32 (x: i32) = intrinsics.sitofp_i32_f16 x
  let i64 (x: i64) = intrinsics.sitofp_i64_f16 x

  let f16 (x: f16) = intrinsics.fpconv_f16_f16 x
  let f32 (x: f32) = intrinsics.fpconv_f32_f16 x
  let f64 (x: f64) = intrinsics.fpconv_f64_f16 x

  let bool (x: bool) = if x then 1f16 else 0f16

  let from_fraction (x: i64) (y: i64) = i64 x / i64 y
  let to_i64 (x: f16) = intrinsics.fptosi_f16_i64 x
  let to_f64 (x: f16) = intrinsics.fpconv_f16_f64 x

  let (x: f16) == (y: f16) = intrinsics.eq_f16 (x, y)
  let (x: f16) < (y: f16) = intrinsics.lt16 (x, y)
  let (x: f16) > (y: f16) = intrinsics.lt16 (y, x)
  let (x: f16) <= (y: f16) = intrinsics.le16 (x, y)
  let (x: f16) >= (y: f16) = intrinsics.le16 (y, x)
  let (x: f16) != (y: f16) = !(x == y)

  let neg (x: t) = -x
  let recip (x: t) = 1/x
  let max (x: t) (y: t) = intrinsics.fmax16 (x, y)
  let min (x: t) (y: t) = intrinsics.fmin16 (x, y)

  let sgn (x: f16) = intrinsics.fsignum16 x
  let abs (x: f16) = intrinsics.fabs16 x

  let sqrt (x: f16) = intrinsics.sqrt16 x

  let log (x: f16) = intrinsics.log16 x
  let log2 (x: f16) = intrinsics.log2_16 x
  let log10 (x: f16) = intrinsics.log10_16 x
  let exp (x: f16) = intrinsics.exp16 x
  let sin (x: f16) = intrinsics.sin16 x
  let cos (x: f16) = intrinsics.cos16 x
  let tan (x: f16) = intrinsics.tan16 x
  let acos (x: f16) = intrinsics.acos16 x
  let asin (x: f16) = intrinsics.asin16 x
  let atan (x: f16) = intrinsics.atan16 x
  let sinh (x: f16) = intrinsics.sinh16 x
  let cosh (x: f16) = intrinsics.cosh16 x
  let tanh (x: f16) = intrinsics.tanh16 x
  let acosh (x: f16) = intrinsics.acosh16 x
  let asinh (x: f16) = intrinsics.asinh16 x
  let atanh (x: f16) = intrinsics.atanh16 x
  let atan2 (x: f16) (y: f16) = intrinsics.atan2_16 (x, y)
  let hypot (x: f16) (y: f16) = intrinsics.hypot16 (x, y)
  let gamma = intrinsics.gamma16
  let lgamma = intrinsics.lgamma16

  let lerp v0 v1 t = intrinsics.lerp16 (v0,v1,t)
  let fma a b c = intrinsics.fma16 (a,b,c)
  let mad a b c = intrinsics.mad16 (a,b,c)

  let ceil = intrinsics.ceil16
  let floor = intrinsics.floor16
  let trunc (x: f16) : f16 = i16 (i16m.f16 x)

  let round = intrinsics.round16

  let to_bits (x: f16): u16 = u16m.i16 (intrinsics.to_bits16 x)
  let from_bits (x: u16): f16 = intrinsics.from_bits16 (intrinsics.sign_i16 x)

  let num_bits = 16i32
  let get_bit (bit: i32) (x: t) = u16m.get_bit bit (to_bits x)
  let set_bit (bit: i32) (x: t) (b: i32) = from_bits (u16m.set_bit bit (to_bits x) b)

  let isinf (x: f16) = intrinsics.isinf16 x
  let isnan (x: f16) = intrinsics.isnan16 x

  let inf = 1f16 / 0f16
  let nan = 0f16 / 0f16

  let highest = inf
  let lowest = -inf
  let epsilon = 1.1920929e-7f16

  let pi = f64 f64m.pi
  let e = f64 f64m.e

  let sum = reduce (+) (i32 0)
  let product = reduce (*) (i32 1)
  let maximum = reduce max lowest
  let minimum = reduce min highest
}
