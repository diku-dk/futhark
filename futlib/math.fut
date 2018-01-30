-- | Basic mathematical modules and functions.

local let const 'a 'b (x: a) (_: b): a = x

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

  val f32: f32 -> t
  val f64: f64 -> t

  val bool: bool -> t
}

module type numeric = {
  include from_prim

  val +: t -> t -> t
  val -: t -> t -> t
  val *: t -> t -> t
  val /: t -> t -> t
  val **: t -> t -> t

  val to_i64: t -> i64

  val ==: t -> t -> bool
  val <: t -> t -> bool
  val >: t -> t -> bool
  val <=: t -> t -> bool
  val >=: t -> t -> bool
  val !=: t -> t -> bool

  val negate: t-> t
  val max: t -> t -> t
  val min: t -> t -> t

  val abs: t -> t

  val sgn: t -> t
}

module type integral = {
  include numeric

  val %: t -> t -> t
  val //: t -> t -> t
  val %%: t -> t -> t

  val &: t -> t -> t
  val |: t -> t -> t
  val ^: t -> t -> t
  val ~: t -> t

  val <<: t -> t -> t
  val >>: t -> t -> t
  val >>>: t -> t -> t

  val num_bits: i32
  val get_bit: i32 -> t -> i32
  val set_bit: i32 -> t -> i32 -> t
}

module type size = {
  include integral

  val iota: t -> *[]t
  val replicate 'v: t -> v -> *[]v
}

module type real = {
  include numeric

  val from_fraction: i32 -> i32 -> t
  val to_i32: t -> i32
  val to_i64: t -> i64
  val to_f64: t -> f64

  val sqrt: t -> t
  val exp: t -> t
  val cos: t -> t
  val sin: t -> t
  val asin: t -> t
  val acos: t -> t
  val atan: t -> t
  val atan2: t -> t -> t

  val log: t -> t

  val ceil : t -> t
  val floor : t -> t
  val trunc : t -> t
  val round : t -> t

  val isinf: t -> bool
  val isnan: t -> bool

  val inf: t
  val nan: t

  val pi: t
  val e: t
}

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
}

module bool: from_prim with t = bool = {
  type t = bool

  let i8  (x: i8)  = x != 0i8
  let i16 (x: i16) = x != 0i16
  let i32 (x: i32) = x != 0i32
  let i64 (x: i64) = x != 0i64

  let u8  (x: u8)  = x != 0u8
  let u16 (x: u16) = x != 0u16
  let u32 (x: u32) = x != 0u32
  let u64 (x: u64) = x != 0u64

  let f32 (x: f32) = x != 0f32
  let f64 (x: f64) = x != 0f64

  let bool (x: bool) = x
}

module i8: (size with t = i8) = {
  type t = i8

  let (x: i8) + (y: i8) = intrinsics.add8 x y
  let (x: i8) - (y: i8) = intrinsics.sub8 x y
  let (x: i8) * (y: i8) = intrinsics.mul8 x y
  let (x: i8) / (y: i8) = intrinsics.sdiv8 x y
  let (x: i8) ** (y: i8) = intrinsics.pow8 x y
  let (x: i8) % (y: i8) = intrinsics.smod8 x y
  let (x: i8) // (y: i8) = intrinsics.squot8 x y
  let (x: i8) %% (y: i8) = intrinsics.srem8 x y

  let (x: i8) & (y: i8) = intrinsics.and8 x y
  let (x: i8) | (y: i8) = intrinsics.or8 x y
  let (x: i8) ^ (y: i8) = intrinsics.xor8 x y
  let ~ (x: i8) = intrinsics.complement8 x

  let (x: i8) << (y: i8) = intrinsics.shl8 x y
  let (x: i8) >> (y: i8) = intrinsics.ashr8 x y
  let (x: i8) >>> (y: i8) = intrinsics.lshr8 x y

  let i8  (x: i8)  = intrinsics.sext_i8_i8 x
  let i16 (x: i16) = intrinsics.sext_i16_i8 x
  let i32 (x: i32) = intrinsics.sext_i32_i8 x
  let i64 (x: i64) = intrinsics.sext_i64_i8 x

  let u8  (x: u8)  = intrinsics.zext_i8_i8 (intrinsics.sign_i8 x)
  let u16 (x: u16) = intrinsics.zext_i16_i8 (intrinsics.sign_i16 x)
  let u32 (x: u32) = intrinsics.zext_i32_i8 (intrinsics.sign_i32 x)
  let u64 (x: u64) = intrinsics.zext_i64_i8 (intrinsics.sign_i64 x)

  let f32 (x: f32) = intrinsics.fptosi_f32_i8 x
  let f64 (x: f64) = intrinsics.fptosi_f64_i8 x

  let bool (x: bool) = if x then 1i8 else 0i8

  let to_i32(x: i8) = intrinsics.sext_i8_i32 x
  let to_i64(x: i8) = intrinsics.sext_i8_i64 x

  let (x: i8) == (y: i8) = intrinsics.eq_i8 x y
  let (x: i8) < (y: i8) = intrinsics.slt8 x y
  let (x: i8) > (y: i8) = intrinsics.slt8 y x
  let (x: i8) <= (y: i8) = intrinsics.sle8 x y
  let (x: i8) >= (y: i8) = intrinsics.sle8 y x
  let (x: i8) != (y: i8) = ! (x == y)

  let sgn (x: i8) = intrinsics.ssignum8 x
  let abs (x: i8) = intrinsics.abs8 x

  let negate (x: t) = -x
  let max (x: t) (y: t) = intrinsics.smax8 x y
  let min (x: t) (y: t) = intrinsics.smin8 x y

  let num_bits = 8
  let get_bit (bit: i32) (x: t) = to_i32 ((x >> i32 bit) & i32 1)
  let set_bit (bit: i32) (x: t) (b: i32) =
    ((x & i32 (intrinsics.~ (1 intrinsics.<< b))) | x intrinsics.<< i32 b)

  let iota (n: i8) = 0i8..1i8..<n
  let replicate 'v (n: i8) (x: v) = map (const x) (iota n)
}

module i16: (size with t = i16) = {
  type t = i16

  let (x: i16) + (y: i16) = intrinsics.add16 x y
  let (x: i16) - (y: i16) = intrinsics.sub16 x y
  let (x: i16) * (y: i16) = intrinsics.mul16 x y
  let (x: i16) / (y: i16) = intrinsics.sdiv16 x y
  let (x: i16) ** (y: i16) = intrinsics.pow16 x y
  let (x: i16) % (y: i16) = intrinsics.smod16 x y
  let (x: i16) // (y: i16) = intrinsics.squot16 x y
  let (x: i16) %% (y: i16) = intrinsics.srem16 x y

  let (x: i16) & (y: i16) = intrinsics.and16 x y
  let (x: i16) | (y: i16) = intrinsics.or16 x y
  let (x: i16) ^ (y: i16) = intrinsics.xor16 x y
  let ~ (x: i16) = intrinsics.complement16 x

  let (x: i16) << (y: i16) = intrinsics.shl16 x y
  let (x: i16) >> (y: i16) = intrinsics.ashr16 x y
  let (x: i16) >>> (y: i16) = intrinsics.lshr16 x y

  let i8  (x: i8)  = intrinsics.sext_i8_i16 x
  let i16 (x: i16) = intrinsics.sext_i16_i16 x
  let i32 (x: i32) = intrinsics.sext_i32_i16 x
  let i64 (x: i64) = intrinsics.sext_i64_i16 x

  let u8  (x: u8)  = intrinsics.zext_i8_i16 (intrinsics.sign_i8 x)
  let u16 (x: u16) = intrinsics.zext_i16_i16 (intrinsics.sign_i16 x)
  let u32 (x: u32) = intrinsics.zext_i32_i16 (intrinsics.sign_i32 x)
  let u64 (x: u64) = intrinsics.zext_i64_i16 (intrinsics.sign_i64 x)

  let f32 (x: f32) = intrinsics.fptosi_f32_i16 x
  let f64 (x: f64) = intrinsics.fptosi_f64_i16 x

  let bool (x: bool) = if x then 1i16 else 0i16

  let to_i32(x: i16) = intrinsics.sext_i16_i32 x
  let to_i64(x: i16) = intrinsics.sext_i16_i64 x

  let (x: i16) == (y: i16) = intrinsics.eq_i16 x y
  let (x: i16) < (y: i16) = intrinsics.slt16 x y
  let (x: i16) > (y: i16) = intrinsics.slt16 y x
  let (x: i16) <= (y: i16) = intrinsics.sle16 x y
  let (x: i16) >= (y: i16) = intrinsics.sle16 y x
  let (x: i16) != (y: i16) = ! (x == y)

  let sgn (x: i16) = intrinsics.ssignum16 x
  let abs (x: i16) = intrinsics.abs16 x

  let negate (x: t) = -x
  let max (x: t) (y: t) = intrinsics.smax16 x y
  let min (x: t) (y: t) = intrinsics.smin16 x y

  let num_bits = 8
  let get_bit (bit: i32) (x: t) = to_i32 ((x >> i32 bit) & i32 1)
  let set_bit (bit: i32) (x: t) (b: i32) =
    ((x & i32 (intrinsics.~(1 intrinsics.<< b))) | x intrinsics.<< i32 b)

  let iota (n: i16) = 0i16..1i16..<n
  let replicate 'v (n: i16) (x: v) = map (const x) (iota n)
}

module i32: (size with t = i32) = {
  type t = i32

  let sign (x: u32) = intrinsics.sign_i32 x
  let unsign (x: i32) = intrinsics.unsign_i32 x

  let (x: i32) + (y: i32) = intrinsics.add32 x y
  let (x: i32) - (y: i32) = intrinsics.sub32 x y
  let (x: i32) * (y: i32) = intrinsics.mul32 x y
  let (x: i32) / (y: i32) = intrinsics.sdiv32 x y
  let (x: i32) ** (y: i32) = intrinsics.pow32 x y
  let (x: i32) % (y: i32) = intrinsics.smod32 x y
  let (x: i32) // (y: i32) = intrinsics.squot32 x y
  let (x: i32) %% (y: i32) = intrinsics.srem32 x y

  let (x: i32) & (y: i32) = intrinsics.and32 x y
  let (x: i32) | (y: i32) = intrinsics.or32 x y
  let (x: i32) ^ (y: i32) = intrinsics.xor32 x y
  let ~ (x: i32) = intrinsics.complement32 x

  let (x: i32) << (y: i32) = intrinsics.shl32 x y
  let (x: i32) >> (y: i32) = intrinsics.ashr32 x y
  let (x: i32) >>> (y: i32) = intrinsics.lshr32 x y

  let i8  (x: i8)  = intrinsics.sext_i8_i32 x
  let i16 (x: i16) = intrinsics.sext_i16_i32 x
  let i32 (x: i32) = intrinsics.sext_i32_i32 x
  let i64 (x: i64) = intrinsics.sext_i64_i32 x

  let u8  (x: u8)  = intrinsics.zext_i8_i32 (intrinsics.sign_i8 x)
  let u16 (x: u16) = intrinsics.zext_i16_i32 (intrinsics.sign_i16 x)
  let u32 (x: u32) = intrinsics.zext_i32_i32 (intrinsics.sign_i32 x)
  let u64 (x: u64) = intrinsics.zext_i64_i32 (intrinsics.sign_i64 x)

  let f32 (x: f32) = intrinsics.fptosi_f32_i32 x
  let f64 (x: f64) = intrinsics.fptosi_f64_i32 x

  let bool (x: bool) = if x then 1i32 else 0i32

  let to_i32(x: i32) = intrinsics.sext_i32_i32 x
  let to_i64(x: i32) = intrinsics.sext_i32_i64 x

  let (x: i32) == (y: i32) = intrinsics.eq_i32 x y
  let (x: i32) < (y: i32) = intrinsics.slt32 x y
  let (x: i32) > (y: i32) = intrinsics.slt32 y x
  let (x: i32) <= (y: i32) = intrinsics.sle32 x y
  let (x: i32) >= (y: i32) = intrinsics.sle32 y x
  let (x: i32) != (y: i32) = ! (x == y)

  let sgn (x: i32) = intrinsics.ssignum32 x
  let abs (x: i32) = intrinsics.abs32 x

  let negate (x: t) = -x
  let max (x: t) (y: t) = intrinsics.smax32 x y
  let min (x: t) (y: t) = intrinsics.smin32 x y

  let num_bits = 8
  let get_bit (bit: i32) (x: t) = to_i32 ((x >> i32 bit) & i32 1)
  let set_bit (bit: i32) (x: t) (b: i32) =
    ((x & i32 (intrinsics.~(1 intrinsics.<< b))) | x intrinsics.<< i32 b)

  let iota (n: i32) = 0..1..<n
  let replicate 'v (n: i32) (x: v) = map (const x) (iota n)
}

module i64: (size with t = i64) = {
  type t = i64

  let sign (x: u64) = intrinsics.sign_i64 x
  let unsign (x: i64) = intrinsics.unsign_i64 x

  let (x: i64) + (y: i64) = intrinsics.add64 x y
  let (x: i64) - (y: i64) = intrinsics.sub64 x y
  let (x: i64) * (y: i64) = intrinsics.mul64 x y
  let (x: i64) / (y: i64) = intrinsics.sdiv64 x y
  let (x: i64) ** (y: i64) = intrinsics.pow64 x y
  let (x: i64) % (y: i64) = intrinsics.smod64 x y
  let (x: i64) // (y: i64) = intrinsics.squot64 x y
  let (x: i64) %% (y: i64) = intrinsics.srem64 x y

  let (x: i64) & (y: i64) = intrinsics.and64 x y
  let (x: i64) | (y: i64) = intrinsics.or64 x y
  let (x: i64) ^ (y: i64) = intrinsics.xor64 x y
  let ~ (x: i64) = intrinsics.complement64 x

  let (x: i64) << (y: i64) = intrinsics.shl64 x y
  let (x: i64) >> (y: i64) = intrinsics.ashr64 x y
  let (x: i64) >>> (y: i64) = intrinsics.lshr64 x y

  let i8  (x: i8)  = intrinsics.sext_i8_i64 x
  let i16 (x: i16) = intrinsics.sext_i16_i64 x
  let i32 (x: i32) = intrinsics.sext_i32_i64 x
  let i64 (x: i64) = intrinsics.sext_i64_i64 x

  let u8  (x: u8)  = intrinsics.zext_i8_i64 (intrinsics.sign_i8 x)
  let u16 (x: u16) = intrinsics.zext_i16_i64 (intrinsics.sign_i16 x)
  let u32 (x: u32) = intrinsics.zext_i32_i64 (intrinsics.sign_i32 x)
  let u64 (x: u64) = intrinsics.zext_i64_i64 (intrinsics.sign_i64 x)

  let f32 (x: f32) = intrinsics.fptosi_f32_i64 x
  let f64 (x: f64) = intrinsics.fptosi_f64_i64 x

  let bool (x: bool) = if x then 1i64 else 0i64

  let to_i32(x: i64) = intrinsics.sext_i64_i32 x
  let to_i64(x: i64) = intrinsics.sext_i64_i64 x

  let (x: i64) == (y: i64) = intrinsics.eq_i64 x y
  let (x: i64) < (y: i64) = intrinsics.slt64 x y
  let (x: i64) > (y: i64) = intrinsics.slt64 y x
  let (x: i64) <= (y: i64) = intrinsics.sle64 x y
  let (x: i64) >= (y: i64) = intrinsics.sle64 y x
  let (x: i64) != (y: i64) = ! (x == y)

  let sgn (x: i64) = intrinsics.ssignum64 x
  let abs (x: i64) = intrinsics.abs64 x

  let negate (x: t) = -x
  let max (x: t) (y: t) = intrinsics.smax64 x y
  let min (x: t) (y: t) = intrinsics.smin64 x y

  let num_bits = 8
  let get_bit (bit: i32) (x: t) = to_i32 ((x >> i32 bit) & i32 1)
  let set_bit (bit: i32) (x: t) (b: i32) =
    ((x & i32 (intrinsics.~(1 intrinsics.<< b))) | x intrinsics.<< i32 b)

  let iota (n: i64) = 0i64..1i64..<n
  let replicate 'v (n: i64) (x: v) = map (const x) (iota n)
}

module u8: (size with t = u8) = {
  type t = u8

  let sign (x: u8) = intrinsics.sign_i8 x
  let unsign (x: i8) = intrinsics.unsign_i8 x

  let (x: u8) + (y: u8) = unsign (intrinsics.add8 (sign x) (sign y))
  let (x: u8) - (y: u8) = unsign (intrinsics.sub8 (sign x) (sign y))
  let (x: u8) * (y: u8) = unsign (intrinsics.mul8 (sign x) (sign y))
  let (x: u8) / (y: u8) = unsign (intrinsics.udiv8 (sign x) (sign y))
  let (x: u8) ** (y: u8) = unsign (intrinsics.pow8 (sign x) (sign y))
  let (x: u8) % (y: u8) = unsign (intrinsics.umod8 (sign x) (sign y))
  let (x: u8) // (y: u8) = unsign (intrinsics.udiv8 (sign x) (sign y))
  let (x: u8) %% (y: u8) = unsign (intrinsics.umod8 (sign x) (sign y))

  let (x: u8) & (y: u8) = unsign (intrinsics.and8 (sign x) (sign y))
  let (x: u8) | (y: u8) = unsign (intrinsics.or8 (sign x) (sign y))
  let (x: u8) ^ (y: u8) = unsign (intrinsics.xor8 (sign x) (sign y))
  let ~ (x: u8) = unsign (intrinsics.complement8 (sign x))

  let (x: u8) << (y: u8) = unsign (intrinsics.shl8 (sign x) (sign y))
  let (x: u8) >> (y: u8) = unsign (intrinsics.ashr8 (sign x) (sign y))
  let (x: u8) >>> (y: u8) = unsign (intrinsics.lshr8 (sign x) (sign y))

  let u8  (x: u8)  = unsign (i8.u8 x)
  let u16 (x: u16) = unsign (i8.u16 x)
  let u32 (x: u32) = unsign (i8.u32 x)
  let u64 (x: u64) = unsign (i8.u64 x)

  let i8  (x: i8)  = unsign (intrinsics.zext_i8_i8 x)
  let i16 (x: i16) = unsign (intrinsics.zext_i16_i8 x)
  let i32 (x: i32) = unsign (intrinsics.zext_i32_i8 x)
  let i64 (x: i64) = unsign (intrinsics.zext_i64_i8 x)

  let f32 (x: f32) = unsign (intrinsics.fptoui_f32_i8 x)
  let f64 (x: f64) = unsign (intrinsics.fptoui_f64_i8 x)

  let bool (x: bool) = if x then 1u8 else 0u8

  let to_i32(x: u8) = intrinsics.zext_i8_i32 (sign x)
  let to_i64(x: u8) = intrinsics.zext_i8_i64 (sign x)

  let (x: u8) == (y: u8) = intrinsics.eq_i8 (sign x) (sign y)
  let (x: u8) < (y: u8) = intrinsics.ult8 (sign x) (sign y)
  let (x: u8) > (y: u8) = intrinsics.ult8 (sign y) (sign x)
  let (x: u8) <= (y: u8) = intrinsics.ule8 (sign x) (sign y)
  let (x: u8) >= (y: u8) = intrinsics.ule8 (sign y) (sign x)
  let (x: u8) != (y: u8) = ! (x == y)

  let sgn (x: u8) = unsign (intrinsics.usignum8 (sign x))
  let abs (x: u8) = x

  let negate (x: t) = -x
  let max (x: t) (y: t) = unsign (intrinsics.umax8 (sign x) (sign y))
  let min (x: t) (y: t) = unsign (intrinsics.umin8 (sign x) (sign y))

  let num_bits = 8
  let get_bit (bit: i32) (x: t) = to_i32 ((x >> i32 bit) & i32 1)
  let set_bit (bit: i32) (x: t) (b: i32) =
    ((x & i32 (intrinsics.~(1 intrinsics.<< b))) | x intrinsics.<< i32 b)

  let iota (n: u8) = 0u8..1u8..<n
  let replicate 'v (n: u8) (x: v) = map (const x) (iota n)
}

module u16: (size with t = u16) = {
  type t = u16

  let sign (x: u16) = intrinsics.sign_i16 x
  let unsign (x: i16) = intrinsics.unsign_i16 x

  let (x: u16) + (y: u16) = unsign (intrinsics.add16 (sign x) (sign y))
  let (x: u16) - (y: u16) = unsign (intrinsics.sub16 (sign x) (sign y))
  let (x: u16) * (y: u16) = unsign (intrinsics.mul16 (sign x) (sign y))
  let (x: u16) / (y: u16) = unsign (intrinsics.udiv16 (sign x) (sign y))
  let (x: u16) ** (y: u16) = unsign (intrinsics.pow16 (sign x) (sign y))
  let (x: u16) % (y: u16) = unsign (intrinsics.umod16 (sign x) (sign y))
  let (x: u16) // (y: u16) = unsign (intrinsics.udiv16 (sign x) (sign y))
  let (x: u16) %% (y: u16) = unsign (intrinsics.umod16 (sign x) (sign y))

  let (x: u16) & (y: u16) = unsign (intrinsics.and16 (sign x) (sign y))
  let (x: u16) | (y: u16) = unsign (intrinsics.or16 (sign x) (sign y))
  let (x: u16) ^ (y: u16) = unsign (intrinsics.xor16 (sign x) (sign y))
  let ~ (x: u16) = unsign (intrinsics.complement16 (sign x))

  let (x: u16) << (y: u16) = unsign (intrinsics.shl16 (sign x) (sign y))
  let (x: u16) >> (y: u16) = unsign (intrinsics.ashr16 (sign x) (sign y))
  let (x: u16) >>> (y: u16) = unsign (intrinsics.lshr16 (sign x) (sign y))

  let u8  (x: u8)  = unsign (i16.u8 x)
  let u16 (x: u16) = unsign (i16.u16 x)
  let u32 (x: u32) = unsign (i16.u32 x)
  let u64 (x: u64) = unsign (i16.u64 x)

  let i8  (x: i8)  = unsign (intrinsics.zext_i8_i16 x)
  let i16 (x: i16) = unsign (intrinsics.zext_i16_i16 x)
  let i32 (x: i32) = unsign (intrinsics.zext_i32_i16 x)
  let i64 (x: i64) = unsign (intrinsics.zext_i64_i16 x)

  let f32 (x: f32) = unsign (intrinsics.fptoui_f32_i16 x)
  let f64 (x: f64) = unsign (intrinsics.fptoui_f64_i16 x)

  let bool (x: bool) = if x then 1u16 else 0u16

  let to_i32(x: u16) = intrinsics.zext_i16_i32 (sign x)
  let to_i64(x: u16) = intrinsics.zext_i16_i64 (sign x)

  let (x: u16) == (y: u16) = intrinsics.eq_i16 (sign x) (sign y)
  let (x: u16) < (y: u16) = intrinsics.ult16 (sign x) (sign y)
  let (x: u16) > (y: u16) = intrinsics.ult16 (sign y) (sign x)
  let (x: u16) <= (y: u16) = intrinsics.ule16 (sign x) (sign y)
  let (x: u16) >= (y: u16) = intrinsics.ule16 (sign y) (sign x)
  let (x: u16) != (y: u16) = ! (x == y)

  let sgn (x: u16) = unsign (intrinsics.usignum16 (sign x))
  let abs (x: u16) = x

  let negate (x: t) = -x
  let max (x: t) (y: t) = unsign (intrinsics.umax16 (sign x) (sign y))
  let min (x: t) (y: t) = unsign (intrinsics.umin16 (sign x) (sign y))

  let num_bits = 8
  let get_bit (bit: i32) (x: t) = to_i32 ((x >> i32 bit) & i32 1)
  let set_bit (bit: i32) (x: t) (b: i32) =
    ((x & i32 (intrinsics.~(1 intrinsics.<< b))) | x intrinsics.<< i32 b)

  let iota (n: u16) = 0u16..1u16..<n
  let replicate 'v (n: u16) (x: v) = map (const x) (iota n)
}

module u32: (size with t = u32) = {
  type t = u32

  let sign (x: u32) = intrinsics.sign_i32 x
  let unsign (x: i32) = intrinsics.unsign_i32 x

  let (x: u32) + (y: u32) = unsign (intrinsics.add32 (sign x) (sign y))
  let (x: u32) - (y: u32) = unsign (intrinsics.sub32 (sign x) (sign y))
  let (x: u32) * (y: u32) = unsign (intrinsics.mul32 (sign x) (sign y))
  let (x: u32) / (y: u32) = unsign (intrinsics.udiv32 (sign x) (sign y))
  let (x: u32) ** (y: u32) = unsign (intrinsics.pow32 (sign x) (sign y))
  let (x: u32) % (y: u32) = unsign (intrinsics.umod32 (sign x) (sign y))
  let (x: u32) // (y: u32) = unsign (intrinsics.udiv32 (sign x) (sign y))
  let (x: u32) %% (y: u32) = unsign (intrinsics.umod32 (sign x) (sign y))

  let (x: u32) & (y: u32) = unsign (intrinsics.and32 (sign x) (sign y))
  let (x: u32) | (y: u32) = unsign (intrinsics.or32 (sign x) (sign y))
  let (x: u32) ^ (y: u32) = unsign (intrinsics.xor32 (sign x) (sign y))
  let ~ (x: u32) = unsign (intrinsics.complement32 (sign x))

  let (x: u32) << (y: u32) = unsign (intrinsics.shl32 (sign x) (sign y))
  let (x: u32) >> (y: u32) = unsign (intrinsics.ashr32 (sign x) (sign y))
  let (x: u32) >>> (y: u32) = unsign (intrinsics.lshr32 (sign x) (sign y))

  let u8  (x: u8)  = unsign (i32.u8 x)
  let u16 (x: u16) = unsign (i32.u16 x)
  let u32 (x: u32) = unsign (i32.u32 x)
  let u64 (x: u64) = unsign (i32.u64 x)

  let i8  (x: i8)  = unsign (intrinsics.zext_i8_i32 x)
  let i16 (x: i16) = unsign (intrinsics.zext_i16_i32 x)
  let i32 (x: i32) = unsign (intrinsics.zext_i32_i32 x)
  let i64 (x: i64) = unsign (intrinsics.zext_i64_i32 x)

  let f32 (x: f32) = unsign (intrinsics.fptoui_f32_i32 x)
  let f64 (x: f64) = unsign (intrinsics.fptoui_f64_i32 x)

  let bool (x: bool) = if x then 1u32 else 0u32

  let to_i32(x: u32) = intrinsics.zext_i32_i32 (sign x)
  let to_i64(x: u32) = intrinsics.zext_i32_i64 (sign x)

  let (x: u32) == (y: u32) = intrinsics.eq_i32 (sign x) (sign y)
  let (x: u32) < (y: u32) = intrinsics.ult32 (sign x) (sign y)
  let (x: u32) > (y: u32) = intrinsics.ult32 (sign y) (sign x)
  let (x: u32) <= (y: u32) = intrinsics.ule32 (sign x) (sign y)
  let (x: u32) >= (y: u32) = intrinsics.ule32 (sign y) (sign x)
  let (x: u32) != (y: u32) = ! (x == y)

  let sgn (x: u32) = unsign (intrinsics.usignum32 (sign x))
  let abs (x: u32) = x

  let negate (x: t) = -x
  let max (x: t) (y: t) = unsign (intrinsics.umax32 (sign x) (sign y))
  let min (x: t) (y: t) = unsign (intrinsics.umin32 (sign x) (sign y))

  let num_bits = 8
  let get_bit (bit: i32) (x: t) = to_i32 ((x >> i32 bit) & i32 1)
  let set_bit (bit: i32) (x: t) (b: i32) =
    ((x & i32 (intrinsics.~(1 intrinsics.<< b))) | x intrinsics.<< i32 b)

  let iota (n: u32) = 0u32..1u32..<n
  let replicate 'v (n: u32) (x: v) = map (const x) (iota n)
}

module u64: (size with t = u64) = {
  type t = u64

  let sign (x: u64) = intrinsics.sign_i64 x
  let unsign (x: i64) = intrinsics.unsign_i64 x

  let (x: u64) + (y: u64) = unsign (intrinsics.add64 (sign x) (sign y))
  let (x: u64) - (y: u64) = unsign (intrinsics.sub64 (sign x) (sign y))
  let (x: u64) * (y: u64) = unsign (intrinsics.mul64 (sign x) (sign y))
  let (x: u64) / (y: u64) = unsign (intrinsics.udiv64 (sign x) (sign y))
  let (x: u64) ** (y: u64) = unsign (intrinsics.pow64 (sign x) (sign y))
  let (x: u64) % (y: u64) = unsign (intrinsics.umod64 (sign x) (sign y))
  let (x: u64) // (y: u64) = unsign (intrinsics.udiv64 (sign x) (sign y))
  let (x: u64) %% (y: u64) = unsign (intrinsics.umod64 (sign x) (sign y))

  let (x: u64) & (y: u64) = unsign (intrinsics.and64 (sign x) (sign y))
  let (x: u64) | (y: u64) = unsign (intrinsics.or64 (sign x) (sign y))
  let (x: u64) ^ (y: u64) = unsign (intrinsics.xor64 (sign x) (sign y))
  let ~ (x: u64) = unsign (intrinsics.complement64 (sign x))

  let (x: u64) << (y: u64) = unsign (intrinsics.shl64 (sign x) (sign y))
  let (x: u64) >> (y: u64) = unsign (intrinsics.ashr64 (sign x) (sign y))
  let (x: u64) >>> (y: u64) = unsign (intrinsics.lshr64 (sign x) (sign y))

  let u8  (x: u8)  = unsign (i64.u8 x)
  let u16 (x: u16) = unsign (i64.u16 x)
  let u32 (x: u32) = unsign (i64.u32 x)
  let u64 (x: u64) = unsign (i64.u64 x)

  let i8 (x: i8)   = unsign (intrinsics.zext_i8_i64 x)
  let i16 (x: i16) = unsign (intrinsics.zext_i16_i64 x)
  let i32 (x: i32) = unsign (intrinsics.zext_i32_i64 x)
  let i64 (x: i64) = unsign (intrinsics.zext_i64_i64 x)

  let f32 (x: f32) = unsign (intrinsics.fptoui_f32_i64 x)
  let f64 (x: f64) = unsign (intrinsics.fptoui_f64_i64 x)

  let bool (x: bool) = if x then 1u64 else 0u64

  let to_i32(x: u64) = intrinsics.zext_i64_i32 (sign x)
  let to_i64(x: u64) = intrinsics.zext_i64_i64 (sign x)

  let (x: u64) == (y: u64) = intrinsics.eq_i64 (sign x) (sign y)
  let (x: u64) < (y: u64) = intrinsics.ult64 (sign x) (sign y)
  let (x: u64) > (y: u64) = intrinsics.ult64 (sign y) (sign x)
  let (x: u64) <= (y: u64) = intrinsics.ule64 (sign x) (sign y)
  let (x: u64) >= (y: u64) = intrinsics.ule64 (sign y) (sign x)
  let (x: u64) != (y: u64) = ! (x == y)

  let sgn (x: u64) = unsign (intrinsics.usignum64 (sign x))
  let abs (x: u64) = x

  let negate (x: t) = -x
  let max (x: t) (y: t) = unsign (intrinsics.umax64 (sign x) (sign y))
  let min (x: t) (y: t) = unsign (intrinsics.umin64 (sign x) (sign y))

  let num_bits = 8
  let get_bit (bit: i32) (x: t) = to_i32 ((x >> i32 bit) & i32 1)
  let set_bit (bit: i32) (x: t) (b: i32) =
    ((x & i32 (intrinsics.~(1 intrinsics.<< b))) | x intrinsics.<< i32 b)

  let iota (n: u64) = 0u64..1u64..<n
  let replicate 'v (n: u64) (x: v) = map (const x) (iota n)
}

module f64: (float with t = f64 with int_t = u64) = {
  type t = f64
  type int_t = u64

  module i64m = i64
  module u64m = u64

  let (x: f64) + (y: f64) = intrinsics.fadd64 x y
  let (x: f64) - (y: f64) = intrinsics.fsub64 x y
  let (x: f64) * (y: f64) = intrinsics.fmul64 x y
  let (x: f64) / (y: f64) = intrinsics.fdiv64 x y
  let (x: f64) ** (y: f64) = intrinsics.fpow64 x y

  let u8  (x: u8)  = intrinsics.uitofp_i8_f64  (i8.u8 x)
  let u16 (x: u16) = intrinsics.uitofp_i16_f64 (i16.u16 x)
  let u32 (x: u32) = intrinsics.uitofp_i32_f64 (i32.u32 x)
  let u64 (x: u64) = intrinsics.uitofp_i64_f64 (i64.u64 x)

  let i8 (x: i8) = intrinsics.sitofp_i8_f64 x
  let i16 (x: i16) = intrinsics.sitofp_i16_f64 x
  let i32 (x: i32) = intrinsics.sitofp_i32_f64 x
  let i64 (x: i64) = intrinsics.sitofp_i64_f64 x

  let f32 (x: f32) = intrinsics.fpconv_f32_f64 x
  let f64 (x: f64) = intrinsics.fpconv_f64_f64 x

  let bool (x: bool) = if x then 1f64 else 0f64

  let from_fraction (x: i32) (y: i32) = i32 x / i32 y
  let to_i32 (x: f64) = intrinsics.fptosi_f64_i32 x
  let to_i64 (x: f64) = intrinsics.fptosi_f64_i64 x
  let to_f64 (x: f64) = x

  let (x: f64) == (y: f64) = intrinsics.eq_f64 x y
  let (x: f64) < (y: f64) = intrinsics.lt64 x y
  let (x: f64) > (y: f64) = intrinsics.lt64 y x
  let (x: f64) <= (y: f64) = intrinsics.le64 x y
  let (x: f64) >= (y: f64) = intrinsics.le64 y x
  let (x: f64) != (y: f64) = ! (x == y)

  let negate (x: t) = -x
  let max (x: t) (y: t) = intrinsics.fmax64 x y
  let min (x: t) (y: t) = intrinsics.fmin64 x y

  let sgn (x: f64) = if      x < 0f64  then -1f64
                     else if x == 0f64 then  0f64
                     else                    1f64
  let abs (x: f64) = intrinsics.fabs64 x

  let sqrt (x: f64) = intrinsics.sqrt64 x

  let log (x: f64) = intrinsics.log64 x
  let exp (x: f64) = intrinsics.exp64 x
  let cos (x: f64) = intrinsics.cos64 x
  let sin (x: f64) = intrinsics.sin64 x
  let acos (x: f64) = intrinsics.acos64 x
  let asin (x: f64) = intrinsics.asin64 x
  let atan (x: f64) = intrinsics.atan64 x
  let atan2 (x: f64) (y: f64) = intrinsics.atan2_64 x y

  let ceil (x: f64) : f64 =
    let i = to_i64 x
    let ix = i64 i
    in if x >= 0.0 then
         if ix < x then i64 (i i64m.+ 1i64) else x
       else if ix > x then ix else x

  let floor (x: f64) : f64 =
    let i = to_i64 x
    let ix = i64 i
    in if x >= 0.0 then
         if ix < x then ix else x
       else if ix > x then i64 (i i64m.- 1i64) else x

  let trunc (x: f64) : f64 = i64 (i64m.f64 x)

  let even (x: f64) = i64m.f64 x % 2i64 i64m.== 0i64

  let round (x: f64) : f64 =
    let t0 = x + 0.5f64
    let floor_t0 = floor t0
    in if floor_t0 == t0 then
	  let t = floor x
	  in if even t then t else floor_t0
	else floor_t0

  let to_bits (x: f64): u64 = u64m.i64 (intrinsics.to_bits64 x)
  let from_bits (x: u64): f64 = intrinsics.from_bits64 (intrinsics.sign_i64 x)

  let num_bits = 64
  let get_bit (bit: i32) (x: t) = u64m.get_bit bit (to_bits x)
  let set_bit (bit: i32) (x: t) (b: i32) = from_bits (u64m.set_bit bit (to_bits x) b)

  let isinf (x: f64) = intrinsics.isinf64 x
  let isnan (x: f64) = intrinsics.isnan64 x

  let inf = 1f64 / 0f64
  let nan = 0f64 / 0f64

  let pi = 3.1415926535897932384626433832795028841971693993751058209749445923078164062f64
  let e = 2.718281828459045235360287471352662497757247093699959574966967627724076630353f64
}

module f32: (float with t = f32 with int_t = u32) = {
  type t = f32
  type int_t = u32

  module i32m = i32
  module u32m = u32
  module f64m = f64

  let (x: f32) + (y: f32) = intrinsics.fadd32 x y
  let (x: f32) - (y: f32) = intrinsics.fsub32 x y
  let (x: f32) * (y: f32) = intrinsics.fmul32 x y
  let (x: f32) / (y: f32) = intrinsics.fdiv32 x y
  let (x: f32) ** (y: f32) = intrinsics.fpow32 x y

  let u8  (x: u8)  = intrinsics.uitofp_i8_f32  (i8.u8 x)
  let u16 (x: u16) = intrinsics.uitofp_i16_f32 (i16.u16 x)
  let u32 (x: u32) = intrinsics.uitofp_i32_f32 (i32.u32 x)
  let u64 (x: u64) = intrinsics.uitofp_i64_f32 (i64.u64 x)

  let i8 (x: i8) = intrinsics.sitofp_i8_f32 x
  let i16 (x: i16) = intrinsics.sitofp_i16_f32 x
  let i32 (x: i32) = intrinsics.sitofp_i32_f32 x
  let i64 (x: i64) = intrinsics.sitofp_i64_f32 x

  let f32 (x: f32) = intrinsics.fpconv_f32_f32 x
  let f64 (x: f64) = intrinsics.fpconv_f64_f32 x

  let bool (x: bool) = if x then 1f32 else 0f32

  let from_fraction (x: i32) (y: i32) = i32 x / i32 y
  let to_i32 (x: f32) = intrinsics.fptosi_f32_i32 x
  let to_i64 (x: f32) = intrinsics.fptosi_f32_i64 x
  let to_f64 (x: f32) = intrinsics.fpconv_f32_f64 x

  let (x: f32) == (y: f32) = intrinsics.eq_f32 x y
  let (x: f32) < (y: f32) = intrinsics.lt32 x y
  let (x: f32) > (y: f32) = intrinsics.lt32 y x
  let (x: f32) <= (y: f32) = intrinsics.le32 x y
  let (x: f32) >= (y: f32) = intrinsics.le32 y x
  let (x: f32) != (y: f32) = ! (x == y)

  let negate (x: t) = -x
  let max (x: t) (y: t) = intrinsics.fmax32 x y
  let min (x: t) (y: t) = intrinsics.fmin32 x y

  let sgn (x: f32) = if      x < 0f32  then -1f32
                     else if x == 0f32 then  0f32
                     else                    1f32
  let abs (x: f32) = intrinsics.fabs32 x

  let sqrt (x: f32) = intrinsics.sqrt32 x

  let log (x: f32) = intrinsics.log32 x
  let exp (x: f32) = intrinsics.exp32 x
  let cos (x: f32) = intrinsics.cos32 x
  let sin (x: f32) = intrinsics.sin32 x
  let acos (x: f32) = intrinsics.acos32 x
  let asin (x: f32) = intrinsics.asin32 x
  let atan (x: f32) = intrinsics.atan32 x
  let atan2 (x: f32) (y: f32) = intrinsics.atan2_32 x y

  let ceil (x: f32) : f32 =
    let i = to_i32 x
    let ix = i32 i
    in if x >= 0f32 then
         if ix < x then i32 (i i32m.+ 1i32) else x
       else if ix > x then ix else x

  let floor (x: f32) : f32 =
    let i = to_i32 x
    let ix = i32 i
    in if x >= 0f32 then
         if ix < x then ix else x
       else if ix > x then i32 (i i32m.- 1i32) else x

  let trunc (x: f32) : f32 = i32 (i32m.f32 x)

  let even (x: f32) = i32m.f32 x % 2i32 i32m.== 0i32

  let round (x: f32) : f32 =
    let t0 = x + 0.5f32
    let floor_t0 = floor t0
    in if floor_t0 == t0 then
	  let t = floor x
	  in if even t then t else floor_t0
	else floor_t0

  let to_bits (x: f32): u32 = u32m.i32 (intrinsics.to_bits32 x)
  let from_bits (x: u32): f32 = intrinsics.from_bits32 (intrinsics.sign_i32 x)

  let num_bits = 32
  let get_bit (bit: i32) (x: t) = u32m.get_bit bit (to_bits x)
  let set_bit (bit: i32) (x: t) (b: i32) = from_bits (u32m.set_bit bit (to_bits x) b)

  let isinf (x: f32) = intrinsics.isinf32 x
  let isnan (x: f32) = intrinsics.isnan32 x

  let inf = 1f32 / 0f32
  let nan = 0f32 / 0f32

  let pi = f64 f64m.pi
  let e = f64 f64m.pi
}
