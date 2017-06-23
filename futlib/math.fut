module type numeric = {
  type t

  val +: t -> t -> t
  val -: t -> t -> t
  val *: t -> t -> t
  val /: t -> t -> t
  val **: t -> t -> t

  val from_i32: i32 -> t
  val from_i64: i64 -> t
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

  val <<: t -> t -> t
  val >>: t -> t -> t
  val >>>: t -> t -> t
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
  val from_f64: f64 -> t
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

  let (x: i8) << (y: i8) = intrinsics.shl8 x y
  let (x: i8) >> (y: i8) = intrinsics.ashr8 x y
  let (x: i8) >>> (y: i8) = intrinsics.lshr8 x y

  let from_i32(x: i32) = i8 x
  let from_i64(x: i64) = i8 x
  let to_i64(x: i8) = i64 x

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

  let iota (n: i8) = intrinsics.iota_i8 n
  let replicate 'v (n: i8) (x: v) = intrinsics.replicate (i32 n) x
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

  let (x: i16) << (y: i16) = intrinsics.shl16 x y
  let (x: i16) >> (y: i16) = intrinsics.ashr16 x y
  let (x: i16) >>> (y: i16) = intrinsics.lshr16 x y

  let from_i32(x: i32) = i16 x
  let from_i64(x: i64) = i16 x
  let to_i64(x: i16) = i64 x

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

  let iota (n: i16) = intrinsics.iota_i16 n
  let replicate 'v (n: i16) (x: v) = intrinsics.replicate (i32 n) x
}

module i32: (size with t = i32) = {
  type t = i32

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

  let (x: i32) << (y: i32) = intrinsics.shl32 x y
  let (x: i32) >> (y: i32) = intrinsics.ashr32 x y
  let (x: i32) >>> (y: i32) = intrinsics.lshr32 x y

  let from_i32(x: i32) = x
  let from_i64(x: i64) = i32 x
  let to_i64(x: i32) = i64 x

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

  let iota (n: i32) = intrinsics.iota_i32 n
  let replicate 'v (n: i32) (x: v) = intrinsics.replicate (i32 n) x
}

module i64: (size with t = i64) = {
  type t = i64

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

  let (x: i64) << (y: i64) = intrinsics.shl64 x y
  let (x: i64) >> (y: i64) = intrinsics.ashr64 x y
  let (x: i64) >>> (y: i64) = intrinsics.lshr64 x y

  let from_i32(x: i32) = i64 x
  let from_i64(x: i64) = x
  let to_i64(x: i64) = x

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

  let iota (n: i64) = intrinsics.iota_i64 n
  let replicate 'v (n: i64) (x: v) = intrinsics.replicate (i32 n) x
}

module u8: (size with t = u8) = {
  type t = u8

  let (x: u8) + (y: u8) = u8 (intrinsics.add8 (i8 x) (i8 y))
  let (x: u8) - (y: u8) = u8 (intrinsics.sub8 (i8 x) (i8 y))
  let (x: u8) * (y: u8) = u8 (intrinsics.mul8 (i8 x) (i8 y))
  let (x: u8) / (y: u8) = u8 (intrinsics.udiv8 (i8 x) (i8 y))
  let (x: u8) ** (y: u8) = u8 (intrinsics.pow8 (i8 x) (i8 y))
  let (x: u8) % (y: u8) = u8 (intrinsics.umod8 (i8 x) (i8 y))
  let (x: u8) // (y: u8) = u8 (intrinsics.udiv8 (i8 x) (i8 y))
  let (x: u8) %% (y: u8) = u8 (intrinsics.umod8 (i8 x) (i8 y))

  let (x: u8) & (y: u8) = u8 (intrinsics.and8 (i8 x) (i8 y))
  let (x: u8) | (y: u8) = u8 (intrinsics.or8 (i8 x) (i8 y))
  let (x: u8) ^ (y: u8) = u8 (intrinsics.xor8 (i8 x) (i8 y))

  let (x: u8) << (y: u8) = u8 (intrinsics.shl8 (i8 x) (i8 y))
  let (x: u8) >> (y: u8) = u8 (intrinsics.ashr8 (i8 x) (i8 y))
  let (x: u8) >>> (y: u8) = u8 (intrinsics.lshr8 (i8 x) (i8 y))

  let from_i32(x: i32) = u8 x
  let from_i64(x: i64) = u8 x
  let to_i64(x: u8) = i64 x

  let (x: u8) == (y: u8) = intrinsics.eq_i8 (i8 x) (i8 y)
  let (x: u8) < (y: u8) = intrinsics.ult8 (i8 x) (i8 y)
  let (x: u8) > (y: u8) = intrinsics.ult8 (i8 y) (i8 x)
  let (x: u8) <= (y: u8) = intrinsics.ule8 (i8 x) (i8 y)
  let (x: u8) >= (y: u8) = intrinsics.ule8 (i8 y) (i8 x)
  let (x: u8) != (y: u8) = ! (x == y)

  let sgn (x: u8) = u8 (intrinsics.usignum8 (i8 x))
  let abs (x: u8) = x

  let negate (x: t) = -x
  let max (x: t) (y: t) = u8 (intrinsics.umax8 (i8 x) (i8 y))
  let min (x: t) (y: t) = u8 (intrinsics.umin8 (i8 x) (i8 y))

  let iota (n: u8) = intrinsics.iota_u8 n
  let replicate 'v (n: u8) (x: v) = intrinsics.replicate (i32 n) x
}

module u16: (size with t = u16) = {
  type t = u16

  let (x: u16) + (y: u16) = u16 (intrinsics.add16 (i16 x) (i16 y))
  let (x: u16) - (y: u16) = u16 (intrinsics.sub16 (i16 x) (i16 y))
  let (x: u16) * (y: u16) = u16 (intrinsics.mul16 (i16 x) (i16 y))
  let (x: u16) / (y: u16) = u16 (intrinsics.udiv16 (i16 x) (i16 y))
  let (x: u16) ** (y: u16) = u16 (intrinsics.pow16 (i16 x) (i16 y))
  let (x: u16) % (y: u16) = u16 (intrinsics.umod16 (i16 x) (i16 y))
  let (x: u16) // (y: u16) = u16 (intrinsics.udiv16 (i16 x) (i16 y))
  let (x: u16) %% (y: u16) = u16 (intrinsics.umod16 (i16 x) (i16 y))

  let (x: u16) & (y: u16) = u16 (intrinsics.and16 (i16 x) (i16 y))
  let (x: u16) | (y: u16) = u16 (intrinsics.or16 (i16 x) (i16 y))
  let (x: u16) ^ (y: u16) = u16 (intrinsics.xor16 (i16 x) (i16 y))

  let (x: u16) << (y: u16) = u16 (intrinsics.shl16 (i16 x) (i16 y))
  let (x: u16) >> (y: u16) = u16 (intrinsics.ashr16 (i16 x) (i16 y))
  let (x: u16) >>> (y: u16) = u16 (intrinsics.lshr16 (i16 x) (i16 y))

  let from_i32(x: i32) = u16 x
  let from_i64(x: i64) = u16 x
  let to_i64(x: u16) = i64 x

  let (x: u16) == (y: u16) = intrinsics.eq_i16 (i16 x) (i16 y)
  let (x: u16) < (y: u16) = intrinsics.ult16 (i16 x) (i16 y)
  let (x: u16) > (y: u16) = intrinsics.ult16 (i16 y) (i16 x)
  let (x: u16) <= (y: u16) = intrinsics.ule16 (i16 x) (i16 y)
  let (x: u16) >= (y: u16) = intrinsics.ule16 (i16 y) (i16 x)
  let (x: u16) != (y: u16) = ! (x == y)

  let sgn (x: u16) = u16 (intrinsics.usignum16 (i16 x))
  let abs (x: u16) = x

  let negate (x: t) = -x
  let max (x: t) (y: t) = u16 (intrinsics.umax16 (i16 x) (i16 y))
  let min (x: t) (y: t) = u16 (intrinsics.umin16 (i16 x) (i16 y))

  let iota (n: u16) = intrinsics.iota_u16 n
  let replicate 'v (n: u16) (x: v) = intrinsics.replicate (i32 n) x
}

module u32: (size with t = u32) = {
  type t = u32

  let (x: u32) + (y: u32) = u32 (intrinsics.add32 (i32 x) (i32 y))
  let (x: u32) - (y: u32) = u32 (intrinsics.sub32 (i32 x) (i32 y))
  let (x: u32) * (y: u32) = u32 (intrinsics.mul32 (i32 x) (i32 y))
  let (x: u32) / (y: u32) = u32 (intrinsics.udiv32 (i32 x) (i32 y))
  let (x: u32) ** (y: u32) = u32 (intrinsics.pow32 (i32 x) (i32 y))
  let (x: u32) % (y: u32) = u32 (intrinsics.umod32 (i32 x) (i32 y))
  let (x: u32) // (y: u32) = u32 (intrinsics.udiv32 (i32 x) (i32 y))
  let (x: u32) %% (y: u32) = u32 (intrinsics.umod32 (i32 x) (i32 y))

  let (x: u32) & (y: u32) = u32 (intrinsics.and32 (i32 x) (i32 y))
  let (x: u32) | (y: u32) = u32 (intrinsics.or32 (i32 x) (i32 y))
  let (x: u32) ^ (y: u32) = u32 (intrinsics.xor32 (i32 x) (i32 y))

  let (x: u32) << (y: u32) = u32 (intrinsics.shl32 (i32 x) (i32 y))
  let (x: u32) >> (y: u32) = u32 (intrinsics.ashr32 (i32 x) (i32 y))
  let (x: u32) >>> (y: u32) = u32 (intrinsics.lshr32 (i32 x) (i32 y))

  let from_i32(x: i32) = u32 x
  let from_i64(x: i64) = u32 x
  let to_i64(x: u32) = i64 x

  let (x: u32) == (y: u32) = intrinsics.eq_i32 (i32 x) (i32 y)
  let (x: u32) < (y: u32) = intrinsics.ult32 (i32 x) (i32 y)
  let (x: u32) > (y: u32) = intrinsics.ult32 (i32 y) (i32 x)
  let (x: u32) <= (y: u32) = intrinsics.ule32 (i32 x) (i32 y)
  let (x: u32) >= (y: u32) = intrinsics.ule32 (i32 y) (i32 x)
  let (x: u32) != (y: u32) = ! (x == y)

  let sgn (x: u32) = u32 (intrinsics.usignum32 (i32 x))
  let abs (x: u32) = x

  let negate (x: t) = -x
  let max (x: t) (y: t) = u32 (intrinsics.umax32 (i32 x) (i32 y))
  let min (x: t) (y: t) = u32 (intrinsics.umin32 (i32 x) (i32 y))

  let iota (n: u32) = intrinsics.iota_u32 n
  let replicate 'v (n: u32) (x: v) = intrinsics.replicate (i32 n) x
}

module u64: (size with t = u64) = {
  type t = u64

  let (x: u64) + (y: u64) = u64 (intrinsics.add64 (i64 x) (i64 y))
  let (x: u64) - (y: u64) = u64 (intrinsics.sub64 (i64 x) (i64 y))
  let (x: u64) * (y: u64) = u64 (intrinsics.mul64 (i64 x) (i64 y))
  let (x: u64) / (y: u64) = u64 (intrinsics.udiv64 (i64 x) (i64 y))
  let (x: u64) ** (y: u64) = u64 (intrinsics.pow64 (i64 x) (i64 y))
  let (x: u64) % (y: u64) = u64 (intrinsics.umod64 (i64 x) (i64 y))
  let (x: u64) // (y: u64) = u64 (intrinsics.udiv64 (i64 x) (i64 y))
  let (x: u64) %% (y: u64) = u64 (intrinsics.umod64 (i64 x) (i64 y))

  let (x: u64) & (y: u64) = u64 (intrinsics.and64 (i64 x) (i64 y))
  let (x: u64) | (y: u64) = u64 (intrinsics.or64 (i64 x) (i64 y))
  let (x: u64) ^ (y: u64) = u64 (intrinsics.xor64 (i64 x) (i64 y))

  let (x: u64) << (y: u64) = u64 (intrinsics.shl64 (i64 x) (i64 y))
  let (x: u64) >> (y: u64) = u64 (intrinsics.ashr64 (i64 x) (i64 y))
  let (x: u64) >>> (y: u64) = u64 (intrinsics.lshr64 (i64 x) (i64 y))

  let from_i32(x: i32) = u64 x
  let from_i64(x: i64) = u64 x
  let to_i64(x: u64) = i64 x

  let (x: u64) == (y: u64) = intrinsics.eq_i64 (i64 x) (i64 y)
  let (x: u64) < (y: u64) = intrinsics.ult64 (i64 x) (i64 y)
  let (x: u64) > (y: u64) = intrinsics.ult64 (i64 y) (i64 x)
  let (x: u64) <= (y: u64) = intrinsics.ule64 (i64 x) (i64 y)
  let (x: u64) >= (y: u64) = intrinsics.ule64 (i64 y) (i64 x)
  let (x: u64) != (y: u64) = ! (x == y)

  let sgn (x: u64) = u64 (intrinsics.usignum64 (i64 x))
  let abs (x: u64) = x

  let negate (x: t) = -x
  let max (x: t) (y: t) = u64 (intrinsics.umax64 (i64 x) (i64 y))
  let min (x: t) (y: t) = u64 (intrinsics.umin64 (i64 x) (i64 y))

  let iota (n: u64) = intrinsics.iota_u64 n
  let replicate 'v (n: u64) (x: v) = intrinsics.replicate (i32 n) x
}

module f64: (real with t = f64) = {
  type t = f64

  let (x: f64) + (y: f64) = intrinsics.fadd64 x y
  let (x: f64) - (y: f64) = intrinsics.fsub64 x y
  let (x: f64) * (y: f64) = intrinsics.fmul64 x y
  let (x: f64) / (y: f64) = intrinsics.fdiv64 x y
  let (x: f64) ** (y: f64) = intrinsics.fpow64 x y

  let from_i32 (x: i32) = f64 x
  let from_i64 (x: i64) = f64 x
  let from_fraction (x: i32) (y: i32) = f64 x / f64 y
  let to_i32 (x: f64) = i32 x
  let from_f64 (x: f64) = x
  let to_f64   (x: f64) = x
  let to_i64   (x: f64) = i64 x

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

  let ceil (x:f64) : f64 =
    let i = i64 x
    let ix = f64 i
    in if x >= 0.0 then
         if ix < x then f64(i i64.+ 1i64) else x
       else if ix > x then ix else x

  let floor (x:f64) : f64 =
    let i = i64 x
    let ix = f64 i
    in if x >= 0.0 then
         if ix < x then ix else x
       else if ix > x then f64(i i64.- 1i64) else x

  let trunc (x:f64) : f64 = f64(i64 x)

  let even (x:f64) = i64 x % 2i64 i64.== 0i64

  let round (x:f64) : f64 =
    let t0 = x + 0.5f64
    let floor_t0 = floor t0
    in if floor_t0 == t0 then
	  let t = floor x
	  in if even t then t else floor_t0
	else floor_t0

  let isinf (x: f64) = intrinsics.isinf64 x
  let isnan (x: f64) = intrinsics.isnan64 x

  let inf = 1f64 / 0f64
  let nan = 0f64 / 0f64

  let pi = 3.1415926535897932384626433832795028841971693993751058209749445923078164062f64
  let e = 2.718281828459045235360287471352662497757247093699959574966967627724076630353f64
}

module f32: (real with t = f32) = {
  type t = f32

  let (x: f32) + (y: f32) = intrinsics.fadd32 x y
  let (x: f32) - (y: f32) = intrinsics.fsub32 x y
  let (x: f32) * (y: f32) = intrinsics.fmul32 x y
  let (x: f32) / (y: f32) = intrinsics.fdiv32 x y
  let (x: f32) ** (y: f32) = intrinsics.fpow32 x y

  let from_i32 (x: i32) = f32 x
  let from_i64 (x: i64) = f32 x
  let from_fraction (x: i32) (y: i32) = f32 x / f32 y
  let to_i32 (x: f32) = i32 x
  let from_f64 (x: f64) = f32 x
  let to_f64   (x: f32) = f64 x
  let to_i64   (x: f32) = i64 x

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

  let ceil (x:f32) : f32 =
    let i = i32 x
    let ix = f32 i
    in if x >= 0.0f32 then
         if ix < x then f32(i i32.+ 1i32) else x
       else if ix > x then ix else x

  let floor (x:f32) : f32 =
    let i = i32 x
    let ix = f32 i
    in if x >= 0.0f32 then
         if ix < x then ix else x
       else if ix > x then f32(i i32.- 1i32) else x

  let trunc (x:f32) : f32 = f32(i32 x)

  let even (x:f32) = i32 x % 2i32 i32.== 0i32

  let round (x:f32) : f32 =
    let t0 = x + 0.5f32
    let floor_t0 = floor t0
    in if floor_t0 == t0 then
	  let t = floor x
	  in if even t then t else floor_t0
	else floor_t0

  let isinf (x: f32) = intrinsics.isinf32 x
  let isnan (x: f32) = intrinsics.isnan32 x

  let inf = 1f32 / 0f32
  let nan = 0f32 / 0f32

  let pi = f32 f64.pi
  let e = f32 f64.pi
}
