module type NUMERIC = {
  type t

  val +: t -> t -> t
  val -: t -> t -> t
  val *: t -> t -> t
  val /: t -> t -> t

  val fromInt: i32 -> t

  val ==: t -> t -> bool
  val <: t -> t -> bool
  val >: t -> t -> bool

  val sgn: t -> i32
}

module type INTEGRAL = {
  type t

  val +: t -> t -> t
  val -: t -> t -> t
  val *: t -> t -> t
  val /: t -> t -> t
  val %: t -> t -> t

  val fromInt: i32 -> t

  val ==: t -> t -> bool
  val <: t -> t -> bool
  val >: t -> t -> bool

  val sgn: t -> i32
}

module type REAL = {
  type t

  val +: t -> t -> t
  val -: t -> t -> t
  val *: t -> t -> t
  val /: t -> t -> t

  val fromInt: i32 -> t
  val fromFraction: i32 -> i32 -> t
  val toInt: t -> i32

  val ==: t -> t -> bool
  val <: t -> t -> bool
  val >: t -> t -> bool

  val sgn: t -> i32

  val sqrt: t -> t
}

module I8 = {
  type t = i8

  fun (x: i8) + (y: i8) = x Intrinsics.+ y
  fun (x: i8) - (y: i8) = x Intrinsics.- y
  fun (x: i8) * (y: i8) = x Intrinsics.* y
  fun (x: i8) / (y: i8) = x Intrinsics./ y
  fun (x: i8) % (y: i8) = x Intrinsics.% y

  fun fromInt(x: i32) = i8 x

  fun (x: i8) == (y: i8) = x Intrinsics.== y
  fun (x: i8) < (y: i8) = x Intrinsics.< y
  fun (x: i8) > (y: i8) = x Intrinsics.> y

  fun sgn (x: i8) = Intrinsics.sgn x
  fun abs (x: i8) = Intrinsics.abs x
}

module I16 = {
  type t = i16

  fun (x: i16) + (y: i16) = x Intrinsics.+ y
  fun (x: i16) - (y: i16) = x Intrinsics.- y
  fun (x: i16) * (y: i16) = x Intrinsics.* y
  fun (x: i16) / (y: i16) = x Intrinsics./ y
  fun (x: i16) % (y: i16) = x Intrinsics.% y

  fun fromInt(x: i32) = i16 x

  fun (x: i16) == (y: i16) = x Intrinsics.== y
  fun (x: i16) < (y: i16) = x Intrinsics.< y
  fun (x: i16) > (y: i16) = x Intrinsics.> y

  fun sgn (x: i16) = Intrinsics.sgn x
  fun abs (x: i16) = Intrinsics.abs x
}

module I32 = {
  type t = i32

  fun (x: i32) + (y: i32) = x Intrinsics.+ y
  fun (x: i32) - (y: i32) = x Intrinsics.- y
  fun (x: i32) * (y: i32) = x Intrinsics.* y
  fun (x: i32) / (y: i32) = x Intrinsics./ y
  fun (x: i32) % (y: i32) = x Intrinsics.% y

  fun fromInt(x: i32) = x

  fun (x: i32) == (y: i32) = x Intrinsics.== y
  fun (x: i32) < (y: i32) = x Intrinsics.< y
  fun (x: i32) > (y: i32) = x Intrinsics.> y

  fun sgn (x: i32) = Intrinsics.sgn x
  fun abs (x: i32) = Intrinsics.abs x
}

module I64 = {
  type t = i64

  fun (x: i64) + (y: i64) = x Intrinsics.+ y
  fun (x: i64) - (y: i64) = x Intrinsics.- y
  fun (x: i64) * (y: i64) = x Intrinsics.* y
  fun (x: i64) / (y: i64) = x Intrinsics./ y
  fun (x: i64) % (y: i64) = x Intrinsics.% y

  fun fromInt(x: i32) = i64 x

  fun (x: i64) == (y: i64) = x Intrinsics.== y
  fun (x: i64) < (y: i64) = x Intrinsics.< y
  fun (x: i64) > (y: i64) = x Intrinsics.> y

  fun sgn (x: i64) = Intrinsics.sgn x
  fun abs (x: i64) = Intrinsics.abs x
}

module U8 = {
  type t = u8

  fun (x: u8) + (y: u8) = x Intrinsics.+ y
  fun (x: u8) - (y: u8) = x Intrinsics.- y
  fun (x: u8) * (y: u8) = x Intrinsics.* y
  fun (x: u8) / (y: u8) = x Intrinsics./ y
  fun (x: u8) % (y: u8) = x Intrinsics.% y

  fun fromInt(x: i32) = u8 x

  fun (x: u8) == (y: u8) = x Intrinsics.== y
  fun (x: u8) < (y: u8) = x Intrinsics.< y
  fun (x: u8) > (y: u8) = x Intrinsics.> y

  fun sgn (x: u8) = Intrinsics.sgn x
  fun abs (x: u8) = Intrinsics.abs x
}

module U16 = {
  type t = u16

  fun (x: u16) + (y: u16) = x Intrinsics.+ y
  fun (x: u16) - (y: u16) = x Intrinsics.- y
  fun (x: u16) * (y: u16) = x Intrinsics.* y
  fun (x: u16) / (y: u16) = x Intrinsics./ y
  fun (x: u16) % (y: u16) = x Intrinsics.% y

  fun fromInt(x: i32) = u16 x

  fun (x: u16) == (y: u16) = x Intrinsics.== y
  fun (x: u16) < (y: u16) = x Intrinsics.< y
  fun (x: u16) > (y: u16) = x Intrinsics.> y

  fun sgn (x: u16) = Intrinsics.sgn x
  fun abs (x: u16) = Intrinsics.abs x
}

module U32 = {
  type t = u32

  fun (x: u32) + (y: u32) = x Intrinsics.+ y
  fun (x: u32) - (y: u32) = x Intrinsics.- y
  fun (x: u32) * (y: u32) = x Intrinsics.* y
  fun (x: u32) / (y: u32) = x Intrinsics./ y
  fun (x: u32) % (y: u32) = x Intrinsics.% y

  fun fromInt(x: i32) = u32 x

  fun (x: u32) == (y: u32) = x Intrinsics.== y
  fun (x: u32) < (y: u32) = x Intrinsics.< y
  fun (x: u32) > (y: u32) = x Intrinsics.> y

  fun sgn (x: u32) = Intrinsics.sgn x
  fun abs (x: u32) = Intrinsics.abs x
}

module U64 = {
  type t = u64

  fun (x: u64) + (y: u64) = x Intrinsics.+ y
  fun (x: u64) - (y: u64) = x Intrinsics.- y
  fun (x: u64) * (y: u64) = x Intrinsics.* y
  fun (x: u64) / (y: u64) = x Intrinsics./ y
  fun (x: u64) % (y: u64) = x Intrinsics.% y

  fun fromInt(x: i32) = u64 x

  fun (x: u64) == (y: u64) = x Intrinsics.== y
  fun (x: u64) < (y: u64) = x Intrinsics.< y
  fun (x: u64) > (y: u64) = x Intrinsics.> y

  fun sgn (x: u64) = Intrinsics.sgn x
  fun abs (x: u64) = Intrinsics.abs x
}

module F32 = {
  type t = f32

  fun (x: f32) + (y: f32) = x Intrinsics.+ y
  fun (x: f32) - (y: f32) = x Intrinsics.- y
  fun (x: f32) * (y: f32) = x Intrinsics.* y
  fun (x: f32) / (y: f32) = x Intrinsics./ y

  fun fromInt (x: i32) = f32 x
  fun fromFraction (x: i32) (y: i32) = f32 x Intrinsics./ f32 y
  fun toInt (x: f32) = i32 x

  fun (x: f32) == (y: f32) = x Intrinsics.== y
  fun (x: f32) <  (y: f32) = x Intrinsics.< y
  fun (x: f32) >  (y: f32) = x Intrinsics.> y

  fun sgn (x: f32) = if      x Intrinsics.< 0f32  then -1
                     else if x Intrinsics.== 0f32 then  0
                     else                               1
  fun abs (x: f32) = Intrinsics.abs x

  fun sqrt (x: f32) = Intrinsics.sqrt32 x

  fun log (x: f32) = Intrinsics.log32 x
  fun exp (x: f32) = Intrinsics.exp32 x
  fun cos (x: f32) = Intrinsics.cos32 x
  fun sin (x: f32) = Intrinsics.sin32 x
  fun acos (x: f32) = Intrinsics.acos32 x
  fun asin (x: f32) = Intrinsics.asin32 x
  fun atan (x: f32) = Intrinsics.atan32 x
  fun atan2 (x: f32) (y: f32) = Intrinsics.atan2_32 x y

  fun isinf (x: f32) = Intrinsics.isinf32 x
  fun isnan (x: f32) = Intrinsics.isnan32 x
}

module F64 = {
  type t = f64

  fun (x: f64) + (y: f64) = x Intrinsics.+ y
  fun (x: f64) - (y: f64) = x Intrinsics.- y
  fun (x: f64) * (y: f64) = x Intrinsics.* y
  fun (x: f64) / (y: f64) = x Intrinsics./ y

  fun fromInt (x: i64) = f64 x
  fun fromFraction (x: i64) (y: i64) = f64 x Intrinsics./ f64 y
  fun toInt (x: f64) = i64 x

  fun (x: f64) == (y: f64) = x Intrinsics.== y
  fun (x: f64) <  (y: f64) = x Intrinsics.< y
  fun (x: f64) >  (y: f64) = x Intrinsics.> y

  fun sgn (x: f64) = if      x Intrinsics.< 0f64  then -1
                     else if x Intrinsics.== 0f64 then  0
                     else                               1
  fun abs (x: f64) = Intrinsics.abs x

  fun sqrt (x: f64) = Intrinsics.sqrt64 x

  fun log (x: f64) = Intrinsics.log64 x
  fun exp (x: f64) = Intrinsics.exp64 x
  fun cos (x: f64) = Intrinsics.cos64 x
  fun sin (x: f64) = Intrinsics.sin64 x
  fun acos (x: f64) = Intrinsics.acos64 x
  fun asin (x: f64) = Intrinsics.asin64 x
  fun atan (x: f64) = Intrinsics.atan64 x
  fun atan2 (x: f64) (y: f64) = Intrinsics.atan2_64 x y

  fun isinf (x: f64) = Intrinsics.isinf64 x
  fun isnan (x: f64) = Intrinsics.isnan64 x
}
