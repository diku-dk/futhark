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

  fun sgn (x: i32) = signum x
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

  fun sqrt (x: f32) = sqrt32 x
}
