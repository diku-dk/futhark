module type NUMERIC {
  type t

  val add: t -> t -> t
  val sub: t -> t -> t
  val mul: t -> t -> t
  val div: t -> t -> t

  val fromInt: i32 -> t

  val eq: t -> t -> bool
  val lt: t -> t -> bool
  val gt: t -> t -> bool

  val sgn: t -> i32
}

module type INTEGRAL {
  type t

  val add: t -> t -> t
  val sub: t -> t -> t
  val mul: t -> t -> t
  val div: t -> t -> t
  val mod: t -> t -> t

  val fromInt: i32 -> t

  val eq: t -> t -> bool
  val lt: t -> t -> bool
  val gt: t -> t -> bool

  val sgn: t -> i32
}

module type REAL {
  type t

  val add: t -> t -> t
  val sub: t -> t -> t
  val mul: t -> t -> t
  val div: t -> t -> t

  val fromInt: i32 -> t
  val fromFraction: i32 -> i32 -> t
  val toInt: t -> i32

  val eq: t -> t -> bool
  val lt: t -> t -> bool
  val gt: t -> t -> bool

  val sgn: t -> i32

  val sqrt: t -> t
}

module I32 {
  type t = i32

  fun add (x: i32) (y: i32) = x + y
  fun sub  (x: i32) (y: i32) = x - y
  fun mul (x: i32) (y: i32) = x * y
  fun div (x: i32) (y: i32) = x / y
  fun mod (x: i32) (y: i32) = x % y

  fun fromInt(x: i32) = x

  fun eq (x: i32) (y: i32) = x == y
  fun lt (x: i32) (y: i32) = x < y
  fun gt (x: i32) (y: i32) = x > y

  fun sgn (x: i32) = signum x
}

module F32 {
  type t = f32

  fun add (x: f32) (y: f32) = x + y
  fun sub  (x: f32) (y: f32) = x - y
  fun mul (x: f32) (y: f32) = x * y
  fun div (x: f32) (y: f32) = x / y

  fun fromInt (x: i32) = f32 x
  fun fromInts (x: i32) (y: i32) = f32 x / f32 y
  fun toInt (x: f32) = i32 x

  fun eq (x: f32) (y: f32) = x == y
  fun lt (x: f32) (y: f32) = x < y
  fun gt (x: f32) (y: f32) = x > y

  fun sgn (x: f32) = if      x < 0f32  then -1
                     else if x == 0f32 then  0
                     else                    1

  fun sqrt (x: f32) = sqrt32 x
}
