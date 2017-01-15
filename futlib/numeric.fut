module type NUMERIC {
  type t

  val add: t -> t -> t
  val sub: t -> t -> t
  val mul: t -> t -> t
  val div: t -> t -> t

  val fromInt: int -> t

  val eq: t -> t -> bool
  val lt: t -> t -> bool
  val gt: t -> t -> bool

  val sgn: t -> int
}

module type INTEGRAL {
  type t

  val add: t -> t -> t
  val sub: t -> t -> t
  val mul: t -> t -> t
  val div: t -> t -> t
  val mod: t -> t -> t

  val fromInt: int -> t

  val eq: t -> t -> bool
  val lt: t -> t -> bool
  val gt: t -> t -> bool

  val sgn: t -> int
}

module type REAL {
  type t

  val add: t -> t -> t
  val sub: t -> t -> t
  val mul: t -> t -> t
  val div: t -> t -> t

  val fromInt: int -> t
  val fromFraction: int -> int -> t
  val toInt: t -> int

  val eq: t -> t -> bool
  val lt: t -> t -> bool
  val gt: t -> t -> bool

  val sgn: t -> int

  val sqrt: t -> t
}

module I32 {
  type t = int

  fun add (x: int) (y: int) = x + y
  fun sub  (x: int) (y: int) = x - y
  fun mul (x: int) (y: int) = x * y
  fun div (x: int) (y: int) = x / y
  fun mod (x: int) (y: int) = x % y

  fun fromInt(x: int) = x

  fun eq (x: int) (y: int) = x == y
  fun lt (x: int) (y: int) = x < y
  fun gt (x: int) (y: int) = x > y

  fun sgn (x: int) = signum x
}

module F32 {
  type t = f32

  fun add (x: f32) (y: f32) = x + y
  fun sub  (x: f32) (y: f32) = x - y
  fun mul (x: f32) (y: f32) = x * y
  fun div (x: f32) (y: f32) = x / y

  fun fromInt (x: int) = f32 x
  fun fromInts (x: int) (y: int) = f32 x / f32 y
  fun toInt (x: f32) = int x

  fun eq (x: f32) (y: f32) = x == y
  fun lt (x: f32) (y: f32) = x < y
  fun gt (x: f32) (y: f32) = x > y

  fun sgn (x: f32) = if      x < 0f32  then -1
                     else if x == 0f32 then  0
                     else                    1

  fun sqrt (x: f32) = sqrt32 x
}
