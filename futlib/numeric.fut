module type NUMERIC {
type t

val add: t -> t -> t
val sub: t -> t -> t
val mul: t -> t -> t

val fromInt: int -> t

val eq: t -> t -> bool
val lt: t -> t -> bool
val gt: t -> t -> bool
}

module I32 {
type t = int

fun add (x: int) (y: int) = x + y
fun sub  (x: int) (y: int) = x - y
fun mul (x: int) (y: int) = x * y

fun fromInt(x: int) = x

fun eq (x: int) (y: int) = x == y
fun lt (x: int) (y: int) = x < y
fun gt (x: int) (y: int) = x > y
}

module F32 {
type t = f32

fun add (x: f32) (y: f32) = x + y
fun sub  (x: f32) (y: f32) = x - y
fun mul (x: f32) (y: f32) = x * y

fun fromInt(x: int) = f32 x

fun eq (x: f32) (y: f32) = x == y
fun lt (x: f32) (y: f32) = x < y
fun gt (x: f32) (y: f32) = x > y
}
