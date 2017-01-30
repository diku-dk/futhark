-- Abstract types must be abstract.
-- ==
-- error: type i32.*type t

module type SIG = {
type t

val inject: i32 -> t
val extract: t -> i32
}

module Struct: SIG = {
type t = i32

fun inject (x: i32): i32 = x
fun extract (x: i32): i32 = x
}

fun main(x: i32): i32 =
  Struct.inject x
