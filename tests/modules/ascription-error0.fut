-- Abstract types must be abstract.
-- ==
-- error: type i32.*type t

module type SIG {
type t

val inject: int -> t
val extract: t -> int
}

module Struct: SIG {
type t = int

fun inject (x: int): int = x
fun extract (x: int): int = x
}

fun main(x: int): int =
  Struct.inject x
