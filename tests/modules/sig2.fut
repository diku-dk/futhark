-- Signature referring to externally defined type.

type two_ints = (i32, i32)

module type S {
type t = two_ints

val x: t
val f: []t -> t
}

fun main(): S.t = (0,0)
