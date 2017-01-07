-- Basic signature.

module type S {
type t = (int, int)

val x: t
val f: []t -> t
}

fun main(): S.t = (0,0)
