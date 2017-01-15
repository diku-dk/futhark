-- Basic signature.

module type S {
type t = (i32, i32)

val x: t
val f: []t -> t
}

fun main(): S.t = (0,0)
