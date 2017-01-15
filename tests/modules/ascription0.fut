-- Basic signature matching without abstract types.
-- ==
-- input { [1,2,3] [4,5,6] }
-- output { 6 15 }

module type SIG {
type t = (i32, i32)

val x: t
val f: []t -> t
}

module Struct: SIG {
type t = (i32,i32)

val x: (i32, i32) = (2,2)

fun f (as: []t): t = reduce (\(a,b) (c,d) -> (a+c,b+d)) (0,0) as
}

fun main(xs: []i32, ys: []i32): Struct.t = Struct.f (zip xs ys)
