-- Signature matching with a single abstract type.
-- ==
-- input { [1,2,3] [4,5,6] }
-- output { 6 15 }

sig SIG {
type t

val inject: int -> int -> t
val extract: t -> (int,int)
val f: []t -> t
}

struct Struct: SIG {
type t = (int,int)

val x: (int, int) = (2,2)

fun inject (x: int) (y: int): t = (x, y)
fun extract (v:t): t = v
fun f (as: []t): t = reduce (fn (a,b) (c,d) => (a+c,b+d)) (0,0) as
}

fun main(xs: []int, ys: []int): (int,int) =
  Struct.extract (Struct.f (map Struct.inject xs ys))
