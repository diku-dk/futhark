-- Basic signature matching without abstract types.
-- ==
-- input { [1,2,3] [4,5,6] }
-- output { 6 15 }

sig SIG {
type t = (int, int)

val x: t
val f: []t -> t
}

struct Struct: SIG {
type t = (int,int)

val x: (int, int) = (2,2)

fun f (as: []t): t = reduce (fn (a,b) (c,d) => (a+c,b+d)) (0,0) as
}

fun main(xs: []int, ys: []int): Struct.t = Struct.f (zip xs ys)
