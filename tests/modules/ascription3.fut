-- Ascription only needs a subtype.
-- ==
-- input { 2 } output { [0,0] }

module type S { val f: int -> []int }

module M: S {
  fun f(x: int): *[]int = replicate x 0
}

fun main(n: int): []int = M.f n
