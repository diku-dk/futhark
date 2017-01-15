-- Ascription can happen anywhere in a module expression.
-- ==
-- input { 2 } output { [0,0] }

module type S { val f: int -> []int }

module M = {
  fun f(x: int): *[]int = replicate x 0
}: S

fun main(n: int): []int = M.f n
