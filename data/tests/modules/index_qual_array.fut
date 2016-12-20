-- ==
-- input { 4 } output { 5 }

struct M {
  val a: []int = [1,2,3]
}

fun main(x: int): int = M.a[0] + x
