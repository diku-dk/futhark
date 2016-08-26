-- ==
-- input { 8 }
-- output { [7, 0, 1, 2, 3, 4, 5, 6] }

fun main(i: int): []int =
  let a = iota(i) in
  rotate(-1, a)
