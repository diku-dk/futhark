-- ==
-- input { 8 }
-- output { [1, 2, 3, 4, 5, 6, 7, 0] }

fun main(i: int): []int =
  let a = iota(i)
  in rotate 1 a
