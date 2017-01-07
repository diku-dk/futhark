-- An empty array can still have specific inner dimensions.
--
-- ==
-- input { 0 0 } output { [0i32,0i32,0i32] }
-- input { 2 3 } output { [0i32,2i32,3i32] }

fun main(n: int, m: int): []int =
  shape (empty([n][m]int))
