-- An empty array can still have specific inner dimensions.
--
-- ==
-- input { 0 0 } output { [0i32,0i32,0i32] }
-- input { 2 3 } output { [0i32,2i32,3i32] }

fun main(n: i32, m: i32): []i32 =
  shape (empty([n][m]i32))
