-- Simplest possible in-place operation.
-- ==
-- input {
--   [1,2,3,4]
--   2
--   10
-- }
-- output {
--   [1,2,10,4]
-- }
fun main(a: *[]int, i: int, x: int): []int =
  let a[i] = x in
  a
