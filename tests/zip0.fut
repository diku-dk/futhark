-- Basic test that zip doesn't totally mess up everything.
-- ==
-- input {
--   [1,2,3]
--   [4,5,6]
-- }
-- output {
--   [1, 2, 3]
--   [4, 5, 6]
-- }
fun main(a: []int, b: []int): ([]int,[]int) =
  unzip(zip a b)
