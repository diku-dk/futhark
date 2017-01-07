-- ==
-- input {
--   [[1,2,3],[4,5,6]]
-- }
-- output {
--   [[1, 4], [2, 5], [3, 6]]
-- }
fun main(a: [][]int): [][]int =
  transpose(a)
