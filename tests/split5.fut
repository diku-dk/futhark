-- Split with a multi-dimensional array.
-- ==
-- input {
--   2
--   [[4,3],[3,2],[2,1],[1,0]]
-- }
-- output {
--   [[4,3],[3,2]]
--   [[2,1],[1,0]]
-- }
fun main(n: i32, a: [][]i32): ([][]i32, [][]i32) =
  split (n) a
