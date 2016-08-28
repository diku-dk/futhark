-- ==
-- input {
--   [[1,2,3], [4,5,6], [7,8,9]]
--   [6,5,4,3,2,1]
--   1
-- }
-- output {
--   [[1,2,3], [6,5,4], [7,8,9]]
-- }
fun main(a: *[][n]int, b: []int, i: int): [][]int =
  let (br, _) = split (n) b in
  let a[i] = br in
  a
