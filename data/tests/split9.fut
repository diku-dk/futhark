-- Checks that the results of splits are properly copied to where they
-- are supposed to go.
-- ==
-- input {
--   [[1,2,2,1], [3,4,5,4], [6,7,8,9]]
--   2
-- }
-- output {
--   [[1,2], [3,4], [6,7]]
-- }

fun take (n: int) (r: []int): []int =
  let (part, _) = split (n) r in
  part

fun main(rs: [][]int, n: int): [][]int =
  map(take(n), rs)
