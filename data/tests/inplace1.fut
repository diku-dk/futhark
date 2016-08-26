-- Test an in-place update of an argument to main()
-- ==
-- input {
--   [[1],[2],[3],[4],[5]]
--   2
--   42
-- }
-- output {
--   [[1],[2],[42],[4],[5]]
-- }

fun main(a: *[][n]int, i: int, v: int): [][]int =
  let a[i] = replicate(n,v)
  in a
