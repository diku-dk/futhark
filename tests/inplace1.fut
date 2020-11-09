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

let main [n][m] (a: *[m][n]i32) (i: i32) (v: i32): [][]i32 =
  let a[i] = replicate n v
  in a
