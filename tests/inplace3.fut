-- In-place update without 'let'.
-- ==
-- input {
--   [[1],[2],[3],[4],[5]]
--   2
--   42
-- }
-- output {
--   [[1],[2],[42],[4],[5]]
-- }

let main [k][n] (a: *[k][n]i32) (i: i32) (v: i32): [][]i32 =
  a with [i] = replicate n v
