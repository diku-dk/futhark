-- Test lowering of an in-place update.
-- ==
-- input {
--   3i64
--   1
--   2
--   42
-- }
-- output {
--   [[0,0,0], [0,0,0], [0,42,0]]
-- }

def main (n: i64) (i: i32) (j: i32) (x: i32) : [][]i32 =
  let a = replicate n (replicate n 0)
  let b = replicate n 0
  let b[i] = x
  let a[j] = b
  in a
