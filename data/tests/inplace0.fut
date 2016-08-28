-- Test lowering of an in-place update.
-- ==
-- input {
--   3
--   1
--   2
--   42
-- }
-- output {
--   [[0,0,0], [0,0,0], [0,42,0]]
-- }

fun main(n: int, i: int, j: int, x: int): [][]int =
  let a = replicate n (replicate n 0) in
  let b = replicate n 0 in
  let b[i] = x in
  let a[j] = b in
  a
