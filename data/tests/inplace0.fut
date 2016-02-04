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

fun [[int]] main(int n, int i, int j, int x) =
  let a = replicate(n, replicate(n, 0)) in
  let b = replicate(n, 0) in
  let b[i] = x in
  let a[j] = b in
  a
