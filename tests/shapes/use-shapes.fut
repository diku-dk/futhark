-- Test that a variable shape annotation is actually bound.
-- ==
-- input {
--   [42i64,1337i64,5i64,4i64,3i64,2i64,1i64]
-- }
-- output {
--   [49i64,1344i64,12i64,11i64,10i64,9i64,8i64]
-- }

def main [n] (a: [n]i64) : []i64 =
  map (+ n) a
