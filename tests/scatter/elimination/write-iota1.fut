-- Test that multiple iotas can be eliminated in a write.
-- ==
-- input {
--   4i64
--   [5i64, 10i64, 15i64, 20i64, 25i64, 30i64]
-- }
-- output {
--   [0i64, 1i64, 2i64, 3i64, 25i64, 30i64]
-- }
-- structure { Screma 1 }

def main [n] (k: i64) (array: *[n]i64) : [n]i64 =
  scatter array (iota k) (iota k)
