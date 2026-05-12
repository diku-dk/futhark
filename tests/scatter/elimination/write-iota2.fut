-- Test that multiple iotas with different start values can be eliminated in a
-- write.
-- ==
-- input {
--   5i64
--   [5, 10, 15, 20, 25, 30]
-- }
-- output {
--   [-9, -8, -7, -6, -5, 30]
-- }
-- structure { Screma 1 }

def main [n] (k: i64) (array: *[n]i32) : [n]i32 =
  scatter array (iota k) (map (\x -> i32.i64 x - 9) (iota k))
