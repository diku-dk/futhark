-- Test that multiple iotas with different start values can be eliminated in a
-- write.
-- ==
-- input {
--   5
--   [5, 10, 15, 20, 25, 30]
-- }
-- output {
--   [-9, -8, -7, -6, -5, 30]
-- }
-- structure { Scatter 1 }

let main [n] (k: i32, array: *[n]i32): [n]i32 =
  scatter array (iota k) (map (-9) (iota k))
