-- Test that multiple iotas can be eliminated in a write.
-- ==
-- input {
--   4
--   [5, 10, 15, 20, 25, 30]
-- }
-- output {
--   [0, 1, 2, 3, 25, 30]
-- }
-- structure { Scatter 1 }

let main(k: i32, array: *[n]i32): [n]i32 =
  scatter array (iota k) (iota k)
