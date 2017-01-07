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
-- structure { Write 1 }

fun main(k: i32, array: *[n]i32): [n]i32 =
  write (iota k) (map (-9) (iota k)) array
