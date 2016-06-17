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

fun [n]i32
  main(i32 k, *[n]i32 array) =
  write(iota(k), map(-9, iota(k)), array)
