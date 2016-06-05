-- Test that multiple iotas can be eliminated in a write.
-- ==
-- input {
--   4
--   [5, 10, 15, 20, 25, 30]
-- }
-- output {
--   [0, 1, 2, 3, 25, 30]
-- }
-- structure { Write 1 }

fun [i32, n]
  main(i32 k, *[i32, n] array) =
  write(iota(k), iota(k), array)
