-- Test that an iota can be eliminated in a write.  Contrived example.
-- ==
-- input {
--   [100, 200, 300]
--   [5, 10, 15, 20, 25, 30]
-- }
-- output {
--   [100, 200, 300, 20, 25, 30]
-- }
-- structure { Write 1 }

fun [n]i32
  main([k]i32 values,
       *[n]i32 array) =
  write(iota(k), values, array)
