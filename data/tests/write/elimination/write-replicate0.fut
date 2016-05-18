-- Test that a replicate can be eliminated in a write.
-- ==
-- input {
--   [0, 3, 1]
--   [9, 8, -3, 90, 41]
-- }
-- output {
--   [5, 5, -3, 5, 41]
-- }
-- structure { Write 1 }

fun [i32, n]
  main([i32, k] indexes,
       *[i32, n] array) =
  write(indexes, replicate(k, 5), array)
