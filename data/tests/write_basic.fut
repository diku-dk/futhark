-- Test that write works in its most simple use.
-- ==
-- tags { notravis }
--
-- input {
--   [0, 2, -1]
--   [9, 7, 0]
--   [3, 4, 5]
-- }
-- output {
--   [9, 4, 7]
-- }

fun [i32, n]
  main([i32, n] indexes,
       [i32, n] values,
       [i32, n] array) =
  write(indexes, values, array)
