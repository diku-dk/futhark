-- Test that write works in non-trivial cases.
-- ==
--
-- input {
--   [1, -1]
--   [[5.0, 4.3], [0.0, 0.0]]
--   [[1.0, 1.2], [2.3, -11.6], [4.0, 44.2]]
-- }
-- output {
--   [[1.0, 1.2], [5.0, 4.3], [4.0, 44.2]]
-- }

fun [[f64, m], n]
  main([i32, k] indexes,
       [[f64, m], k] values,
       *[[f64, m], n] array) =
  write(indexes, values, array)
