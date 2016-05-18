-- Test that write works in non-trivial cases.
-- ==
--
-- input {
--   [1, -1]
--   [[5.0f32, 4.3f32], [0.0f32, 0.0f32]]
--   [[1.0f32, 1.2f32], [2.3f32, -11.6f32], [4.0f32, 44.2f32]]
-- }
-- output {
--   [[1.0f32, 1.2f32], [5.0f32, 4.3f32], [4.0f32, 44.2f32]]
-- }

fun [[f32, m], n]
  main([i32, k] indexes,
       [[f32, m], k] values,
       *[[f32, m], n] array) =
  write(indexes, values, array)
