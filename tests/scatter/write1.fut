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

let main [k][m][n] (indexes: [k]i32) (values: [k][m]f32) (array: *[n][m]f32): [n][m]f32 =
  scatter array indexes values
