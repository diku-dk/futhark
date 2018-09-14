-- Can we do operations on f32s, even though these are not natively supported?
-- ==
--
-- input  {
--   [0f32, 0f32, 0f32, 0f32, 0f32]
--   [1, 1, 1, 1, 1]
-- }
-- output {
--   [0f32, 5f32, 0f32, 0f32, 0f32]
-- }
--
-- input  {
--   [0f32, 0f32, 0f32, 0f32, 0f32]
--   [1, 1, 4, 4, 4]
-- }
-- output {
--   [0f32, 2f32, 0f32, 0f32, 12f32]
-- }
--
-- input  {
--   [1f32, 2f32, 3f32, 4f32, 5f32]
--   [1, 1, 4, 4, 4]
-- }
-- output {
--   [1f32, 4f32, 3f32, 4f32, 17f32]
-- }

let main [m][n] (hist : *[n]f32) (image : [m]i32) : [n]f32 =
  reduce_by_index hist (+) 0f32 image (map r32 image)
