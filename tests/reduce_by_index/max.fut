-- Test with i32.max/u32.max.
-- ==
--
-- input  {
--   5
--   [0, 1, 2, 3, 4]
--   [1, 1, 1, 1, 1]
-- }
-- output {
--   [1, 1, 1, 1, 1]
--   [1, 1, 1, 1, 1]
-- }
--
-- input  {
--   5
--   [0, 0, 0, 0, 0]
--   [6, 1, 4, 5, -1]
-- }
-- output {
--   [6, 0, 0, 0, 0]
--   [-1, 0, 0, 0, 0]
-- }
--
-- input  {
--   5
--   [1, 2, 1, 4, 5]
--   [1, 1, 4, 4, 4]
-- }
-- output {
--   [0, 4, 1, 0, 4]
--   [0, 4, 1, 0, 4]
-- }

let main [m] (n: i32) (is: [m]i32) (image: [m]i32) : ([n]i32, [n]i32) =
  (reduce_by_index (replicate n 0) i32.max i32.smallest is image,
   map i32.u32 (reduce_by_index (replicate n 0) u32.max u32.smallest is (map u32.i32 image)))
