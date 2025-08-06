-- Test with i32.min/u32.min.
-- ==
--
-- input  {
--   5i64
--   [0, 1, 2, 3, 4]
--   [1, -1, 1, 1, 1]
-- }
-- output {
--   [0, -1, 0, 0, 0]
--   [0, 0, 0, 0, 0]
-- }
--
-- input  {
--   5i64
--   [0, 0, 0, 0, 0]
--   [6, 1, 4, 5, -1]
-- }
-- output {
--   [-1, 0, 0, 0, 0]
--   [0, 0, 0, 0, 0]
-- }
--
-- input  {
--   5i64
--   [1, 2, 1, 4, 5]
--   [1, 1, 4, 4, 4]
-- }
-- output {
--   [0, 0, 0, 0, 0]
--   [0, 0, 0, 0, 0]
-- }

def main [m] (n: i64) (is: [m]i32) (image: [m]i32) : ([n]i32, [n]i32) =
  ( reduce_by_index (replicate n 0) i32.min i32.highest (map i64.i32 is) image
  , map i32.u32
        (reduce_by_index (replicate n 0)
                         u32.min
                         u32.highest
                         (map i64.i32 is)
                         (map u32.i32 image))
  )
