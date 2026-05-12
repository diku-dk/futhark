-- Test with i8.min/u8.min.
-- ==
--
-- input  {
--   5i64
--   [0, 1, 2, 3, 4]
--   [1i8, -1i8, 1i8, 1i8, 1i8]
-- }
-- output {
--   [0i8, -1i8, 0i8, 0i8, 0i8]
--   [0i8, 0i8, 0i8, 0i8, 0i8]
-- }
--
-- input  {
--   5i64
--   [0, 0, 0, 0, 0]
--   [6i8, 1i8, 4i8, 5i8, -1i8]
-- }
-- output {
--   [-1i8, 0i8, 0i8, 0i8, 0i8]
--   [0i8, 0i8, 0i8, 0i8, 0i8]
-- }
--
-- input  {
--   5i64
--   [1, 2, 1, 4, 5]
--   [1i8, 1i8, 4i8, 4i8, 4i8]
-- }
-- output {
--   [0i8, 0i8, 0i8, 0i8, 0i8]
--   [0i8, 0i8, 0i8, 0i8, 0i8]
-- }

def main [m] (n: i64) (is: [m]i32) (image: [m]i8) : ([n]i8, [n]i8) =
  ( reduce_by_index (replicate n 0)
                    i8.min
                    i8.highest
                    (map i64.i32 is)
                    image
  , map i8.u8
        (reduce_by_index (replicate n 0)
                         u8.min
                         u8.highest
                         (map i64.i32 is)
                         (map u8.i8 image))
  )
