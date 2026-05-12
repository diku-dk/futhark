-- Test with i16.min/u16.min.
-- ==
--
-- input  {
--   5i64
--   [0, 1, 2, 3, 4]
--   [1i16, -1i16, 1i16, 1i16, 1i16]
-- }
-- output {
--   [0i16, -1i16, 0i16, 0i16, 0i16]
--   [0i16, 0i16, 0i16, 0i16, 0i16]
-- }
--
-- input  {
--   5i64
--   [0, 0, 0, 0, 0]
--   [6i16, 1i16, 4i16, 5i16, -1i16]
-- }
-- output {
--   [-1i16, 0i16, 0i16, 0i16, 0i16]
--   [0i16, 0i16, 0i16, 0i16, 0i16]
-- }
--
-- input  {
--   5i64
--   [1, 2, 1, 4, 5]
--   [1i16, 1i16, 4i16, 4i16, 4i16]
-- }
-- output {
--   [0i16, 0i16, 0i16, 0i16, 0i16]
--   [0i16, 0i16, 0i16, 0i16, 0i16]
-- }

def main [m] (n: i64) (is: [m]i32) (image: [m]i16) : ([n]i16, [n]i16) =
  ( reduce_by_index (replicate n 0)
                    i16.min
                    i16.highest
                    (map i64.i32 is)
                    image
  , map i16.u16
        (reduce_by_index (replicate n 0)
                         u16.min
                         u16.highest
                         (map i64.i32 is)
                         (map u16.i16 image))
  )
