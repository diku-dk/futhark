-- Test with i64.&.
-- ==
--
-- input  {
--   5i64
--   [0, 1, 2, 3, 4]
--   [1, 1, 1, 1, 1]
-- }
-- output {
--   [1i64, 1i64, 1i64, 1i64, 1i64]
-- }
--
-- input  {
--   5i64
--   [0, 0, 0, 0, 0]
--   [6, 1, 4, 5, -1]
-- }
-- output {
--   [0i64, -1i64, -1i64, -1i64, -1i64]
-- }
--
-- input  {
--   5i64
--   [1, 2, 1, 4, 5]
--   [1, 1, 4, 4, 4]
-- }
-- output {
--   [-1i64, 0i64, 1i64, -1i64, 4i64]
-- }

def main [m] (n: i64) (is: [m]i32) (image: [m]i32) : [n]i64 =
  reduce_by_index (replicate n (-1)) (i64.&) (-1)
                  (map i64.i32 is) (map i64.i32 image)
