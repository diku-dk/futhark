-- Segmented sum
-- ==
-- input {
--   [[1.0f32, 2.0f32, 3.0f32], [4.0f32, 5.0f32, 6.0f32]]
-- }
-- output {
--   [6.0f32, 15.0f32]
-- }
def main [m] [n] (xss: [m][n]f32) : [m]f32 =
  map (\xs -> reduce_comm (+) 0.0f32 xs) xss
