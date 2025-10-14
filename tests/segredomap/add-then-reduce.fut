-- Add y to all elements of all inner arrays in xss, then we sum all inner
-- arrays in xss.
--
-- This is an interesting example only because the two expressions will be fused
-- into a single segmented-redomap
def main [m] [n] (xss: [m][n]f32, y: f32) : [m]f32 =
  let xss' = map (\xs -> map (y +) xs) xss
  in map (\xs -> reduce_comm (+) 0.0f32 xs) xss'
