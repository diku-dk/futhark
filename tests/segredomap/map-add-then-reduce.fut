-- a 3D version of 'add-then-reduce.fut'
--
-- As before, we will get a `map (map (redomap) )` expression, which can be
-- turned into a single segmented redomap kernel.
--
-- This example is interesting, because the value `y` is bound in the outermost
-- map, and special care must be taken to handle such a map-invariant variable.

def add_then_reduce [m] [n] (xss: [m][n]f32) (y: f32) : [m]f32 =
  let xss' = map (\xs -> map (y +) xs) xss
  in map (\xs -> reduce_comm (+) 0.0f32 xs) xss'

def main [l] [m] [n] (xsss: [l][m][n]f32, ys: [l]f32) : [l][m]f32 =
  map2 add_then_reduce xsss ys
