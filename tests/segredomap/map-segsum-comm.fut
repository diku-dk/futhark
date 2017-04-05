-- a 3D version of 'segsum-comm' - we compute a sum over the inner arrays of a
-- 3D-array

let segsum (xss : [#m][#n]f32): [m]f32 =
  map (\xs -> reduce_comm (+) 0.0f32 xs) xss

let main (xsss : [#l][#m][#n]f32): [l][m]f32 =
  map segsum xsss
