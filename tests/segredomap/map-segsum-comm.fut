-- a 3D version of 'segsum-comm' - we compute a sum over the inner arrays of a
-- 3D-array

fun segsum (xss : [m][n]f32): [m]f32 =
  map (\xs -> reduceComm (+) 0.0f32 xs) xss

fun main (xsss : [l][m][n]f32): [l][m]f32 =
  map segsum xsss
