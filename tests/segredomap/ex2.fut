-- A simple example of a redomap within a map, that uses different types for the
-- different parts of the redomap
-- ==
-- input {
--   [ [ [1.0f32, 2.0f32, 3.0f32], [4.0f32, 5.0f32, 6.0f32] ]
--   , [ [1.0f32, 2.0f32, 3.0f32], [4.0f32, 5.0f32, 6.0f32] ]
--   ]
-- }
-- output {
--   [ [6i32, 15i32], [6i32, 15i32] ]
--   [ [ [-1.000000f64, -2.000000f64, -3.000000f64], [-4.000000f64, -5.000000f64, -6.000000f64] ]
--   , [ [-1.000000f64, -2.000000f64, -3.000000f64], [-4.000000f64, -5.000000f64, -6.000000f64] ]
--   ]
-- }
fun main (xsss : [l][m][n]f32): ([l][m]i32, [l][m][n]f64) =
  unzip (map (fn xss =>
         unzip (map( fn (xs : [n]f32) : (i32, [n]f64) =>
                       let (xs_int, xs_neg) = unzip (map( fn (x : f32) : (i32, f64) => (int x, f64(-x))) xs)
                       in (reduce (+) 0 xs_int, xs_neg)
                   ) xss)
       ) xsss)
