-- A simple example of a redomap within a map, that uses different types for the
-- different parts of the redomap
-- ==
-- input {
--   [[1.0f32, 2.0f32, 3.0f32], [4.0f32, 5.0f32, 6.0f32]]
-- }
-- output {
--   [6i64, 15i64]
--   [[-1.000000f64, -2.000000f64, -3.000000f64], [-4.000000f64, -5.000000f64, -6.000000f64]]
-- }
def main [m] [n] (xss: [m][n]f32) : ([m]i64, [m][n]f64) =
  unzip (map (\(xs: [n]f32) : (i64, [n]f64) ->
                let (xs_int, xs_neg) = unzip (map (\x -> (i64.f32 x, f64.f32 (-x))) xs)
                in (reduce_comm (+) 0 xs_int, xs_neg))
             xss)
