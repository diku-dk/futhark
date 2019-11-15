-- A simple example of a redomap within a map, that uses different types for the
-- different parts of the redomap
-- ==
-- input {
--   true
--   [[1.0f32, 2.0f32, 3.0f32], [4.0f32, 5.0f32, 6.0f32]]
-- }
-- output {
--   [6i32, 15i32]
--   [[-1.000000f64, -2.000000f64, -3.000000f64], [-4.000000f64, -5.000000f64, -6.000000f64]]
-- }

-- Add a data-driven branch to prevent the compiler from noticing that
-- this is commutative.
let add (b: bool) (x : i32) (y : i32): i32 = if b then x + y else x - y

let main [m][n] (b: bool) (xss : [m][n]f32): ([m]i32, [m][n]f64) =
  unzip (map( \(xs : [n]f32) : (i32, [n]f64) ->
         let (xs_int, xs_neg) = unzip (map(\x -> (t32 x, f64.f32(-x))) xs)
         in (reduce (add b) 0 xs_int, xs_neg)
     ) xss)
