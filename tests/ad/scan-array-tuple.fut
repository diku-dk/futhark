-- The scalar comes first; the array has a different element type.
-- Scalar products have derivatives [1+5+5*11, 2+2*11, 2*5].
-- The array components compose affine functions as in scan-array-affine.
-- ==
-- tags { autodiff }
-- entry: main
-- input { [2f32, 5, 11] [[2f64, 3], [5f64, 7], [11f64, 13]]
--         [1f32, 1, 1] [[1f64, 1], [1f64, 1], [1f64, 1]] }
-- output { [61f32, 24, 10] [[61f64, 61], [60f64, 12], [32f64, 1]] }
-- input { [2f32] [[2f64, 3]] [4f32] [[4f64, 5]] }
-- output { [4f32] [[4f64, 5]] }
-- input { empty([0]f32) empty([0][2]f64)
--         empty([0]f32) empty([0][2]f64) }
-- output { empty([0]f32) empty([0][2]f64) }

entry main [n] (as: [n]f32) (xs: [n][2]f64)
               (da: [n]f32) (dx: [n][2]f64) =
  let primal (bs, ys) =
    scan (\(a, x) (b, y) ->
            (a * b, [y[0] * x[0], y[0] * x[1] + y[1]]))
         (1f32, [1f64, 0]) (zip bs ys)
    |> unzip
  in vjp primal (as, xs) (da, dx)
