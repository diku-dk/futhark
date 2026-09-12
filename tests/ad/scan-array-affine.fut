-- Composition of affine functions: associative, noncommutative, and coupled.
-- Prefixes satisfy A_i = a_i*A_(i-1), B_i = a_i*B_(i-1)+b_i.
-- Expected derivatives are from these recurrences, not another AD mode.
-- ==
-- tags { autodiff }
-- entry: main
-- input { [[2f64, 3], [5f64, 7], [11f64, 13]]
--         [[1f64, 1], [1f64, 1], [1f64, 1]] }
-- output { [[61f64, 61], [60f64, 12], [32f64, 1]] }
-- input { [[2f64, 3], [5f64, 7], [11f64, 13]]
--         [[2f64, -1], [-3f64, 4], [5f64, -2]] }
-- output { [[262f64, -91], [50f64, -18], [6f64, -2]] }
-- input { [[2f64, 3], [0f64, 7], [11f64, 13]]
--         [[2f64, -1], [-3f64, 4], [5f64, -2]] }
-- output { [[2f64, -1], [50f64, -18], [-14f64, -2]] }
-- input { [[2f64, 3]] [[4f64, 5]] }
-- output { [[4f64, 5]] }
-- input { empty([0][2]f64) empty([0][2]f64) }
-- output { empty([0][2]f64) }

entry main [n] (xs: [n][2]f64) (adj: [n][2]f64) =
  vjp (scan (\x y -> [y[0] * x[0], y[0] * x[1] + y[1]]) [1, 0])
      xs adj
