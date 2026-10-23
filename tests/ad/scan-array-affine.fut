-- Composition of affine functions: associative, noncommutative, and coupled.
-- Prefixes satisfy A_i = a_i*A_(i-1), B_i = a_i*B_(i-1)+b_i.
-- Expected derivatives are from these recurrences, not another AD mode.
-- ==
-- tags { autodiff }
-- entry: main
-- input { [[2f32, 3], [5f32, 7], [11f32, 13]]
--         [[1f32, 1], [1f32, 1], [1f32, 1]] }
-- output { [[61f32, 61], [60f32, 12], [32f32, 1]] }
-- input { [[2f32, 3], [5f32, 7], [11f32, 13]]
--         [[2f32, -1], [-3f32, 4], [5f32, -2]] }
-- output { [[262f32, -91], [50f32, -18], [6f32, -2]] }
-- input { [[2f32, 3], [0f32, 7], [11f32, 13]]
--         [[2f32, -1], [-3f32, 4], [5f32, -2]] }
-- output { [[2f32, -1], [50f32, -18], [-14f32, -2]] }
-- input { [[2f32, 3]] [[4f32, 5]] }
-- output { [[4f32, 5]] }
-- input { empty([0][2]f32) empty([0][2]f32) }
-- output { empty([0][2]f32) }

entry main [n] (xs: [n][2]f32) (adj: [n][2]f32) =
  vjp (scan (\x y -> [y[0] * x[0], y[0] * x[1] + y[1]]) [1, 0])
      xs adj
