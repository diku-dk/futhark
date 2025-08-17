def primal (x, y) : [2]f64 = [x + y, x * y]

-- ==
-- tags { autodiff }
-- entry: fwd fwd_vec
-- input { 5.0 7.0 }
-- output { [[1.0,7.0], [1.0, 5.0]] }

entry fwd x y =
  [ jvp primal (x, y) (1, 0)
  , jvp primal (x, y) (0, 1)
  ]

entry fwd_vec x y =
  jvp_vec primal (x, y) [(1, 0), (0, 1)]
