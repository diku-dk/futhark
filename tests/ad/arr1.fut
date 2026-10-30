def primal (x, y) : [2]f32 = [x + y, x * y]

-- ==
-- tags { autodiff }
-- entry: fwd fwd_vec
-- input { 5.0f32 7.0f32 }
-- output { [[1.0f32,7.0f32], [1.0f32, 5.0f32]] }

entry fwd x y =
  [ jvp primal (x, y) (1, 0)
  , jvp primal (x, y) (0, 1)
  ]

entry fwd_vec x y =
  jmp primal (x, y) [(1, 0), (0, 1)]
