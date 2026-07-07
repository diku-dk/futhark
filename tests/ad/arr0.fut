-- ==
-- tags { autodiff }

def primal (xs: [2]f64) = xs[0] * xs[1]

-- ==
-- entry: fwd fwd_vec
-- input { [5.0, 7.0] }
-- output { [7.0, 5.0] }

entry fwd xs =
  [ jvp primal xs [1, 0]
  , jvp primal xs [0, 1]
  ]

entry fwd_vec xs =
  jmp primal xs [[1, 0], [0, 1]]

-- ==
-- entry: rev
-- input { [5.0, 7.0] }
-- output { [7.0, 5.0] }

entry rev xs =
  vjp primal xs 1
