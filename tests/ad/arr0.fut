-- ==
-- tags { autodiff }

def f (xs: [2]f64) = xs[0] * xs[1]

-- ==
-- entry: f_jvp
-- input { [5.0, 7.0] }
-- output { 7.0 5.0 }

entry f_jvp xs =
  ( jvp f xs [1, 0]
  , jvp f xs [0, 1]
  )

-- ==
-- entry: f_vjp
-- input { [5.0, 7.0] }
-- output { [7.0, 5.0] }

entry f_vjp xs =
  vjp f xs 1
