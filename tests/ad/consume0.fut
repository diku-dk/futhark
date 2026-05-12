
-- ==
-- tags { autodiff }
-- entry: rev fwd
-- input { [1.0,2.0,3.0] }
-- output { [[0.0, 0.0, 0.0], [0.0, 1.0, 0.0], [0.0, 0.0, 1.0]] }

def f (xs: []f64) =
  copy xs with [0] = 0

entry fwd [n] (xs: *[n]f64) =
  #[unsafe]
  tabulate n (\i -> jvp f xs (replicate n 0 with [i] = 1))

entry rev [n] (xs: *[n]f64) =
  #[unsafe]
  tabulate n (\i -> vjp f xs (replicate n 0 with [i] = 1))
