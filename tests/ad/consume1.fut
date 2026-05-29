
-- ==
-- tags { autodiff }
-- entry: rev fwd fwd_vec rev_vec
-- input { true [1.0,2.0,3.0] }
-- output { [[0.0, 0.0, 0.0], [0.0, 1.0, 0.0], [0.0, 0.0, 1.0]] }

def f b (xs: []f64) =
  let ys = copy xs
  in if b then ys with [0] = 0 else ys

entry fwd [n] b (xs: *[n]f64) =
  #[unsafe]
  tabulate n (\i -> jvp (f b) xs (replicate n 0 with [i] = 1))

entry rev [n] b (xs: *[n]f64) =
  #[unsafe]
  tabulate n (\i -> vjp (f b) xs (replicate n 0 with [i] = 1))

entry fwd_vec [n] b (xs: *[n]f64) =
  #[unsafe]
  let seeds = tabulate n (\i -> replicate n 0 with [i] = 1)
  in jvp_vec (f b) xs seeds

entry rev_vec [n] b (xs: *[n]f64) =
  #[unsafe]
  let seeds = tabulate n (\i -> replicate n 0 with [i] = 1)
  in vjp_vec (f b) xs seeds
