-- Reduce with addition.
-- ==
-- tags { autodiff }
-- entry: fwd_map fwd_vec
-- input { [1.0f32, 2.0f32, 3.0f32, 4.0f32, 5.0f32] }
-- output { [1.0f32, 1.0, 1.0, 1.0, 1.0] }

entry fwd_map [n] (a: [n]f32) =
  tabulate n (\i -> jvp (reduce (+) 0) a (replicate n 0 with [i] = 1))

entry fwd_vec [n] (a: [n]f32) =
  let seeds = tabulate n (\i -> replicate n 0 with [i] = 1)
  in jvp_vec (reduce (+) 0) a seeds

entry rev_vec [n] (a: [n]f32) =
  head (vjp_vec (reduce (+) 0) a [1])
