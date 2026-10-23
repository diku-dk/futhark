
-- ==
-- tags { autodiff }
-- entry: rev fwd fwd_vec rev_vec
-- input { true [1.0f32,2.0f32,3.0f32] }
-- output { [[0.0f32, 0.0f32, 0.0f32], [0.0f32, 1.0f32, 0.0f32], [0.0f32, 0.0f32, 1.0f32]] }

def f b (xs: []f32) =
  let ys = copy xs
  in if b then ys with [0] = 0 else ys

entry fwd [n] b (xs: *[n]f32) =
  #[unsafe]
  tabulate n (\i -> jvp (f b) xs (replicate n 0 with [i] = 1))

entry rev [n] b (xs: *[n]f32) =
  #[unsafe]
  tabulate n (\i -> vjp (f b) xs (replicate n 0 with [i] = 1))

entry fwd_vec [n] b (xs: *[n]f32) =
  #[unsafe]
  let seeds = tabulate n (\i -> replicate n 0 with [i] = 1)
  in jmp (f b) xs seeds

entry rev_vec [n] b (xs: *[n]f32) =
  #[unsafe]
  let seeds = tabulate n (\i -> replicate n 0 with [i] = 1)
  in mjp (f b) xs seeds
