
-- ==
-- tags { autodiff }
-- entry: rev fwd fwd_vec rev_vec
-- input { [1.0f32,2.0f32,3.0f32] }
-- output { [[0.0f32, 0.0f32, 0.0f32], [0.0f32, 1.0f32, 0.0f32], [0.0f32, 0.0f32, 1.0f32]] }

def f (xs: []f32) =
  copy xs with [0] = 0

entry fwd [n] (xs: *[n]f32) =
  #[unsafe]
  tabulate n (\i -> jvp f xs (replicate n 0 with [i] = 1))

entry rev [n] (xs: *[n]f32) =
  #[unsafe]
  tabulate n (\i -> vjp f xs (replicate n 0 with [i] = 1))

entry fwd_vec [n] (xs: *[n]f32) =
  #[unsafe]
  let seeds = tabulate n (\i -> replicate n 0 with [i] = 1)
  in jmp f xs seeds

entry rev_vec [n] (xs: *[n]f32) =
  #[unsafe]
  let seeds = tabulate n (\i -> replicate n 0 with [i] = 1)
  in mjp f xs seeds
