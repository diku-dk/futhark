-- ==
-- tags { autodiff }
-- entry: rev fwd fwd_vec
-- input { [0.0f32, 2.0f32, 0.0f32, 4.0f32] } output { [0.0f32, 0.0f32, 0.0f32, 0.0f32] }

def red_mult [n] (xs: [n]f32) : f32 =
  reduce (*) 1 xs

entry rev [n] (xs: [n]f32) =
  vjp red_mult (xs) 1

entry fwd [n] (xs: [n]f32) =
  tabulate n (\i -> jvp red_mult xs (replicate n 0 with [i] = 1))

entry fwd_vec [n] (xs: [n]f32) =
  let seeds = tabulate n (\i -> replicate n 0 with [i] = 1)
  in jmp red_mult xs seeds
