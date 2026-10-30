-- ==
-- tags { autodiff }
-- entry: fwd rev fwd_vec
-- input { [1.0f32, 2.0f32, 3.0f32, 4.0f32, 5.0f32, 6.0f32] }
-- output { [5040.0f32, 2521.0f32, 1684.0f32, 1278.0f32, 1104.0f32, 1440.0f32] }

def primal [m] (x: [m]f32) =
  let muls = scan (*) 1 x
  in f32.sum (map2 (*) muls x)

entry rev [m] (x: [m]f32) =
  vjp (\x' -> primal x') x 1

entry fwd [m] (x: [m]f32) =
  tabulate m (\i -> jvp (\x' -> primal x') x (replicate m 0 with [i] = 1))

entry fwd_vec [m] (x: [m]f32) =
  let seeds = tabulate m (\i -> replicate m 0 with [i] = 1)
  in jmp (\x' -> primal x') x seeds
