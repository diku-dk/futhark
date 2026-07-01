-- Scan with multiplication.
-- generic case
-- ==
-- tags { autodiff }
-- entry: fwd_J rev_J fwd_vec_J rev_vec_J
-- input { [1.0f32, 2.0f32, 3.0f32, 4.0f32, 5.0f32] }
-- output { [[1.0f32, 0.0f32, 0.0f32, 0.0f32, 0.0f32],
--           [2.0f32, 1.0f32, 0.0f32, 0.0f32, 0.0f32],
--           [6.0f32, 3.0f32, 2.0f32, 0.0f32, 0.0f32],
--           [24.0f32, 12.0f32, 8.0f32, 6.0f32, 0.0f32],
--           [120.0f32, 60.0f32, 40.0f32, 30.0f32, 24.0f32]]
--        }

entry fwd_J [n] (a: [n]f32) =
  tabulate n (\i -> jvp (scan (*) 1) a (replicate n 0 with [i] = 1))
  |> transpose

entry rev_J [n] (a: [n]f32) =
  tabulate n (\i -> vjp (scan (*) 1) a (replicate n 0 with [i] = 1))

entry fwd_vec_J [n] (a: [n]f32) =
  let seeds = tabulate n (\i -> replicate n 0 with [i] = 1)
  in jmp (scan (*) 1) a seeds |> transpose

entry rev_vec_J [n] (a: [n]f32) =
  let seeds = tabulate n (\i -> replicate n 0 with [i] = 1)
  in mjp (scan (*) 1) a seeds
