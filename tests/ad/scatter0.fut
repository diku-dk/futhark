-- Simple scatter, differentiating wrt. values.
-- ==
-- tags { autodiff }
-- entry: fwd rev fwd_vec rev_vec
-- input { [0f32, 0f32, 0f32, 0f32] [0i64, 1i64, 2i64, 3i64] [1f32, 2f32, 3f32, 0f32] }
-- output {
--  [[1.000000f32, 0.000000f32, 0.000000f32, 0.000000f32],
--   [0.000000f32, 1.000000f32, 0.000000f32, 0.000000f32],
--   [0.000000f32, 0.000000f32, 1.000000f32, 0.000000f32],
--   [0.000000f32, 0.000000f32, 0.000000f32, 1.000000f32]]
-- }
-- input { [0f32, 0f32, 0f32, 0f32] [0i64, 1i64, 2i64, -100000i64] [1f32, 2f32, 3f32, 0f32] }
-- output {
--  [[1.0f32, 0.0f32, 0.0f32, 0.0f32],
--   [0.0f32, 1.0f32, 0.0f32, 0.0f32],
--   [0.0f32, 0.0f32, 1.0f32, 0.0f32],
--   [0.0f32, 0.0f32, 0.0f32, 0.0f32]]
-- }

def f [n] [k] (xs: [k]f32) (is: [n]i64) (vs: [n]f32) =
  scatter (copy xs) is vs

entry fwd [n] [k] (xs: [k]f32) (is: [n]i64) (vs: [n]f32) =
  let g i = jvp (\vs -> f xs is vs) vs (replicate n 0 with [i] = 1)
  in tabulate n g

entry rev [n] [k] (xs: [k]f32) (is: [n]i64) (vs: [n]f32) =
  let g i = vjp (\vs -> f xs is vs) vs (replicate k 0 with [i] = 1)
  in tabulate n g

entry fwd_vec [n] [k] (xs: [k]f32) (is: [n]i64) (vs: [n]f32) =
  let seeds = tabulate n (\i -> replicate n 0 with [i] = 1)
  in jmp (\vs -> f xs is vs) vs seeds

entry rev_vec [n] [k] (xs: [k]f32) (is: [n]i64) (vs: [n]f32) =
  let seeds = tabulate n (\i -> replicate k 0 with [i] = 1)
  in mjp (\vs -> f xs is vs) vs seeds
