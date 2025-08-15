-- Simple scatter, differentiating wrt. target.
-- ==
-- entry: fwd fwd_vec
-- input { [0f32, 0f32, 0f32, 0f32] [0i64, 1i64] [1f32, 2f32] }
-- output {
--  [[0.000000f32, 0.000000f32, 0.000000f32, 0.000000f32],
--   [0.000000f32, 0.000000f32, 0.000000f32, 0.000000f32],
--   [0.000000f32, 0.000000f32, 1.000000f32, 0.000000f32],
--   [0.000000f32, 0.000000f32, 0.000000f32, 1.000000f32]]
-- }

def f [n] [k] (xs: [k]f32) (is: [n]i64) (vs: [n]f32) =
  scatter (copy xs) is vs

entry fwd [n] [k] (xs: [k]f32) (is: [n]i64) (vs: [n]f32) =
  let g i = jvp (\xs -> f xs is vs) xs (replicate k 0 with [i] = 1)
  in tabulate k g

entry fwd_vec [n] [k] (xs: [k]f32) (is: [n]i64) (vs: [n]f32) =
  let seeds =
    map (\i -> map (\j -> f32.bool (i == j)) (indices xs)) (indices xs)
  in jvp_vec (\xs -> f xs is vs) xs seeds
