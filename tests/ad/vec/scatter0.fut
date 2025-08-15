-- Simple scatter, differentiating wrt. values.
-- ==
-- entry: fwd fwd_vec
-- input { [0f32, 0f32, 0f32, 0f32] [0i64, 1i64, 2i64, 3i64] [1f32, 2f32, 3f32, 0f32] }
-- output {
--  [[1.000000f32, 0.000000f32, 0.000000f32, 0.000000f32],
--   [0.000000f32, 1.000000f32, 0.000000f32, 0.000000f32],
--   [0.000000f32, 0.000000f32, 1.000000f32, 0.000000f32],
--   [0.000000f32, 0.000000f32, 0.000000f32, 1.000000f32]]
-- }

def f [n] [k] (xs: [k]f32) (is: [n]i64) (vs: [n]f32) =
  scatter (copy xs) is vs

entry fwd [n] [k] (xs: [k]f32) (is: [n]i64) (vs: [n]f32) =
  let g i = jvp (\vs -> f xs is vs) vs (replicate n 0 with [i] = 1)
  in tabulate n g

entry fwd_vec [n] [k] (xs: [k]f32) (is: [n]i64) (vs: [n]f32) =
  let seeds =
    map (\i -> map (\j -> f32.bool (i == j)) (indices vs)) (indices vs)
  in jvp_vec (\vs -> f xs is vs) vs seeds
