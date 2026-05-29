-- Simple scatter, differentiating wrt. values.
-- ==
-- tags { autodiff }
-- entry: fwd rev fwd_vec rev_vec
-- input { [0f64, 0f64, 0f64, 0f64] [0i64, 1i64, 2i64, 3i64] [1f64, 2f64, 3f64, 0f64] }
-- output {
--  [[1.000000f64, 0.000000f64, 0.000000f64, 0.000000f64],
--   [0.000000f64, 1.000000f64, 0.000000f64, 0.000000f64],
--   [0.000000f64, 0.000000f64, 1.000000f64, 0.000000f64],
--   [0.000000f64, 0.000000f64, 0.000000f64, 1.000000f64]]
-- }
-- input { [0f64, 0f64, 0f64, 0f64] [0i64, 1i64, 2i64, -100000i64] [1f64, 2f64, 3f64, 0f64] }
-- output {
--  [[1.0, 0.0, 0.0, 0.0],
--   [0.0, 1.0, 0.0, 0.0],
--   [0.0, 0.0, 1.0, 0.0],
--   [0.0, 0.0, 0.0, 0.0]]
-- }

def f [n] [k] (xs: [k]f64) (is: [n]i64) (vs: [n]f64) =
  scatter (copy xs) is vs

entry fwd [n] [k] (xs: [k]f64) (is: [n]i64) (vs: [n]f64) =
  let g i = jvp (\vs -> f xs is vs) vs (replicate n 0 with [i] = 1)
  in tabulate n g

entry rev [n] [k] (xs: [k]f64) (is: [n]i64) (vs: [n]f64) =
  let g i = vjp (\vs -> f xs is vs) vs (replicate k 0 with [i] = 1)
  in tabulate n g

entry fwd_vec [n] [k] (xs: [k]f64) (is: [n]i64) (vs: [n]f64) =
  let seeds = tabulate n (\i -> replicate n 0 with [i] = 1)
  in jvp_vec (\vs -> f xs is vs) vs seeds

entry rev_vec [n] [k] (xs: [k]f64) (is: [n]i64) (vs: [n]f64) =
  let seeds = tabulate n (\i -> replicate k 0 with [i] = 1)
  in vjp_vec (\vs -> f xs is vs) vs seeds
