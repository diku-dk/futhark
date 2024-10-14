-- Simple scatter, differentiating wrt. values.
-- ==
-- entry: fwd rev
-- input { [0f64, 0f64, 0f64, 0f64] [0i64, 1i64, 2i64, 3i64] [1f64, 2f64, 3f64, 0f64] }
-- output {
--  [[1.000000f64, 0.000000f64, 0.000000f64, 0.000000f64],
--   [0.000000f64, 1.000000f64, 0.000000f64, 0.000000f64],
--   [0.000000f64, 0.000000f64, 1.000000f64, 0.000000f64],
--   [0.000000f64, 0.000000f64, 0.000000f64, 1.000000f64]]
-- }

def f [n][k] (xs: [k]f64) (is: [n]i64) (vs: [n]f64) =
  scatter (copy xs) is vs

entry fwd [n][k] (xs: [k]f64) (is: [n]i64) (vs: [n]f64) =
  let g i = jvp (\vs -> f xs is vs) vs (replicate n 0 with [i] = 1)
  in tabulate n g

entry rev [n][k] (xs: [k]f64) (is: [n]i64) (vs: [n]f64) =
  let g i = vjp (\vs -> f xs is vs) vs (replicate k 0 with [i] = 1)
  in tabulate n g
