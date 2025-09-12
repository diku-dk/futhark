-- Simple scatter, differentiating wrt. target.
-- ==
-- tags { autodiff }
-- entry: fwd rev
-- input { [0f64, 0f64, 0f64, 0f64] [0i64, 1i64] [1f64, 2f64] }
-- output {
--  [[0.000000f64, 0.000000f64, 0.000000f64, 0.000000f64],
--   [0.000000f64, 0.000000f64, 0.000000f64, 0.000000f64],
--   [0.000000f64, 0.000000f64, 1.000000f64, 0.000000f64],
--   [0.000000f64, 0.000000f64, 0.000000f64, 1.000000f64]]
-- }

def f [n] [k] (xs: [k]f64) (is: [n]i64) (vs: [n]f64) =
  scatter (copy xs) is vs

entry fwd [n] [k] (xs: [k]f64) (is: [n]i64) (vs: [n]f64) =
  let g i = jvp (\xs -> f xs is vs) xs (replicate k 0 with [i] = 1)
  in tabulate k g

entry rev [n] [k] (xs: [k]f64) (is: [n]i64) (vs: [n]f64) =
  let g i = vjp (\xs -> f xs is vs) xs (replicate k 0 with [i] = 1)
  in tabulate k g
