-- ==
-- tags { autodiff }
-- entry: fwd_J rev_J
-- input
-- {
-- [[1.0,2.0],[3.0,4.0]] [[5.0,6.0],[7.0,8.0]]
-- }
-- output
-- {
-- [[[[1.0f64, 0.0f64],
--    [2.0f64, 0.0f64]],
--   [[0.0f64, 1.0f64],
--    [0.0f64, 2.0f64]]],
--  [[[3.0f64, 0.0f64],
--    [4.0f64, 0.0f64]],
--   [[0.0f64, 3.0f64],
--    [0.0f64, 4.0f64]]]]
-- }

def dotprod xs ys = f64.sum (map2 (*) xs ys)

def matmul xss yss = map (\xs -> map (dotprod xs) (transpose yss)) xss

def onehot_2d n m p : [n][m]f64 =
  tabulate_2d n m (\i j -> f64.bool ((i, j) == p))

entry fwd_J [n] [m] [p] (xss: [n][m]f64) (yss: [m][p]f64) =
  tabulate_2d m p (\i j -> jvp (matmul xss) yss (onehot_2d m p (i, j)))
  |> transpose
  |> map transpose
  |> transpose

entry rev_J [n] [m] [p] (xss: [n][m]f64) (yss: [m][p]f64) =
  tabulate_2d n p (\i j -> vjp (matmul xss) yss (onehot_2d n p (i, j)))
