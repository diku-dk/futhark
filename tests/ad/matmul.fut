-- ==
-- tags { autodiff }
-- entry: fwd_J rev_J fwd_vec_J rev_vec_J
-- input
-- {
-- [[1.0f32,2.0f32],[3.0f32,4.0f32]] [[5.0f32,6.0f32],[7.0f32,8.0f32]]
-- }
-- output
-- {
-- [[[[1.000000f32, 0.000000f32],
--    [1.000000f32, 0.000000f32]],
--   [[0.000000f32, 1.000000f32],
--    [0.000000f32, 1.000000f32]]],
--  [[[1.000000f32, 0.000000f32],
--    [1.000000f32, 0.000000f32]],
--   [[0.000000f32, 1.000000f32],
--    [0.000000f32, 1.000000f32]]]]
-- }

def dotprod xs ys = f32.sum (map2 (+) xs ys)

def matmul xss yss = map (\xs -> map (dotprod xs) (transpose yss)) xss

def onehot_2d n m p : [n][m]f32 =
  tabulate_2d n m (\i j -> f32.bool ((i, j) == p))

entry fwd_J [n] [m] [p] (xss: [n][m]f32) (yss: [m][p]f32) =
  tabulate_2d m p (\i j -> jvp (matmul xss) yss (onehot_2d m p (i, j)))
  |> transpose
  |> map transpose
  |> transpose

entry rev_J [n] [m] [p] (xss: [n][m]f32) (yss: [m][p]f32) =
  tabulate_2d n p (\i j -> vjp (matmul xss) yss (onehot_2d n p (i, j)))

entry fwd_vec_J [n] [m] [p] (xss: [n][m]f32) (yss: [m][p]f32) =
  let seeds = tabulate (m * p) (\k -> onehot_2d m p (k / p, k % p))
  in jmp (matmul xss) yss seeds
  |> unflatten
  |> transpose
  |> map transpose
  |> transpose

entry rev_vec_J [n] [m] [p] (xss: [n][m]f32) (yss: [m][p]f32) =
  let seeds = tabulate (n * p) (\k -> onehot_2d n p (k / p, k % p))
  in mjp (matmul xss) yss seeds
  |> unflatten
