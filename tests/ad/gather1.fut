-- ==
-- tags { autodiff no_webgpu }
-- entry: fwd_J rev_J fwd_vec_J rev_vec_J
-- input
-- {
-- [[1.0f32,2.0f32],[3.0f32,4.0f32]] [1i64, 0i64, 1i64, 1i64]
-- }
-- output
-- {
-- [[[[0.000000f32, 1.000000f32],
--    [0.000000f32, 0.000000f32]],
--   [[1.000000f32, 0.000000f32],
--    [0.000000f32, 0.000000f32]],
--   [[0.000000f32, 1.000000f32],
--    [0.000000f32, 0.000000f32]],
--   [[0.000000f32, 1.000000f32],
--    [0.000000f32, 0.000000f32]]],
--  [[[0.000000f32, 0.000000f32],
--    [0.000000f32, 1.000000f32]],
--   [[0.000000f32, 0.000000f32],
--    [1.000000f32, 0.000000f32]],
--   [[0.000000f32, 0.000000f32],
--    [0.000000f32, 1.000000f32]],
--   [[0.000000f32, 0.000000f32],
--    [0.000000f32, 1.000000f32]]]]
-- }

def gather xs is = map (\(i: i64) -> xs[i]) is

def mapgather xss is = map (`gather` is) xss

def onehot n i : [n]f32 =
  tabulate n (\j -> f32.bool (i == j))

def onehot_2d n m p : [n][m]f32 =
  tabulate_2d n m (\i j -> f32.bool ((i, j) == p))

entry fwd_J [n] [m] [k] (xs: [n][m]f32) (is: [k]i64) =
  tabulate_2d n m (\i j -> jvp (`mapgather` is) xs (onehot_2d n m (i, j)))
  |> map transpose
  |> map (map transpose)
  |> map transpose

entry rev_J [n] [m] [k] (xs: [n][m]f32) (is: [k]i64) =
  tabulate_2d n k (\i j -> vjp (`mapgather` is) xs (onehot_2d n k (i, j)))

entry fwd_vec_J [n] [m] [k] (xs: [n][m]f32) (is: [k]i64) =
  let seeds = tabulate (n * m) (\p -> onehot_2d n m (p / m, p % m))
  in jmp (`mapgather` is) xs seeds
  |> unflatten
  |> map transpose
  |> map (map transpose)
  |> map transpose

entry rev_vec_J [n] [m] [k] (xs: [n][m]f32) (is: [k]i64) =
  let seeds = tabulate (n * k) (\p -> onehot_2d n k (p / k, p % k))
  in mjp (`mapgather` is) xs seeds
  |> unflatten
