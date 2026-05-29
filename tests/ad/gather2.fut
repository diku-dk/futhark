-- ==
-- tags { autodiff }
-- entry: fwd_J rev_J fwd_vec_J rev_vec_J
-- input
-- {
--   [1.0,2.0,3.0,4.0]
--   [[1i64, 3i64], [2i64, 2i64]]
-- }
-- output
-- {
-- [[[0.000000f64, 1.000000f64, 0.000000f64, 0.000000f64],
--   [0.000000f64, 0.000000f64, 0.000000f64, 1.000000f64]],
--  [[0.000000f64, 0.000000f64, 1.000000f64, 0.000000f64],
--   [0.000000f64, 0.000000f64, 1.000000f64, 0.000000f64]]]
-- }

def gather xs is = map (\(i: i64) -> xs[i]) is

def mapgather [k] [n] [m] (xs: [k]f64) (iss: [n][m]i64) : [n][m]f64 =
  map (gather xs) iss

def onehot n i : [n]f64 =
  tabulate n (\j -> f64.bool (i == j))

entry fwd_J [k] [n] [m] (xs: [k]f64) (iss: [n][m]i64) =
  tabulate k (\i -> jvp (`mapgather` iss) xs (onehot k i))
  |> transpose
  |> map transpose

def onehot_2d n m p : [n][m]f64 =
  tabulate_2d n m (\i j -> f64.bool ((i, j) == p))

entry rev_J [k] [n] [m] (xs: [k]f64) (iss: [n][m]i64) =
  tabulate_2d n m (\i j -> vjp (`mapgather` iss) xs (onehot_2d n m (i, j)))

entry fwd_vec_J [k] [n] [m] (xs: [k]f64) (iss: [n][m]i64) =
  let seeds = tabulate k (\i -> onehot k i)
  in jvp_vec (`mapgather` iss) xs seeds
  |> transpose
  |> map transpose

entry rev_vec_J [k] [n] [m] (xs: [k]f64) (iss: [n][m]i64) =
  let seeds = tabulate (n * m) (\p -> onehot_2d n m (p / m, p % m))
  in vjp_vec (`mapgather` iss) xs seeds
  |> unflatten
