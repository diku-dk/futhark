-- ==
-- entry: fwd_J rev_J
-- compiled input
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

let gather xs is = map (\(i: i64) -> xs[i]) is

let mapgather [k][n][m] (xs: [k]f64) (iss: [n][m]i64) : [n][m]f64 =
  map (gather xs) iss

let onehot n i : [n]f64 =
  tabulate n (\j -> f64.bool (i==j))

entry fwd_J [k][n][m] (xs: [k]f64) (iss: [n][m]i64) =
  tabulate k (\i -> jvp (`mapgather` iss) xs (onehot k i))
  |> transpose
  |> map transpose

let onehot_2d n m p : [n][m]f64 =
  tabulate_2d n m (\i j -> f64.bool ((i,j)==p))

entry rev_J [k][n][m] (xs: [k]f64) (iss: [n][m]i64) =
  tabulate_2d n m (\i j -> vjp (`mapgather` iss) xs (onehot_2d n m (i,j)))
