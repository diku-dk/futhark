-- ==
-- entry: fwd_J rev_J
-- input
-- {
-- [[1.0,2.0],[3.0,4.0]] [1i64, 0i64, 1i64, 1i64]
-- }
-- output
-- {
-- [[[[0.000000f64, 1.000000f64],
--    [0.000000f64, 0.000000f64]],
--   [[1.000000f64, 0.000000f64],
--    [0.000000f64, 0.000000f64]],
--   [[0.000000f64, 1.000000f64],
--    [0.000000f64, 0.000000f64]],
--   [[0.000000f64, 1.000000f64],
--    [0.000000f64, 0.000000f64]]],
--  [[[0.000000f64, 0.000000f64],
--    [0.000000f64, 1.000000f64]],
--   [[0.000000f64, 0.000000f64],
--    [1.000000f64, 0.000000f64]],
--   [[0.000000f64, 0.000000f64],
--    [0.000000f64, 1.000000f64]],
--   [[0.000000f64, 0.000000f64],
--    [0.000000f64, 1.000000f64]]]]
-- }


def gather xs is = map (\(i: i64) -> xs[i]) is

def mapgather xss is = map (`gather` is) xss

def onehot n i : [n]f64 =
  tabulate n (\j -> f64.bool (i==j))

def onehot_2d n m p : [n][m]f64 =
  tabulate_2d n m (\i j -> f64.bool ((i,j)==p))

entry fwd_J [n][m][k] (xs: [n][m]f64) (is: [k]i64) =
  tabulate_2d n m (\i j -> jvp (`mapgather` is) xs (onehot_2d n m (i,j)))
  |> map transpose
  |> map (map transpose)
  |> map transpose

entry rev_J [n][m][k] (xs: [n][m]f64) (is: [k]i64) =
  tabulate_2d n k (\i j -> vjp (`mapgather` is) xs (onehot_2d n k (i,j)))
