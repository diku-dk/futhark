-- Map with free array variable.
-- ==
-- tags { autodiff }
-- entry: fwd_J rev_J fwd_vec_J rev_vec_J
-- input { [[1,2,3],[4,5,6]] [0,0] }
-- output { [[1, 0], [0, 1]] }

def onehot n i : [n]i32 =
  tabulate n (\j -> i32.bool (i == j))

def f [n] [m] (free: [n][m]i32) (is: [n]i32) =
  map (\i -> foldl (+) 0 free[i] + i) is

entry fwd_J [n] [m] (free: [n][m]i32) (is: [n]i32) =
  tabulate n (\i -> jvp (f free) is (onehot n i)) |> transpose

entry rev_J [n] [m] (free: [n][m]i32) (is: [n]i32) =
  tabulate n (\i -> vjp (f free) is (onehot n i))

entry fwd_vec_J [n] [m] (free: [n][m]i32) (is: [n]i32) =
  let seeds = tabulate n (\i -> onehot n i)
  in jvp_vec (f free) is seeds |> transpose

entry rev_vec_J [n] [m] (free: [n][m]i32) (is: [n]i32) =
  let seeds = tabulate n (\i -> onehot n i)
  in vjp_vec (f free) is seeds
