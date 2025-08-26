-- Map with free array variable.
-- ==
-- tags { autodiff }
-- entry: fwd_map rev_map
-- input { [[1,2,3],[4,5,6]] [0,0] }
-- output { [[1, 0], [0, 1]] }

def onehot n i : [n]i32 =
  tabulate n (\j -> i32.bool (i == j))

def primal [n] [m] (free: [n][m]i32) (is: [n]i32) =
  map (\i -> foldl (+) 0 free[i] + i) is

entry fwd_map [n] [m] (free: [n][m]i32) (is: [n]i32) =
  tabulate n (\i -> jvp (primal free) is (onehot n i)) |> transpose

entry fwd_vec [n] [m] (free: [n][m]i32) (is: [n]i32) =
  let seeds = tabulate n (\i -> onehot n i)
  in jvp_vec (primal free) is seeds |> transpose

entry rev_map [n] [m] (free: [n][m]i32) (is: [n]i32) =
  tabulate n (\i -> vjp (primal free) is (onehot n i))

entry rev_vec [n] [m] (free: [n][m]i32) (is: [n]i32) =
  let seeds = tabulate n (\i -> onehot n i)
  in vjp_vec (primal free) is seeds
