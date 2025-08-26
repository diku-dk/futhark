-- An array is both a 'map' input and a free variable in the lambda.
-- ==
-- tags { autodiff }
-- entry: fwd_map fwd_vec rev_map rev_vec
-- input { [1,2,3] }
-- output {
-- [[[2, 0, 0], [1, 1, 0], [1, 0, 1]], [[1, 1, 0], [0, 2, 0], [0, 1, 1]], [[1, 0, 1], [0, 1, 1], [0, 0, 2]]]
-- }

def primal (xs: []i32) =
  map (\x -> map (+ x) xs) xs

def onehot n i : [n]i32 =
  tabulate n (\j -> i32.bool (i == j))

def onehot_2d n m p : [n][m]i32 =
  tabulate_2d n m (\i j -> i32.bool ((i, j) == p))

entry fwd_map [n] (xs: [n]i32) =
  tabulate n (\i -> jvp primal xs (onehot n i))
  |> map transpose
  |> transpose
  |> map transpose

entry fwd_vec [n] (xs: [n]i32) =
  let seeds = tabulate n (\i -> onehot n i)
  in jvp_vec primal xs seeds
     |> map transpose
     |> transpose
     |> map transpose

entry rev_map [n] (xs: [n]i32) =
  tabulate_2d n n (\i j -> vjp primal xs (onehot_2d n n (i, j)))

entry rev_vec [n] (xs: [n]i32) =
  let seeds = tabulate_2d n n (\i j -> onehot_2d n n (i, j))
  in unflatten (vjp_vec primal xs (flatten seeds))
