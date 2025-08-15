-- An array is both a 'map' input and a free variable in the lambda.
-- ==
-- tags { autodiff }
-- entry: fwd_J rev_J
-- input { [1,2,3] }
-- output {
-- [[[2, 0, 0], [1, 1, 0], [1, 0, 1]], [[1, 1, 0], [0, 2, 0], [0, 1, 1]], [[1, 0, 1], [0, 1, 1], [0, 0, 2]]]
-- }

def f (xs: []i32) =
  map (\x -> map (+ x) xs) xs

def onehot n i : [n]i32 =
  tabulate n (\j -> i32.bool (i == j))

def onehot_2d n m p : [n][m]i32 =
  tabulate_2d n m (\i j -> i32.bool ((i, j) == p))

entry fwd_J [n] (xs: [n]i32) =
  tabulate n (\i -> jvp f xs (onehot n i))
  |> map transpose
  |> transpose
  |> map transpose

entry rev_J [n] (xs: [n]i32) =
  tabulate_2d n n (\i j -> vjp f xs (onehot_2d n n (i, j)))
