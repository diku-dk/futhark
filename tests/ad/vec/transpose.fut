-- ==
-- entry: fwd_vec fwd_map
-- input { [[1.0,2.0],[3.0,4.0]] }
-- output { [[[1.0, 0.0],[0.0, 0.0]],[[0.0, 0.0],[1.0, 0.0]],[[0.0, 1.0],[0.0, 0.0]],[[0.0, 0.0],[0.0, 1.0]]] }

def f (xs: [][]f64) = transpose xs

entry fwd_vec [n] [m] (xs: [n][m]f64) =
  let seeds =
    tabulate (n * m) (\i -> tabulate (n * m) (\j -> f64.bool (i == j)) |> unflatten)
  in (jvp2_vec f xs seeds).1

entry fwd_map [n] [m] (xs: [n][m]f64) =
  tabulate (n * m)
           (\i -> jvp f xs (tabulate (n * m) (\j -> f64.bool (i == j)) |> unflatten))
