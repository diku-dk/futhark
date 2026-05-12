entry main (gridDim: (i64, i64)) =
  tabulate_2d gridDim.0 gridDim.1 (\i j -> (i, j))
  |> flatten
