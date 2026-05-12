-- input { 2i64 3i64 }
-- output { [[[1, 1], [2, 0], [3, -1]], [[2, 2], [3, 1], [4, 0]]] }
-- ==
-- structure { Screma 3 }

entry main n m =
  tabulate n (\i -> tabulate m (\j -> [i + j, i - j]) |> flatten)
  |> map unflatten
  |> map (map (map (+ 1)))
