def f (ids: []i64) : {[][2]i64 | \_ -> true} =
  map (\i -> map (\j -> 2*i + j) (iota 2)) ids
