def f (xs: []i64) : {[][2]i64 | \_ -> true} =
  map (\x -> map (\j -> j + x) (iota 2)) xs
