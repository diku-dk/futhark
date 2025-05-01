def f (xs: []i64) : {[][2]i64 | \_ -> true} =
  map (\_x -> scan (+) 0 (iota 2)) xs
