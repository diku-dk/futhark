def f xs : {[]i64 | \_ -> true} =
  map (\x -> x * 2) xs
