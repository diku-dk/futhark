def f [n] (xs: [n]i64) : {[]i64 | \_ -> true} =
  map (\x -> if n > 0 then x * 2 else x) xs
