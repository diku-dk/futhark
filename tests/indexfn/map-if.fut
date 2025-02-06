def f xs : {[]i64 | \_ -> true} =
  map (\x -> if x > 100 then x * 2 else x) xs
