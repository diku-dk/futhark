def f xs : {[]i64 | \_ -> true} =
  map (\x -> if (if 0 < x then x > 100 else x > 10) then x * 2 else x) xs
