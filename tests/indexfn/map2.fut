def f [n] (xs: [n]i64) (inds: [n]i64) : {[]i64 | \_ -> true} =
  map (\i -> if 0 < i && i <= n then xs[i-1] else 0) inds
