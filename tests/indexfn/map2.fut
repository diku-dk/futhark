def f [n] (xs: [n]i64) (inds: [n]i64) =
  map (\i -> if i > 0 then xs[i-1] else 0) inds
