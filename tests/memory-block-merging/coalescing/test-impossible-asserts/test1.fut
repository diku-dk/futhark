let main [n] (xs: *[n][n]i64) (a0: [n]i64) (i: i64): (i64, [n][n]i64) =
  let a = map (\e -> e*e/3) a0
  let b = if i > 5 then a[i:n] else a[0:n-i]
  let s = reduce (+) 0i64 b
  let xs[i] = a
  in  (s, xs)
