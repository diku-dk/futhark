def g [n] (xs: [n]i64) : i64 =
  let k = n + 1
  in xs[0]

entry f (xs: [2]i64) : i64 =
  g xs
