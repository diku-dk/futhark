entry ensure_pow_2 [n] (xs: [n]i64) : []i64 =
  if n == 2
  then xs
  else let largest = xs[0]
       in iota largest
