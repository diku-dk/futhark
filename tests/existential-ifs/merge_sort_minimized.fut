entry ensure_pow_2 [n] (xs: [n]i32): []i32 =
  if n == 2
     then xs
     else let largest = xs[0]
          in iota largest
