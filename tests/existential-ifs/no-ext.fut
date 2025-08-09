-- ==
-- structure gpu-mem { Manifest 1 }
entry ensure_pow_2 [n] [m] (xs: [n][m]i32) : [][]i32 =
  if n == 2
  then xs[0:m / 2, 0:n / 2]
  else xs[0:n / 2, 0:m / 2]
