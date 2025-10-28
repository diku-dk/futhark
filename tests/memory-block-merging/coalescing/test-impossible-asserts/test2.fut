-- ==
-- structure gpu-mem { Alloc 1 }
-- structure seq-mem { Alloc 1 }

def main [n] (xs: *[n][n]i64) (a0: [n]i64) (i: i64) : ([n]i64, [n][n]i64) =
  let a = map (\e -> e * e / 3) a0
  let a[i] = 33i64
  let xs[i] = a
  in (a, xs)
