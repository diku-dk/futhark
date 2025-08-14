-- ==
-- structure gpu-mem { Alloc 3 }
-- structure seq-mem { Alloc 2 }

def main [n] (xs: *[n][n]i64) (i: i64) : (i64, [n][n]i64) =
  let a = iota n
  let b = if i > 5 then a[i:n] else a[0:n - i]
  let s = reduce (+) 0i64 b
  let xs[i] = a
  in (s, xs)
