-- Bad to bind in slices, but accepted
-- ==
-- warning: with binding

def main (n: i64) (xs: *[n]i64) =
  let t = iota n
  in t[:let m = n - 4 in m * m / n]
