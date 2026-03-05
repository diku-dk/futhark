def consume (xs: *[]i64) : i64 = xs[0]

-- Here the challenge is that the index expression must be computed/compiled
-- before the value expression, or else we will have a consumption error.
def main (xs: *[]i64) (ys: *[]i64) =
  ys with [xs[1]] = consume xs
