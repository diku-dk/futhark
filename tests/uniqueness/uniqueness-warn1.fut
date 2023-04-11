-- Bad to consume in slices, but accepted
-- ==
-- warning: with consumption

def consume (xs: *[]i64): i64 = xs[0]

def main (n:i64) (xs:*[n]i64) =
  let t = iota n
  in t[:consume xs]
