-- Ranges with unknown size produce warnings
-- ==
-- warning: with consumption

def consume (xs: *[]i64): i64 = xs[0]

def main (xs:*[]i64) =
  0..<(consume xs)
