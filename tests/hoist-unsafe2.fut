-- Test that we *do* hoist a potentially unsafe (but loop-invariant)
-- expression out of a loop.
-- ==
-- input { 4i64 [1i64,2i64,3i64] } output { 6i64 }
-- input { 0i64 empty([0]i64) } output { 0i64 }
-- structure { /Loop/BinOp 2 }

def main [n] (a: i64) (xs: [n]i64) =
  loop acc = 0 for x in xs do acc + x * (a / n)
