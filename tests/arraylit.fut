-- Array literals should work even if their safety cannot be
-- determined until runtime.
--
-- ==
-- input { 2i64 2i64 } output { [[0i64,1i64], [3i64, 3i64]] }
-- input { 2i64 3i64 } error: Error

def main (n: i64) (m: i64) : [][]i64 =
  [iota n, replicate m 3i64 :> [n]i64]
