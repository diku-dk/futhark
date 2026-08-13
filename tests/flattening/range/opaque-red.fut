-- ==
-- input { [1i64,2i64] [3i64,3i64] [10i64,8i64] }
-- output { [25i64, 27i64] }
-- input { [1i64,2i64] [3i64,2i64] [10i64,-8i64] }
-- error: Range 2..2..<-8 is invalid
-- structure gpu { /Apply/segiota 1 /SegScan 1 }

def main as bs cs =
  #[flattening(only_inner)]
  map3 (\a b c -> i64.sum (opaque (a..b..<c))) as bs cs
