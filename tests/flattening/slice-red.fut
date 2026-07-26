-- ==
-- input { [[0i64,1i64,5i64],[-2i64,9i64,1i64]] [0i64,1i64] }
-- output { [6i64,10i64] }
-- structure gpu { /If/True/SegMap 1 /If/False/SegMap 3 /If/False/SegScan 2 }

def main =
  map2 (\A (i: i64) -> i64.sum A[i:])
