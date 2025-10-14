-- Distributing an in-place update of slice with a bounds check.
-- ==
-- input { [[1,2,3],[4,5,6]] [0i64,1i64] [42,1337] }
-- output { [[42,1337,3],[4,42,1337]] }
-- structure gpu { SegMap/Update 0 }

def main [n] [m] (xss: *[n][m]i32) (is: [n]i64) (ys: [2]i32) =
  map2 (\xs i -> copy xs with [i:i + 2] = ys) xss is
