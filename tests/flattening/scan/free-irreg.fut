-- An irregular array that is free in the map part of an inner
-- scanomap, and hence must be replicated. Its size intentionally
-- differs from the width of the scanomap.
-- ==
-- input { [1i64,4i64,0i64,3i64] }
-- auto output
-- structure gpu { /If/True/SegMap 1 /If/False/SegScan 2 }

def main (ns: []i64) =
  map (\n ->
         let ys = opaque (iota (n + 1))
         in i64.sum (scan (+) 0 (map (\i -> ys[i] + ys[i + 1]) (iota n))))
      ns
