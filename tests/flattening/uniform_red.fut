-- This one is multiversioned due to heuristics that may change.
-- ==
-- compiled random input { [1][256]i32 } auto output
-- compiled random input { [100][256]i32 } auto output
-- compiled random input { [100][512]i32 } auto output
-- structure gpu { SegScan 0 SegMap 1 SegRed 1 If 1 }

entry main [n] [m] (xss: [n][m]i32) =
  map (\xs ->
         let a = xs[0]
         let ys = map (+ a) xs
         let zs = reduce (+) 0 ys
         in zs)
      xss
