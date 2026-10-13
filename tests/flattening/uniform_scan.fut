-- ==
-- compiled random input { [1][256]i32 } auto output
-- compiled random input { [100][256]i32 } auto output
-- compiled random input { [100][512]i32 } auto output
-- structure gpu { SegScan 1 If 0 }

entry main [n] [m] (xss: [n][m]i32) =
  map (\xs ->
         let a = xs[0]
         let ys = map (+ a) xs
         let zs = scan (+) 0 ys
         in zs)
      xss
