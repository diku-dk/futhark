-- ==
-- input { 1i64 [[6i64, 2i64, 3i64], [4i64, 5i64, 6i64]] }
-- auto output
-- structure gpu { /If/True/SegMap 1 /If/False/If/True/SegMap/SegRed 1 /If/False/If/False/SegMap 1 }

entry main [n] [m] (k: i64) (xss: [n][m]i64) =
  map (\xs ->
         let ys = map (\x -> let z = opaque (map (+ 2) (iota x)) in z[k]) xs
         let s = reduce (+) (0) ys
         in s + ys[m - 1])
      xss
