-- ==
-- input { 3i64 [[1i64, 2i64, 3i64], [4i64, 5i64, 6i64]] }
-- auto output

entry main [n][m] (k: i64) (xss: [n][m]i64) =
  map
    (\xs ->
       let ys = map (\x -> let z = opaque (iota x) in z[k]) xs
       let s = reduce (+) (0) ys
       in s + ys[m - 1])
    xss
