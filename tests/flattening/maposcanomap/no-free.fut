-- ==
-- input { 3i32 [[1i32, 2i32, 3i32], [4i32, 5i32, 6i32]] }
-- auto output

entry main [n] [m] (k: i32) (xss: [n][m]i32) =
  map (\xs ->
         let ys = map (+ k) xs
         let zs = scan (+) 0 ys
         let ks = map2 (\z y -> z * 2 + y) zs ys
         in (ks ++ ys ++ zs))
      xss
