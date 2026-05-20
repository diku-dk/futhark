-- ==
-- input { 3i64 [[1i64, 2i64, 3i64], [4i64, 5i64, 6i64]] }
-- auto output

entry main [n] [m] (k: i64) (xss: [n][m]i64) =
  map (\xs ->
         let zs = scan (+) 0 xs
          let ys = map (\x -> 
           let ts = opaque (iota x) in
           ts[k] 
         ) zs
         in (ys[k] + zs[k]))
      xss
