-- ==
-- input { [[7i64, 2i64, 3i64], [4i64, 5i64, 6i64]] }
-- auto output
entry main [n] [m] (xss: [n][m]i64) =
  map (\xs ->
         let ys = map (\x -> 
           let ts = opaque (iota x) in
           ts[1]
         ) xs
         let zs = scan (+) 0 ys
         let ks = map2 (\z y -> z * 2 + y) zs ys
         in (ks[1] + ys[1] + zs[1]))
      xss
