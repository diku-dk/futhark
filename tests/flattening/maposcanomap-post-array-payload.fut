-- ==
-- input { [[[1i64, 2i64], [3i64, 4i64], [5i64, 6i64]]] }
-- auto output

entry main [n] [m] [k] (xsss: [n][m][k]i64) =
  map (\xss ->
         let xs = map (\ys -> ys[0]) xss
         let xs' = scan (+) 0 xs
         in map2 (\x ys ->
                    reduce (+) 0 (map (+ x) ys))
                 xs' xss)
      xsss
