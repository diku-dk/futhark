-- ==
-- input { [[1i32, 2i32, 3i32], [4i32, 5i32, 6i32]] }
-- auto output

entry main [n] [m] (xss: [n][m]i32) =
  map (\xs ->
         let a = xs[0]
         let ys = map (+ a) xs
         let zs = scan (+) 0 ys
         in map (+ a) zs)
      xss
