-- ==
-- random input { true [10][20][30]f32 } auto output
-- random input { false [10][20][30]f32 } auto output

entry main [m][n] (b: bool) (xsss: [][m][n]f32) =
  map (\xss ->
         let ys = map (\xs -> if b then map (+ f32.sum xs) xs else map (* 2) xs) xss
         let s = reduce (map2 (+)) (replicate n 0) ys
         let ys[0] = s
         in (ys, xss))
      xsss
  |> unzip
