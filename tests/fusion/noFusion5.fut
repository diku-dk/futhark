-- #2474

entry main [n] (xss: [n][n]f32) (ys: [n]f32) =
  reduce (map2 (+)) (replicate n 0) (map (map2 (+) ys) xss)
  |> map (+ 2)
