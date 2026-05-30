-- #2474

entry main [n] (xss: [n][n]f64) (ys: [n]f64) =
  reduce (map2 (+)) (replicate n 0) (map (map2 (+) ys) xss)
  |> map (+ 2)
