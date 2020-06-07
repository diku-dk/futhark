-- A funky reduce with vectorised operator (and not interchangeable).
-- ==
-- compiled random input { [100][100]i32 } auto output

let main [n][m] (xss: [n][m]i32) =
  reduce (map2 (+)) (replicate m 0) (map (scan (+) 0) xss)
