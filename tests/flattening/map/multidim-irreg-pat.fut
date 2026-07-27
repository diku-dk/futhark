def main [n] [m] (xs: [n]i64) (ys: [m]i64) =
  map (\x ->
         let rows = map (\y -> opaque (replicate x y)) ys
         in map (\row -> reduce (+) 0 (opaque row)) rows)
      xs
