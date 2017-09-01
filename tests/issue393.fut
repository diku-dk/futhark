-- ==
-- structure { /Map 1 /Map/Map 1 /Map/Redomap 1 }

let dotprod [n] (xs: [n]f64) (ys: [n]f64): f64 =
  reduce (+) 0.0 (map (*) xs ys)

let matvecmul [n] [m] (xss: [n][m]f64) (ys: [m]f64) =
  map (dotprod ys) xss

let outer_prod [m][n] (a:[m]f64) (b:[n]f64): [m][n]f64 =
  map (\x -> map (\y -> x * y) b) a

let main [i] [j] [k] (b2: [j]f64) (b3: [k]f64) (w3: [k][j]f64) (x:[i]f64) =
  let delta2 = map (*) (matvecmul (rearrange (1,0) w3) b3) b2
  let nabla_w2 = outer_prod delta2 x
  in (delta2,nabla_w2)
