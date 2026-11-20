-- ==
-- tags { no_webgpu }
-- structure { /Screma 1 /Screma/Screma 2 }

def dotprod [n] (xs: [n]f32) (ys: [n]f32) : f32 =
  reduce (+) 0.0 (map2 (*) xs ys)

def matvecmul [n] [m] (xss: [n][m]f32) (ys: [m]f32) =
  map (dotprod ys) xss

def outer_prod [m] [n] (a: [m]f32) (b: [n]f32) : [m][n]f32 =
  map (\x -> map (\y -> x * y) b) a

def main [i] [j] [k] (b2: [j]f32) (b3: [k]f32) (w3: [k][j]f32) (x: [i]f32) =
  let delta2 = map2 (*) (matvecmul (transpose w3) b3) b2
  let nabla_w2 = outer_prod delta2 x
  in (delta2, nabla_w2)
