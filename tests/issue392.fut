def dotprod [n] (xs: [n]f64) (ys: [n]f64) : f64 =
  reduce (+) 0.0 (map2 (*) xs ys)

def matvecmul [n] [m] (xss: [n][m]f64) (ys: [m]f64) =
  map (dotprod ys) xss

def cost_derivative [n] (output_activations: [n]f64) (y: [n]f64) : [n]f64 =
  map2 (-) output_activations y

def outer_prod [m] [n] (a: [m]f64) (b: [n]f64) : *[m][n]f64 =
  map (\x -> map (\y -> x * y) b) a

def main [i] [j] [k] (w3: [k][j]f64) (x: [i]f64, y: [k]f64) (z2: []f64) (z3: [k]f64) =
  let delta3 = map2 (*) (cost_derivative z3 y) z3
  let nabla_b3 = delta3
  let nabla_w3 = outer_prod delta3 z2
  in (nabla_b3, nabla_w3)
