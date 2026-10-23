def dotprod [n] (xs: [n]f32) (ys: [n]f32) : f32 =
  reduce (+) 0.0 (map2 (*) xs ys)

def matvecmul [n] [m] (xss: [n][m]f32) (ys: [m]f32) =
  map (dotprod ys) xss

def cost_derivative [n] (output_activations: [n]f32) (y: [n]f32) : [n]f32 =
  map2 (-) output_activations y

def outer_prod [m] [n] (a: [m]f32) (b: [n]f32) : *[m][n]f32 =
  map (\x -> map (\y -> x * y) b) a

def main [i] [j] [k] (w3: [k][j]f32) (x: [i]f32, y: [k]f32) (z2: []f32) (z3: [k]f32) =
  let delta3 = map2 (*) (cost_derivative z3 y) z3
  let nabla_b3 = delta3
  let nabla_w3 = outer_prod delta3 z2
  in (nabla_b3, nabla_w3)
