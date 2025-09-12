-- ==
-- tags { autodiff }

def dotprod [n] (xs: [n]f32) (ys: [n]f32) =
  reduce (+) 0 (map2 (*) xs ys)

def matvecmul_row [n] [m] (xss: [n][m]f32) (ys: [m]f32) =
  map (dotprod ys) xss

def helmholtz [n] (R: f32) (T: f32) (b: [n]f32) (A: [n][n]f32) (xs: [n]f32) : f32 =
  let bxs = dotprod b xs
  let term1 = map (\x -> f32.log (x / (1 - bxs))) xs |> f32.sum
  let term2 = dotprod xs (matvecmul_row A xs) / (f32.sqrt (8) * bxs)
  let term3 = (1 + (1 + f32.sqrt (2)) * bxs) / (1 + (1 - f32.sqrt (2)) * bxs) |> f32.log
  in R * T * term1 - term2 * term3

entry calculate_jacobian [n] (R: f32) (T: f32) (b: [n]f32) (A: [n][n]f32) (xs: [n]f32) =
  vjp (helmholtz R T b A) xs 1.0
