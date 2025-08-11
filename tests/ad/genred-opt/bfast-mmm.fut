-- | dot-product but in which we filter-out the entries for which `vct[i]==NAN`

def dotprod_filt [n] (vct: [n]f32) (xs: [n]f32) (ys: [n]f32) : f32 =
  f32.sum (map3 (\v x y -> x * y * if (v == 333.333) then 0.0 else 1.0) vct xs ys)

-- f32.isnan v

-- | matrix-matrix multiplication but with NAN-filtering on `vct`
def matmul_filt [n] [p] [m] (xss: [n][p]f32) (yss: [p][m]f32) (vct: [p]f32) : [n][m]f32 =
  map (\xs -> map (dotprod_filt vct xs) (transpose yss)) xss

-- | implementation is in this entry point
--   the outer map is distributed directly
entry main [m] [N] [k] (n: i64) (X: [k][N]f32) (images: [m][N]f32) (res_adj: [m][k][k]f32) =
  let Xt = copy (transpose X)
  let Xh = (X[:, :n])
  let Xth = (Xt[:n, :])
  let Yh = (images[:, :n])
  let batchMMM (Z, Zt, Q) = map (matmul_filt Z Zt) Q
  in vjp batchMMM (X, Xt, images) res_adj
