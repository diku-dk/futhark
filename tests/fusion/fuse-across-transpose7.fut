-- Careful not to fuse excessively.
-- ==
-- structure { /Screma 2 }

def matmul_diag_full [n] [m] (ds: [n]f64) (A: [n][m]f64) : [n][m]f64 =
  map2 (\d as -> map (* d) as) ds A

def matmul_full_diag [n] [m] (A: [n][m]f64) (ds: [m]f64) : [n][m]f64 =
  transpose (map2 (\d as -> map (* d) as) ds (transpose A))

def main (k0: f64) (D: []f64) (W: [][]f64) =
  let X = map (\d -> f64.exp (f64.neg d * k0 * 1)) D
  let temp = X `matmul_diag_full` W `matmul_full_diag` X
  in temp
