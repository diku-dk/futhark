-- Careful not to fuse excessively.
-- ==
-- structure { /Screma 2 }

def matmul_diag_full [n] [m] (ds: [n]f32) (A: [n][m]f32) : [n][m]f32 =
  map2 (\d as -> map (* d) as) ds A

def matmul_full_diag [n] [m] (A: [n][m]f32) (ds: [m]f32) : [n][m]f32 =
  transpose (map2 (\d as -> map (* d) as) ds (transpose A))

def main (k0: f32) (D: []f32) (W: [][]f32) =
  let X = map (\d -> f32.exp (f32.neg d * k0 * 1)) D
  let temp = X `matmul_diag_full` W `matmul_full_diag` X
  in temp
