-- ==
-- random input { [2][3][4]f32 } auto output

def foo [n][m] (A: [n][m]f32): [n][m]f32 =
  (loop A for _i < n do
   let irow = A[0]
   let Ap = A[1:n]
   in concat Ap [irow]) :> [n][m]f32

def main = map foo
