let foo [n][m] (A: [n][m]f32): [n][m]f32 =
  (loop A for _i < n do
   let irow = A[0]
   let Ap = A[1:n]
   in concat Ap [irow]) :> [n][m]f32

let main = map foo
