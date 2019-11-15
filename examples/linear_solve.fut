-- Solving a linear system using Gauss-Jordan elimination without pivoting.
--
-- Taken from https://www.cs.cmu.edu/~scandal/nesl/alg-numerical.html#solve
--
-- ==
-- input { [[1.0f32, 2.0f32, 1.0f32], [2.0f32, 1.0f32, 1.0f32], [1.0f32, 1.0f32, 2.0f32]]
--         [1.0f32, 2.0f32, 3.0f32] }
-- output { [0.5f32, -0.5f32, 1.5f32] }

let Gauss_Jordan [n][m] (A: [n][m]f32): [n][m]f32 =
  (loop (A) for i < n do
   let irow = A[0]
   let Ap = A[1:n]
   let v1 = irow[i]
   let irow = map (/v1) irow
   let Ap = map (\jrow ->
                   let scale = jrow[i]
                   in map2 (\x y -> y - scale * x) irow jrow)
                Ap
   in Ap ++ [irow]) :> [n][m]f32

let concat 'a (m: i32) (a: []a) (b: []a) : [m]a =
  a ++ b :> [m]a

let linear_solve [n][m] (A: [n][m]f32) (b: [n]f32): [n]f32 =
  -- Pad the matrix with b.
  let Ap = map2 (concat (m+1)) A (transpose [b])
  let Ap' = Gauss_Jordan Ap
  -- Extract last column.
  in Ap'[0:n,m]

let main [n][m] (A: [n][m]f32) (b: [n]f32): [n]f32 = linear_solve A b
