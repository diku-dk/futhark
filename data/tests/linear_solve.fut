-- Solving a linear system using Gauss-Jordan elimination without pivoting.
--
-- Taken from https://www.cs.cmu.edu/~scandal/nesl/alg-numerical.html#solve
--
-- ==
-- input { [[1.0f32, 2.0f32, 1.0f32], [2.0f32, 1.0f32, 1.0f32], [1.0f32, 1.0f32, 2.0f32]]
--         [1.0f32, 2.0f32, 3.0f32] }
-- output { [0.5f32, -0.5f32, 1.5f32] }

fun Gauss_Jordan (A: [n][m]f32) (i: int): [n][m]f32 =
  if i == n then A else
    let irow = A[0]
    let Ap = A[1:n]
    let v1 = irow[i]
    let irow = map (/v1) irow
    let Ap = map (fn jrow =>
                    let scale = jrow[i]
                    in zipWith (fn x y => y - scale * x) irow jrow)
                 Ap
    in Gauss_Jordan (concat Ap ([irow])) (i+1)

fun linear_solve (A: [n][m]f32) (b: [n]f32): [n]f32 =
  -- Pad the matrix with b.
  let Ap = concat@1 A (transpose ([b]))
  let Ap' = Gauss_Jordan Ap 0
  -- Extract last column.
  in Ap'[0:n,m]

fun main(A: [n][m]f32) (b: [n]f32): [n]f32 = linear_solve A b
