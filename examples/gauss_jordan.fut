-- Matrix inversion using Gauss-Jordan elimination without pivoting.
--
-- Taken from https://www.cs.cmu.edu/~scandal/nesl/alg-numerical.html#inverse
--
-- ==
-- input { [[1.0f32, 2.0f32, 1.0f32], [2.0f32, 1.0f32, 1.0f32], [1.0f32, 1.0f32, 2.0f32]] }
-- output { [[-0.25f32, 0.75f32, -0.25f32], [0.75f32, -0.25f32, -0.25f32], [-0.25f32, -0.25f32, 0.75f32]] }

default (f32)

fun Gauss_Jordan (A: [n][m]f32): [n][m]f32 =
  loop (A) = for i < n do
    let irow = A[0]
    let Ap = A[1:n]
    let v1 = irow[i]
    let irow = map (/v1) irow
    let Ap = map (\jrow ->
                    let scale = jrow[i]
                    in map (\x y -> y - scale * x) irow jrow)
                 Ap
    in concat Ap ([irow])
  in A

fun matrix_inverse (A: [n][n]f32): [n][n]f32 =
  -- Pad the matrix with the identity matrix.
  let Ap = map (\row i ->
                      let padding = replicate n 0.0
                      let padding[i] = 1.0
                      in concat row padding)
                    A (iota n)
  let Ap' = Gauss_Jordan Ap
  -- Drop the identity matrix at the front.
  in Ap'[0:n,n:n*2]

fun main (A: [n][n]f32): [n][n]f32 = matrix_inverse A
