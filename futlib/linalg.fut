-- Small library of linear algebra-ish operations.

import "/futlib/math"
import "/futlib/array"

module type linalg = {
  type t

  val dotprod: []t -> []t -> t
}

module linalg(T: numeric): {
  type t = T.t
  val dotprod: []t -> []t -> t
  val matvecmul: [][]t -> []t -> []t
  val matmul: [][]t -> [][]t -> [][]t
  val inv: [][]t -> [][]t
  val ols: [][]t -> []t -> []t
} = {
  open T
  type t = T.t
  let dotprod (xs: [#n]t) (ys: [#n]t): t =
    reduce (+) (from_i32 0) (map (*) xs ys)

  let matvecmul (xss: [#n][#m]t) (ys: [#m]t) =
    map (dotprod ys) xss

  let matmul (xss: [#n][#p]t) (yss: [#p][#m]t): [n][m]t =
    map (\xs -> map (dotprod xs) (transpose yss)) xss

  -- Matrix inversion is implemented with Gauss-Jordan.
  let gauss_jordan (A: [#n][#m]t) (i: i32): [n][m]t =
    loop (A) = for i < n do
      (let irow = A[0]
       let Ap = A[1:n]
       let v1 = irow[i]
       let irow = map (/v1) irow
       let Ap = map (\jrow ->
                     let scale = jrow[i]
                     in map (\x y -> y - scale * x) irow jrow)
                 Ap
       in concat Ap [irow])
    in A

  let inv (A: [#n][#n]t): [n][n]t =
    -- Pad the matrix with the identity matrix.
    let Ap = map (\row i ->
                  let padding = replicate n (from_i32 0)
                  let padding[i] = from_i32 1
                  in concat row padding)
                 A (iota n)
    let Ap' = gauss_jordan Ap 0
    -- Drop the identity matrix at the front.
    in Ap'[0:n,n:n intrinsics.* 2]

  -- Solves Ax=b.
  let ols (X: [#n][#m]t) (b: [#n]t): [m]t =
    matvecmul (matmul (inv (matmul (transpose X) X)) (transpose X)) b
}
