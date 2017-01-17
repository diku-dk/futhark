-- Small library of linear algebra-ish operations.

include futlib.numeric

module type LINALG = {
  type t

  val dotprod: []t -> []t -> t
}

module LinAlg(T: NUMERIC): {
  type t = T.t
  val dotprod: []t -> []t -> t
  val matmul: [][]t -> [][]t -> [][]t
  val inv: [][]t -> [][]t
} = {
  open T
  type t = T.t
  fun dotprod (xs: [n]t) (ys: [n]t): t =
    reduce (+) (fromInt 0) (map (*) xs ys)

  fun matmul (xss: [n][p]t) (yss: [p][m]t): [n][m]t =
    map (\xs -> map (dotprod xs) (transpose yss)) xss

  -- Matrix inversion is implemented with Gauss-Jordan.
  fun gauss_jordan (A: [n][m]t) (i: i32): [n][m]t =
    if i Intrinsics.== n then A else
    let irow = A[0]
    let Ap = A[1:n]
    let v1 = irow[i]
    let irow = map (\x -> x / v1) irow
    let Ap = map (\jrow ->
                  let scale = jrow[i]
                  in map (\x y -> y - (scale * x)) irow jrow)
                 Ap
    in gauss_jordan (concat Ap ([irow])) (i Intrinsics.+ 1)

  fun inv (A: [n][n]t): [n][n]t =
    -- Pad the matrix with the identity matrix.
    let Ap = map (\row i ->
                  let padding = replicate n (fromInt 0)
                  let padding[i] = fromInt 1
                  in concat row padding)
                 A (iota n)
    let Ap' = gauss_jordan Ap 0
    -- Drop the identity matrix at the front.
    in Ap'[0:n,n:n Intrinsics.* 2]
}
