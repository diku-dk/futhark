-- Small library of linear algebra-ish operations.

include futlib.numeric

module type LINALG {
type t

val dotprod: []t -> []t -> t
}

module LinAlg(T: NUMERIC) {
type t = T.t

fun dotprod (xs: [n]t) (ys: [n]t): t =
  reduce T.add (T.fromInt 0) (map T.mul xs ys)

fun matmul (xss: [n][p]t) (yss: [p][m]t): [n][m]t =
  map (fn xs => map (dotprod xs) (transpose yss)) xss
}
