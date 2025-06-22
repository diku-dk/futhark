-- ==
-- input { [0.0, 0.0, 0.0] }
-- output { [[0.0, 0.0, 0.0],
--           [0.0, 2.0, 0.0],
--           [0.0, 0.0, 2.0]] }

def identity_mat n = tabulate_2d n n (\i j -> f64.bool (i == j))

def Jacobi [n] (f: [n]f64 -> [n]f64) (x: [n]f64) : [n][n]f64 =
  map (\i -> jvp f x i) (identity_mat n)

def Hessian [n] (f: [n]f64 -> f64) (x: [n]f64) : [n][n]f64 =
  Jacobi (\x -> vjp f x 1) x

entry main (x: [3]f64) = Hessian (\x -> x[1] ** 2 + x[2] ** 2) x
