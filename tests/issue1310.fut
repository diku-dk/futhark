-- This is a fragile test, in that it depends on subtle interchange to
-- even produce the code that sequentialisation chokes on.

let dotprod [n] (xs: [n]f64) (ys: [n]f64) =
  f64.sum (map2 (*) xs ys)

let identity (n: i64): [n][n]f64 =
  tabulate_2d n n (\i j ->if j == i then 1 else 0)

let back_substitution [n] (U: [n][n]f64) (y: [n]f64): [n]f64 =
  let x = replicate n 0
  in loop x for j in 0..<n do
    let i = n - j - 1
    let sumx = dotprod U[i,i+1:n] x[i+1:n]
    let x[i] = (y[i] - sumx) / U[i,i]
    in x

let chol2inv [n] (U: [n][n]f64) =
  map (back_substitution U) (identity n)

entry mrecresid = map chol2inv
