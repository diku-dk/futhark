let n = 2i64

let grid (i: i64): [n][n]i64 =
  let grid = unflatten n n (0..<n * n)
  in if i == 0
     then unflatten n n (scatter (flatten grid) (0..<n) (replicate n 1))
     else grid

let main (k: i64): [k][n][n]i64 = map grid (0..<k)
