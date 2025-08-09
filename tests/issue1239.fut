def n = 2i64

def grid (i: i64) : [n][n]i64 =
  let grid = unflatten (0..<(n * n))
  in if i == 0
     then unflatten (scatter (flatten grid) (0..<n) (replicate n 1))
     else grid

def main (k: i64) : [k][n][n]i64 = map grid (0..<k)
