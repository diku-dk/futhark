-- ==
-- input { 3i64 4i64 } 
-- auto output
def main (m: i64) (n: i64) : [n][m]f32 =
  map (\j ->
    let row_j = replicate n 0.0f32 with [j] = 7.0f32
    let mat = replicate m (replicate n 5.0f32) with [0] = row_j
    in map (\row -> reduce (+) 0 row) mat
  ) (iota n)
