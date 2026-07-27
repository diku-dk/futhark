-- ==
-- input { 5i64 4i64 }
-- auto output
def main (m: i64) (n: i64) =
  map (\j ->
         let row_j = replicate n j with [j] = 7i64
         let mat = replicate m (replicate n m) with [0] = row_j
         in map (\row ->
                   let s = mat[0][1]
                   let rs = row[0]
                   let ds = iota j
                   let y = map (\d -> d + mat[0][j] + rs + s) ds
                   let sum = reduce (+) 0 y
                   in map (\x -> x + sum + 3 + rs + s) row)
                mat)
      (iota n)
