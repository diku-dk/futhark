-- ==
-- input { 4i64 [1i64,2i64] [3i64,4i64] }
-- auto output
def main [n] [m] (o: i64) (xs: [n]i64) (ys: [m]i64) =
  map (\x ->
         map (\y ->
                let row_y = replicate y x with [1] = x + o
                let z = map (+ 10) row_y
                let mat = replicate x (row_y) with [0] = z
                in mat[0][0])
             ys)
      xs
