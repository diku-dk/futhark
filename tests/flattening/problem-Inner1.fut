-- ==
-- input { 4i64 [1i64, 2i64, 3i64] [4i64, 5i64, 6i64,9i64, 5i64] }
-- input { 3i64 [5i64, 2i64, 3i64,3i64,3i64] [4i64, 5i64, 6i64,9i64, 5i64] }
-- auto output
def main [n] [m] (o: i64) (xs: [n]i64) (ys: [m]i64) =
  map (\j ->
         map (\x ->
                map (\y ->
                       let row_y = replicate y j with [1] = j + x + y
                       let mat = replicate x (replicate y (j + 4)) with [0] = row_y
                       let mat2 = opaque (map (\row -> map (+1) row) mat)
                      in mat2[0][0])
                    ys)
             xs)
      (iota o)
