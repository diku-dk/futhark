-- ==
-- input { [[[1i64, 2i64]],
--          [[3i64, 4i64]]]
--         [[[5i64, 6i64], [7i64, 8i64]],
--          [[9i64, 10i64], [11i64, 12i64]]] }
-- auto output

def main [n] [m] [z] [k] (xss : [n][z][k]i64) (yss : [n][m][k]i64) =
  map2 (\xs ys ->
          opaque (concat xs ys))
       xss
       yss
