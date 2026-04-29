-- ==
-- input { [1i32, 2i32, 3i32] [4i32, 5i32] [7i32, 8i32, 9i32] }
-- auto output

def main [n] [m] [p] (xs: [n]i32) (ys: [m]i32) (zs: [p]i32) : [n][m][p]i32 =
  map (\x ->
         map (\y ->
                map (\z -> x - z + y) zs)
             ys)
      xs