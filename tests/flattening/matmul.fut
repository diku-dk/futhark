-- ==
-- input { [[1i64, 2i64, 3i64],[4i64, 5i64, 6i64]] [[7i64, 8i64],[9i64, 10i64], [11i64, 12i64]] }
-- auto output
def main [n] [m] [p] (A: [n][m]i64) (B: [m][p]i64) : [n][p]i64 =
  map (\A_row ->
         map (\B_col ->
                #[sequential] reduce (+) 0 (map2 (*) A_row B_col))
             (transpose B))
      A
