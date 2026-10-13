-- 3D register tiling where the index of a tiled input is computed
-- from the thread index, here by splitting it into two dimensions.
-- ==
-- compiled random input { [16][32]i32 [32][5][7]i32 [64][32]i32 } auto output
-- structure gpu { SegMap/Loop/Loop/WithAcc 1 }

def main [m][n][k][h][w] (a: [n][k]i32) (b: [k][h][w]i32) (c: [m][k]i32) : [m][n][h * w]i32 =
  map (\zs ->
         map (\xs ->
                tabulate (h * w) (\j ->
                                    #[sequential]
                                    i32.sum (map3 (\z x y -> z * x * y) zs xs b[:, j / w, j % w])))
             a)
      c
