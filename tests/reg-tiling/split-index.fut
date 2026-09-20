-- Register tiling where the index of a tiled input is computed from
-- the thread index, here by splitting it into two dimensions.
-- ==
-- compiled random input { [40][50]i32 [50][5][7]i32 } auto output
-- structure gpu { SegMap/Loop/WithAcc 2 }

def main [n][k][h][w] (a: [n][k]i32) (b: [k][h][w]i32) : [n][h * w]i32 =
  map (\xs -> tabulate (h * w) (\j -> #[sequential] i32.sum (map2 (*) xs b[:, j / w, j % w]))) a
