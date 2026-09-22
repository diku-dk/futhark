-- 2D tiling where the index of the tiled input is computed from the
-- thread index in the prelude, and one dimension is not a multiple of
-- the tile size.  The out-of-bounds threads must still load the
-- correct elements of the tile.  The reduction has three inputs, as
-- it would otherwise be register tiled (see
-- tests/reg-tiling/split-index.fut).
-- ==
-- compiled random input { [1][2]i32 [2][1][2]i32 } auto output

def main [n][k][h][w] (a: [n][k]i32) (b: [k][h][w]i32) : [n][h * w]i32 =
  map (\xs ->
         tabulate (h * w) (\j ->
                             #[sequential]
                             i32.sum (map3 (\x y z -> x * y * z) xs b[:, j / w, j % w] (reverse xs))))
      a
