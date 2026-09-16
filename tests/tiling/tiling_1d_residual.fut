-- Matrix-vector product where the tiled dimension is larger than the
-- tile size but not a multiple of it, so there is a partial last tile.
-- ==
-- compiled random input { [1000]i32 [100][1000]i32 } auto output

def main [n][k] (xs: [k]i32) (yss: [n][k]i32) : [n]i32 =
  map (\ys -> #[sequential] i32.sum (map2 (*) xs ys)) yss
