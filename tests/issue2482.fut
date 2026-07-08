-- ==
-- compiled random input { [30][10][2]f32 [30][10][30][2]f32 } auto output

def main [n] [k] [l] (xs: [n][l][k]f32) (ys: [n][l][n][k]f32) =
  let op (xa, xb) (ya, yb) =
    ( map2 f32.max xa ya
    , map2 (map2 (+)) xb yb
    )
  in #[incremental_flattening(only_inner)]
     unzip (map (reduce op (replicate k f32.lowest, replicate n (replicate k 0)))
                (map2 zip xs ys))
