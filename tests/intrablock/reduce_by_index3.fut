-- Multidimensional reduce_by_index at block level.
-- ==
-- random input { [1][8][8]f32 [1][8]i64 [1][8]i64 [1][8]f32 } auto output
-- structure gpu { /SegMap/Loop/SegHist 1 }

def main [n] [k] [m] (dests: [k][n][m]f32) iss jss vss =
  #[incremental_flattening(only_intra)]
  map3 (\dest is vs ->
          loop dest = copy dest
          for _i < 10 do
            reduce_by_index_2d dest
                               (\x y -> if f32.abs x > f32.abs y then x else y)
                               0.0
                               (map (\(i, j) -> (i %% n, j %% m)) is)
                               vs)
       dests
       (map2 zip iss jss)
       vss
