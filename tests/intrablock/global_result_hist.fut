-- A block-level reduce_by_index whose destination is the result of
-- the block.  The atomics of a block-level histogram assume shared
-- memory, so the destination must not be moved to global memory.
-- ==
-- random input { [2][8][8]f32 [2][8]i64 [2][8]i64 [2][8]f32 } auto output

def main [n] [k] [m] (dests: [k][n][m]f32) iss jss vss =
  #[flattening(only_intra)]
  #[intrablock_result(global)]
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
