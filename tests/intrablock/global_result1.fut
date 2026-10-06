-- As global_result0, but with a two-dimensional per-block result.
-- ==
-- random input { [6][5][8]f32 } auto output

entry main [n] [m] [k] (a: [n][m][k]f32) =
  #[flattening(only_intra)]
  #[intrablock_result(global)]
  map (\plane -> map (scan (+) 0) plane) a
