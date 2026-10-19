-- As global_result0, but the per-block result is a view with a
-- non-direct layout.  Whatever the compiler does with it, the result
-- must be correct.
-- ==
-- random input { [4][8]f32 } auto output

entry main [m] [p] (a: [m][p]f32) =
  #[flattening(only_intra)]
  #[intrablock_result(global)]
  map (\row -> map (\x -> replicate 16 x) row) a
