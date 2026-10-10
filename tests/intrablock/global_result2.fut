-- As global_result0, but the per-block result ends up with a transposed
-- (non-direct) layout, so it cannot be aliased with a constant byte
-- offset.  It must then fall back to being staged in shared memory and
-- copied out, which must still produce correct results.
-- ==
-- random input { [4][8]f32 } auto output

entry main [m] [p] (a: [m][p]f32) =
  #[flattening(only_intra)]
  #[intrablock_result(global)]
  map (\row -> map (\x -> replicate 16 x) row) a
