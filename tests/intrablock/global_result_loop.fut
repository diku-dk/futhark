-- A per-block result built by a loop that is not updated in place.
-- Such a loop is double buffered in global memory anyway, so the
-- attribute makes no difference, but the result must be correct.
-- ==
-- random input { [4][8]f32 } auto output

entry main [m] [n] (a: [m][n]f32) =
  #[flattening(only_intra)]
  #[intrablock_result(global)]
  map (\row -> loop acc = row for _i < 3 do map (2*) acc) a
