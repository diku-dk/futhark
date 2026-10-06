-- An intra-block result that is built by a loop must also be written
-- directly to global memory, rather than staged in shared memory.  This
-- is the shape used by temporal tiling.
-- ==
-- random input { [4][8]f32 } auto output

entry main [m] [n] (a: [m][n]f32) =
  #[flattening(only_intra)]
  #[intrablock_result(global)]
  map (\row -> loop acc = row for _i < 3 do map (2*) acc) a
