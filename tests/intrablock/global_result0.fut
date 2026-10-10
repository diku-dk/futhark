-- Intra-block kernel result written directly to global memory instead
-- of being staged in shared memory, for block results too large to fit
-- in shared memory.  The block result is bound to its slice of the
-- global result array, so no shared memory is reserved and no copy-out
-- is performed.
-- ==
-- random input { [8][16]f32 } auto output

entry main [n] [m] (a: [m][n]f32) =
  #[flattening(only_intra)]
  #[intrablock_result(global)]
  map (\row -> scan (+) 0 row) a
