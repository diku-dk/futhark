-- With #[intrablock_result(global)], the result of each block is
-- constructed directly in its slice of the result of the kernel,
-- rather than in shared memory followed by a copy.
-- ==
-- random input { [8][16]f32 } auto output
-- structure gpu-mem { Alloc 1 }

entry main [n] [m] (a: [m][n]f32) =
  #[flattening(only_intra)]
  #[intrablock_result(global)]
  map (\row -> scan (+) 0 row) a
