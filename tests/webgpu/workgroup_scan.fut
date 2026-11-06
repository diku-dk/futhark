-- Test workgroup (intra-group) scan in WebGPU.
-- WebGPU uses workgroup shared memory for local operations.
-- This tests that scans using shared memory work correctly.

-- ==
-- random input { [1][256]i32 } auto output
-- random input { [10][256]i32 } auto output
-- random input { [100][64]i32 } auto output
-- structure gpu { SegMap/SegScan 1 }

def main xs =
  #[incremental_flattening(only_intra)]
  map (scan (+) 0i32) xs
