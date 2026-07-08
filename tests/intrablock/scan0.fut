-- Simple intra-group scan.
-- ==
-- random input { [1][256]i32 } auto output
-- random input { [10][256]i32 } auto output
-- structure gpu { SegMap/SegScan 1 }

def main xs =
  #[incremental_flattening(only_intra)]
  map (scan (+) 0i32) xs
