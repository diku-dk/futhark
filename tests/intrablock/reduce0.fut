-- Simple intra-group reduction.
-- ==
-- random input { [1][256]i32 } auto output
-- random input { [10][256]i32 } auto output
-- structure gpu { SegMap/SegRed 1 }

def main xs =
  #[incremental_flattening(only_intra)]
  map i32.sum xs
