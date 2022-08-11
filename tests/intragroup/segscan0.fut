-- Simple intra-group scan.
-- ==
-- random input { [1][1][256]i32 } auto output
-- compiled random input { [10][16][16]i32 } auto output
-- compiled random input { [10][256][1]i32 } auto output
-- structure gpu { SegMap/SegScan 1 SegMap 1 SegScan 1 }

def main xss =
  #[incremental_flattening(only_intra)]
  map (\xs -> map (scan (+) 0i32) xs) xss
