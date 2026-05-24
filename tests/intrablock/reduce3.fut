-- Segmented intra-group reduction.
-- ==
-- random input { [1][16][16]i32 } auto output
-- random input { [10][16][16]i32 } auto output
-- structure gpu { SegMap/SegRed 1 }

def main xsss =
  #[incremental_flattening(only_intra)]
  map (map (i32.sum >-> (* 2))) xsss
