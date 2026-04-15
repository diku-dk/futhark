-- Multiple intra-group reductions.
-- ==
-- random input { [1][256]i32 } auto output
-- compiled random input { [100][256]i32 } auto output
-- structure gpu { SegMap/SegRed 1 SegMap/SegRed/SegBinOp 2 }

def main xs =
  #[incremental_flattening(only_intra)]
  unzip (map (\xs -> (i32.sum xs, i32.product xs)) xs)
