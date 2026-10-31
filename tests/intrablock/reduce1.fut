-- Multiple intra-group reductions.
-- ==
-- random input { [1][256]i32 } auto output
-- compiled random input { [100][256]i32 } auto output
-- input { empty([2][0]i32) } output { [0, 0] [1, 1] }
-- structure gpu { SegMap/SegRed 1 SegMap/SegRed/SegBinOp 2 }

def main xs =
  #[flattening(only_intra)]
  unzip (map (\xs -> (i32.sum xs, i32.product xs)) xs)
