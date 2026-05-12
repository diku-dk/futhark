-- Multiple intra-group reductions.
-- ==
-- random input { [1][1][256]i32 } auto output
-- compiled random input { [10][16][16]i32 } auto output
-- compiled random input { [10][256][1]i32 } auto output
-- structure gpu { /SegMap/SegRed 1 SegMap 1 SegRed 1 }

def main xsss =
  #[incremental_flattening(only_intra)]
  xsss
  |> map (\xss -> map (\xs -> (i32.sum xs, i32.product xs)) xss)
  |> map unzip
  |> unzip
