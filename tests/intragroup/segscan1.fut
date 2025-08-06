-- Multiple intra-group scans.
-- ==
-- random input { [1][1][256]i32 } auto output
-- compiled random input { [10][16][16]i32 } auto output
-- compiled random input { [10][256][1]i32 } auto output
-- structure gpu { /SegMap/SegScan 1 SegMap 1 SegScan 1 }

def main xsss =
  #[incremental_flattening(only_intra)]
  xsss
  |> map (\xss -> map (\xs -> (scan (i32.+) 0 xs, scan (i32.*) 1 xs)) xss)
  |> map unzip
  |> unzip
