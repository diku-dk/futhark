-- Multiple intra-group scans.
-- ==
-- random input { [1][256]i32 } auto output
-- compiled random input { [100][256]i32 } auto output
-- structure gpu { SegMap/SegScan 1 }

def main xss =
  #[incremental_flattening(only_intra)]
  unzip (map (\xs -> (scan (+) 0i32 xs, scan (*) 1i32 xs)) xss)
