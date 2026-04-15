-- Map-scan inside group.
-- ==
-- random input { [1][256]i32 } auto output
-- random input { [100][256]i32 } auto output
-- structure gpu { SegMap/SegScan 1 }

def main xs =
  #[incremental_flattening(only_intra)]
  map (map i32.abs >-> scan (+) 0) xs
