-- Map-reduce inside group.
-- ==
-- random input { [1][256]i32 } auto output
-- random input { [100][256]i32 } auto output
-- structure gpu { SegMap/SegRed 1 }

def main xs =
  #[incremental_flattening(only_intra)]
  map (map i32.abs >-> i32.sum) xs
