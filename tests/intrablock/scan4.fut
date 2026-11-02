-- Intra-block scan where the block must be virtualised, horizontally fused
-- with a map.
-- ==
-- random input { [1][1000]i32 } auto output
-- compiled random input { [10][1000]i32 } auto output
-- compiled random input { [10][257]i32 } auto output
-- structure gpu { SegMap/SegScan 1 }

def main xss =
  #[flattening(only_intra)]
  map (\xs ->
         let ys = scan (+) 0i32 xs
         in map2 (\y x -> y * 2 + x) ys xs)
      xss
