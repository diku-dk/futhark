-- Simple 1D tiling
-- ==
-- compiled random input { [100]i32 } auto output
-- structure gpu { SegMap/Loop/SegMap 2 }

def main (xs: []i32) =
  map (\x -> #[sequential] i32.sum (map (+x) xs)) xs
