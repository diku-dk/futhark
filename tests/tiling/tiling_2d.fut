-- Simple 2D tiling
-- ==
-- structure gpu { SegMap/SegMap 4
-- SegMap/Loop/SegMap 3
-- SegMap/SegMap 4
-- SegMap/Loop/Loop/SegMap/Loop 0 }

def main (xs: [][]i32) (ys: [][]i32) =
  map (\xs' -> map (\ys' -> #[sequential] i32.sum (map2 (*) xs' ys')) ys) xs
