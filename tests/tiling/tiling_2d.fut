-- Simple 2D tiling
-- ==
-- structure gpu { SegMap/SegMap 4
-- SegMap/DoLoop/SegMap 2
-- SegMap/SegMap 3
-- SegMap/DoLoop/DoLoop/SegMap/DoLoop 3 }

def main (xs: [][]i32) (ys: [][]i32) =
  map (\xs' -> map (\ys' -> i32.sum (map2 (*) xs' ys')) ys) xs
