-- Simple 2D tiling
-- ==
-- structure gpu { SegMap/DoLoop/DoLoop/SegMap 4
-- SegMap/DoLoop/DoLoop/DoLoop/SegMap 2
-- SegMap/SegMap 1
-- SegMap/DoLoop/DoLoop/SegMap/DoLoop 3 }

let main (xs: [][]i32) (ys: [][]i32) =
  map (\xs' -> map (\ys' -> i32.sum (map2 (*) xs' ys')) ys) xs
