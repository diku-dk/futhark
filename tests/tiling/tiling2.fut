-- Simple 2D tiling
-- ==
-- structure distributed { SegMap/DoLoop/SegMap 2 }

let main (xs: [][]i32) (ys: [][]i32) =
  map (\xs' -> map (\ys' -> i32.sum (map2 (*) xs' ys')) ys) xs
