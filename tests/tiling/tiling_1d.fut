-- Simple 1D tiling
-- ==
-- compiled random input { [100]i32 } auto output
-- structure distributed { SegMap/DoLoop/SegMap 2 }

let main (xs: []i32) =
  map (\x -> i32.sum (map (+x) xs)) xs
