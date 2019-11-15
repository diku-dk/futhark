-- 2D tiling with extra dimensions on top.
-- ==
-- compiled random input { [2][40][40]i32 [2][40][40]i32 } auto output
-- structure distributed { SegMap/DoLoop/SegMap 2 }

let main [a][b][c] (xss: [a][b][c]i32) (yss: [a][b][c]i32) =
  map2 (\xs ys -> map (\xs' -> map (\ys' -> i32.sum (map2 (*) xs' ys')) ys) xs) xss yss
