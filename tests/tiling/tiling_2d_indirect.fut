-- 2D tiling, but where the arrays are variant to an outermost third dimension.
-- ==
-- structure distributed { SegMap/DoLoop/SegMap 2 }

let main (is: []i32) (js: []i32) (xss: [][][]i32) (yss: [][][]i32) =
  map2 (\i j -> map (\xs' -> map (\ys' -> i32.sum (map2 (*) xs' ys')) (unsafe yss[j])) (unsafe xss[i])) is js
