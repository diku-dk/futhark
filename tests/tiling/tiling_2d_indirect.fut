-- 2D tiling, but where the arrays are variant to an outermost third dimension.
-- ==
-- structure gpu { SegMap/SegMap 4
-- SegMap/Loop/SegMap 3
-- SegMap/SegMap 4
-- SegMap/Loop/Loop/SegMap/Loop 0 }


def main (is: []i32) (js: []i32) (xss: [][][]i32) (yss: [][][]i32) =
  map2 (\i j -> map (\xs' -> map (\ys' -> i32.sum (map2 (*) xs' ys')) (#[unsafe] yss[j])) (#[unsafe] xss[i])) is js
