-- 2D tiling where the loop to tile is inside a sequential loop, that
-- is variant to an outermost dimension.
-- ==
-- input { [1,2,3] [[1,2,3],[4,5,6]] [[1,2,3],[4,5,6]] } auto output
-- structure gpu { SegMap/Loop/Loop/SegMap 2 }

def main [k] (ns: []i32) (xss: [][k]i32) (yss: [][k]i32) =
  map (\n -> map (\xs' -> map (\ys' -> loop z = 0 for _p < n do
                                         #[sequential] i32.sum (map (+z) (map2 (*) xs' ys')))
                              yss)
                 xss)
       ns
