-- Two dimensions in the kernel, but we are only tiling along the
-- innermost one.
-- ==
-- input { [1,2,3] [[1,2,3],[4,5,6],[7,8,9]] [1,2,3] } auto output
-- structure gpu { SegMap/Loop/Loop/SegMap 2 }

def main (ns: []i32) (xs: [][]i32) (ys: []i32) =
  map (\n -> map (\y -> loop y for i < n do
                          #[sequential] i32.sum (map (+y) (#[unsafe] xs[i])))
                 ys)
      ns
