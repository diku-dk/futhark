-- Two dimensions in the kernel, but we are only tiling along the
-- innermost one.
-- ==
-- input { [1,2,3] [[1,2,3],[4,5,6],[7,8,9]] [1,2,3] } auto output
-- structure distributed { SegMap/DoLoop/DoLoop/SegMap 2 }

let main (ns: []i32) (xs: [][]i32) (ys: []i32) =
  map (\n -> map (\y -> loop y for i < n do i32.sum (map (+y) (unsafe xs[i]))) ys) ns
