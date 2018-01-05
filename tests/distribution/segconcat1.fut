-- Nested concatenation with more arrays.
-- ==
-- input { [[1,2],[3,4],[5,6]] [[1,2],[3,4],[5,6]] [[1,2],[3,4],[5,6]] }
-- output { [[1,2,1,2,1,2], [3,4,3,4,3,4], [5,6,5,6,5,6]] }
-- structure distributed { Kernel 0 }

let main (xss: [][]i32) (yss: [][]i32) (zss: [][]i32) =
  map (\(xs, ys, zs) -> concat xs ys zs) (zip xss yss zss)