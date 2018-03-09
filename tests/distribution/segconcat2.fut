-- Nested concatenation with an invariant part.
-- ==
-- input { [[1,2],[3,4]] }
-- output { [[1, 2, 2, 2], [3, 4, 2, 2]]}
-- structure distributed { Kernel 0 }

let main (xss: [][]i32) =
  map (\xs -> concat xs (replicate 2 2)) xss
