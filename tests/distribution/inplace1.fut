-- Ensure that in-place updates with invariant indexes/values are
-- distributed sensibly.
-- ==
-- input { [[1,2], [3,4]] }
-- output { [[0,2], [0,4]] }

let main (xss: *[][]i32) =
  map (\(xs: []i32) -> copy xs with [0] = 0) xss
