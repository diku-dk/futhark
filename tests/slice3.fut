-- Slicing produces a size that we can obtain.
-- ==
-- input { [1,2,3] 0 1 } output { 1 }

let main (xs: []i32) (i: i32) (j: i32) =
  length xs[i:j]
