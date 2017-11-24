-- Tragic problem with index functions.
-- ==
-- input { true 1 2 [1,2,3] } output { [1,2] }
-- input { false 1 2 [1,2,3] } output { [1] }

let main (b: bool) (n: i32) (m: i32) (xs: []i32) =
  if b then xs[0:m] else xs[0:n]
