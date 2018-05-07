-- Fusion would sometimes eat certificates on reshapes.
-- ==
-- input { 1 [1] }
-- output { [4] }
-- input { 2 [1] }
-- error:

let main (n: i32) (xs: []i32) =
  map (+2) (map (+1) (xs: [n]i32))
