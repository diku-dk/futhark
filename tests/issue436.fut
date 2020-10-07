-- Fusion would sometimes eat certificates on reshapes.
-- ==
-- input { 1i64 [1] }
-- output { [4] }
-- input { 2i64 [1] }
-- error:

let main (n: i64) (xs: []i32) =
  map (+2) (map (+1) (xs: [n]i32))
