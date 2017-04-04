-- Make sure inner shape names are available, even if they are
-- "shadowed" by an outer named shape, even in a lambda.
--
-- ==
-- input { [[1,2],[3,4]] }
-- output { [4, 4] }

let main (xss: [][]i32): []i32 =
  map (\((_xs: [m]i32): [n]i32): i32 -> n + m) xss
