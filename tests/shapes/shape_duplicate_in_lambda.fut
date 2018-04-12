-- It is an error to impose two different names on the same dimension
-- in a lambda.
--
-- ==
-- error: Cannot match

let main (xss: [][]i32): []i32 =
  map (\[m][n] ((_xs: [m]i32): [n]i32): i32 -> n + m) xss
