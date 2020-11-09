-- It is an error to impose two different names on the same dimension
-- in a function parameter.
--
-- ==
-- error: do not match

let f [n][m] ((_, elems: [n]i32): (i32,[m]i32)): i32 =
  n + m + elems[0]

let main (x: i32, y: []i32): i32 = f (x, y)
