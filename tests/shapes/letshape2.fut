-- It is an error to impose two different names on the same dimension
-- in a let-binding.
--
-- ==
-- error: Cannot match

let main (x: i32, y: []i32): i32 =
  let [n][m] ((_, elems: [n]i32): (i32,[m]i32)) = (x,y)
  in n + m + elems[0]
