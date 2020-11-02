-- Make sure ascribed names are available.
--
-- ==
-- input { 2 [1i64,2i64,3i64] }
-- output { 4i64 }

let f [n] ((_, elems: []i64): (i32,[n]i64)) =
  n + elems[0]

let main [n] (x: i32) (y: [n]i64) =
  f (x,y)
