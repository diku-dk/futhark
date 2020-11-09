-- This is not OK, because it would imply consuming the original
-- non-unique array.
-- ==
-- error: consumable

let polyid 't (x: t) = x

let main (xs: []i32) =
  let ys = polyid xs
  let ys[0] = 42
  in ys
