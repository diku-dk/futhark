-- This is not OK, because it would imply consuming the original
-- non-unique array.
-- ==
-- error: consumable

def polyid 't (x: t) = x

def main (xs: []i32) =
  let ys = polyid xs
  let ys[0] = 42
  in ys
