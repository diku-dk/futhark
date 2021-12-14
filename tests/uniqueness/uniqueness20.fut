-- It is fine to do an in-place update on something returned by a
-- function that has aliases.
-- ==
-- input { [1, 2, 3] } output { [42, 2, 3] }

def id (xs: []i32) = xs

def polyid 't (x: t) = x

def main (xs: *[]i32) =
  let ys = id xs
  let ys = polyid xs
  let ys[0] = 42
  in ys
