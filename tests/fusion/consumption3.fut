-- Not fusible.
-- ==
-- structure { Screma 2 }

def main (xs: *[]i32) =
  let ys = map (+ 2) xs
  let xs[0] = 2
  let zs = map (* xs[1]) ys
  in (xs, zs)
