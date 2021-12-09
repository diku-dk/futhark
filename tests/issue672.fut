def main (xs: *[]i32) =
  let x = xs[0]
  let xs[0] = xs[1]
  let xs[0] = x
  in xs
