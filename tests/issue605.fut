let main (xs: *[][]i32) =
  let xs_1 = copy xs[1]
  let xs[0] = xs_1
  in xs
