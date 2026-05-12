def main (xs: *[]i32) =
  let xs' = zip xs xs
  let xs'[0] = (0, 0)
  in xs'
