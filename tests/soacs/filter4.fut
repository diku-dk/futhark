-- The array produced by filter should be unique.

let main (xs: *[]i32) =
  let xs' = filter (\x -> x>0) xs
  let xs[0] = 0
  in xs'
