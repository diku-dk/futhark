entry main (xs: []i32, ys: []i32) =
  map (\x -> let d = x + 3 in map (\y -> y + d) ys) xs

