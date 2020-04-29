let main [n] (xs: [n]i32): i32 =
  let xs' = if n > 10
            then let ys = rotate 4 xs
                 in ys[1:5]
            else rotate 3 xs
  in reduce (+) 0 xs'
