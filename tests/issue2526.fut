-- If the simplification rule fires, this entire program should compile away.
-- ==
-- structure { Match 0 Assert 0 }

entry main (b: bool) (xs: []i32) =
  let (a, b) =
    if b
    then let ys = filter (> 0) xs
         in (length ys, length ys)
    else let ys = filter (< 0) xs
         in (length ys, length ys)
  in assert (a == b) true
