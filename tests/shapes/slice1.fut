-- ==
-- error: do not match

let main [n] [m] (xs: [n]i32) (ys: [m]i32) =
  zip xs[1:] ys[1:]
