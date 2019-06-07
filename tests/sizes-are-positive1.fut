-- ==
-- structure { Assert 0 }

let main [n] [m] (xs: [n]i32) (ys: [m]i32) =
  let k = i32.max n m
  in assert ((k >= 0) && !(k < 0)) xs
