-- A consuming function must not be passed as a higher-order argument!
-- ==
-- error: consumption

let zero (xs: *[]i32) (i: i32) =
  xs with [i] = 0

let apply f x = f x

let main (arr: *[]i32)=
  let f = zero arr
  in apply f 0
