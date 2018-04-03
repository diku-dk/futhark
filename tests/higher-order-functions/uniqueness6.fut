-- Nope, this one is also not OK (although it would be possible to
-- change the type system so that it would be).
-- ==
-- error: consumption

let zero (xs: *[]i32) (i: i32) =
  xs with [i] <- 0

let apply f x = f x

let main (arr: *[]i32)=
  let f = zero arr
  in f 0
