-- ==
-- error: consumption

let update (xs: *[]i32) (i: i32) (y: i32) =
  xs with [i] = y

let main (QUUX: *[]i32)=
  let f = update QUUX
  in (f 0 0, f 0 0)
