-- ==
-- error: consumption

let zero (xs: *[]i32) (i: i32) =
  xs with [i] = 0

let uniq (x: i32): *[]i32 = [x,x,x]

let main (x: i32)=
  let f = zero (uniq x)
  in f 0
