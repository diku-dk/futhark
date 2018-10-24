-- ==
-- error: consumption

let update (xs: *[]i32) (x: i32) : *[]i32 =
  xs with [0] = x

let apply (f: i32->[]i32) (x: i32) : []i32 =
  f x

let main (xs: *[]i32) =
  apply (update xs) 2
