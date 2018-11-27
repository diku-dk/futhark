-- ==
-- error: consumption

let update (xs: *[]i32) (i: i32) (y: i32) =
  xs with [i] = y

let main (arr: *[]i32) =
  let f = update arr
  in arr
