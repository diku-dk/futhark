-- ==
-- error: consume variable "s"

let f (xs: [10]i32) : [10]i32 = xs

let main (s: [10]i32) : *[10]i32 =
  let s = f s
  let s = loop s for _i < 10 do f s
  in s with [0] = 0
