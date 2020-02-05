-- Size-variant branches don't have just any size.
-- ==
-- error: \[n\].*\[m\]

let main (b: bool) (n: i32) (m: i32) : [2]i32 =
  if b then iota n else iota m
