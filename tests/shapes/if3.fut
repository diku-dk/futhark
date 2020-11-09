-- Size-variant branches don't have just any size.
-- ==
-- error: \[n\].*\[m\]

let main (b: bool) (n: i64) (m: i64) : [2]i64 =
  if b then iota n else iota m
