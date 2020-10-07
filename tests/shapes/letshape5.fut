-- A size goes out of scope.
-- ==
-- error: "m"

let main (n: i64) : [n]i32 =
  let m = n
  in iota m
