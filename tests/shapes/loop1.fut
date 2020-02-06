-- Loops impose sizes.
-- ==
-- error: \[10\]i32

let main [n] (xs: *[n]i32) : [10]i32 =
  loop xs for i < n do xs with [i] = xs[i] + 1
