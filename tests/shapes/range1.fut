-- Other ranges do not have known sizes.
-- ==
-- error: unknown length of range

let main (n: i64) : [n]i32 =
  1..<n+1
