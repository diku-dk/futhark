-- Other ranges do not have known sizes.
-- ==
-- error: unknown length of range

let main (n: i32) : [n]i32 =
  1..<n+1
