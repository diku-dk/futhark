-- Range with known and wrong size.
-- ==
-- error: n\+1

let main (n: i32) : [n]i32 =
  0..<(n+1)
