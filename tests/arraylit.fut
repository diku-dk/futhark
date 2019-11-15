-- Array literals should work even if their safety cannot be
-- determined until runtime.
--
-- ==
-- input { 2 2 } output { [[0,1], [3, 3]] }
-- input { 2 3 } error: Error

let main (n: i32) (m: i32): [][]i32 =
  [iota n, replicate m 3 :> [n]i32]
