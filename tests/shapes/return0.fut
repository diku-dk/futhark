-- ==
-- input { 2 2 } output { [0, 1] }
-- input { 2 3 } error:

let main (n: i32) (m: i32): [m]i32 = iota n :> [m]i32
