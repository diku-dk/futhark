-- A size parameter can be a constant type.
-- ==
-- input { 0 } error: Error
-- input { 3 } output { [0,1,2] }

type ints [n] = [n]i32

let main (n: i32) = iota n :> ints [3]
