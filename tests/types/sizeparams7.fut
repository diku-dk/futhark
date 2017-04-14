-- No space is needed before the size argument.
-- ==
-- input { 2 } output { [0,1] }

type ints #n = [n]i32

let main (n:i32): ints[n] = iota n
