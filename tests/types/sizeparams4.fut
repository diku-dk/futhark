-- Shadowing of size parameters.
-- ==
-- input { 0i64 } output { empty([0]i64) }
-- input { 3i64 } output { [0i64,1i64,2i64] }

let n = 2i64

type ints [n] = [n]i64

let main(n: i64): ints [n] = iota n
