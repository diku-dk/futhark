-- Shadowing of size parameters.
-- ==
-- input { 0 } output { empty(i32) }
-- input { 3 } output { [0,1,2] }

let n = 2

type ints [n] = [n]i32

fun main(n: i32): ints [n] = iota n
