-- Too few arguments.
-- ==
-- error: ints

type ints [n] = [n]i32

fun main(n: i32): ints = iota n
