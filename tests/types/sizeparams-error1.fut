-- Too many arguments.
-- ==
-- error: ints

type ints [n] = [n]i32

let main(n: i32): ints [1][2] = iota n
