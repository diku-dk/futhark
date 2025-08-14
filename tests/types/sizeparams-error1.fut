-- Too many arguments.
-- ==
-- error: ints

type ints [n] = [n]i32

def main (n: i32) : ints [1] [2] = iota n
