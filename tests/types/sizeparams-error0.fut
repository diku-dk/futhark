-- Too few arguments.
-- ==
-- error: ints

type ints [n] = [n]i32

def main (n: i32) : ints = iota n
