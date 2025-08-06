-- No space is needed before the size argument.
-- ==
-- input { 2i64 } output { [0i64,1i64] }

type ints [n] = [n]i64

def main (n: i64) : ints [n] = iota n
