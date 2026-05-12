-- Basic size-parameterised type.
-- ==
-- input { 0i64 } output { empty([0]i64) }
-- input { 3i64 } output { [0i64,1i64,2i64] }

type ints [n] = [n]i64

def main (n: i64) : ints [n] = iota n
