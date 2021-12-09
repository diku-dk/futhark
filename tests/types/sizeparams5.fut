-- A size parameter can be a constant type.
-- ==
-- input { 0i64 } error: Error
-- input { 3i64 } output { [0i64,1i64,2i64] }

type ints [n] = [n]i64

def main (n: i64) = iota n :> ints [3]
