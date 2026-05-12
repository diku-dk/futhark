-- ==
-- input { 2i64 2i64 } output { [0i64, 1i64] }
-- input { 2i64 3i64 } error:

def main (n: i64) (m: i64) : [m]i64 = iota n :> [m]i64
