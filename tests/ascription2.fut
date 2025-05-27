-- Array type ascription.
--
-- ==
-- input { [[1,2],[3,4]] 2i64 2i64 } output { [[1,2],[3,4]] }
-- input { [[1,2],[3,4]] 1i64 4i64 } error: cannot match shape of type.*"\[1\]\[4\]

def main [n] [m] (x: [n][m]i32) (a: i64) (b: i64) = x :> [a][b]i32
