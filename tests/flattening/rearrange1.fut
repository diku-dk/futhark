-- ==
-- input { [3i64,4i64] }
-- output { [1i64,1i64] }

def main = map (\n -> ((transpose (replicate (n+1) (iota n))))[1,1])
