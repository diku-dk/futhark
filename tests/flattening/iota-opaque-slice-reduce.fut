-- ==
-- input { [1i64,3i64] [0,1] [1,2] }
-- output { [0i64,3i64] }

def main = map3 (\n i m -> i64.sum (opaque (iota n))[i:i+m])
