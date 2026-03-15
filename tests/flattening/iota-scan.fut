-- ==
-- input { [3i64,4i64,5i64] [1i64,2i64,3i64] }
-- auto output
-- input { [1i64,2i64] [0i64,3i64] }
-- error: out of bounds

def main = 
map2 (\n (i: i64) -> (scan (+) 0 (iota n))[i])