-- ==
-- input { [1i64,2i64] [0i64,1i64] }
-- output { [0i64,1i64] }
-- input { [1i64,5i64] [0i64,3i64] }
-- output { [0i64,7i64] }
-- input { [1i64,2i64] [0i64,3i64] }
-- error: out of bounds
-- input { [1i64,-2i64] [0i64,1i64] }
-- error: Range 0..1..<-2 is invalid

def main = map2 (\n (i:i64) -> i64.sum (opaque (iota n))[i:])
