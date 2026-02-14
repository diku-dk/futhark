-- iota is probably simplified away, but certs must be kept.
-- ==
-- input { [1i64,2i64] [0,1] }
-- output { [0i64,1i64] }
-- input { [1i64,2i64] [0,2] }
-- error: out of bounds
-- input { [1i64,-2i64] [0,1] }
-- error: Range 0..1..<-2 is invalid

def main = map2 (\n (i:i32) -> (iota n)[i])
