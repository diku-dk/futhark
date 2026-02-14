-- ==
-- input { [0i64,1i64,2i64] }
-- output { [0i64, 0i64, 1i64] }
-- input { [0i64,1i64,-2i64] }
-- error: Range 0..1..<-2 is invalid

def main = map (\n -> i64.sum (iota n))
