-- ==
-- input { 2i64 2i64 }
-- output { [[0i64, 1i64], [2i64, 3i64]] }
-- input { -2i64 -2i64 }
-- error: Cannot unflatten.*\[-2\]\[-2\]

def main n m = unflatten (iota (n*m))
