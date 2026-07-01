-- ==
-- input { [5i64,7i64] }
-- output { [20i64, 35i64] }

def main = map (\n -> i64.sum (map (+2) (iota n)))
