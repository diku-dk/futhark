-- ==
-- input { [5i64,7i64] }
-- output { [20i64, 35i64] }
-- structure gpu { /If/True/SegMap 1 /If/True/SegRed 0
--                 /If/False/SegMap 2 /If/False/Apply/segiota 1
--                 /If/False/SegScan 1 }

def main = map (\n -> i64.sum (map (+ 2) (iota n)))
