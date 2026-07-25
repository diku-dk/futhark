-- ==
-- input { [1,2,3] [4,5,6] }
-- output { [5,7,9] }
-- structure gpu { SegMap 1 }

def main = map2 (i32.+)
