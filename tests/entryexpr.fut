-- ==
-- input { [1,2] [3] }
-- output { [1,2,3] }

def main [n] (xs: [n * 2]i32) (ys: [n]i32) = xs ++ ys
