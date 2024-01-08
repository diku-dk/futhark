-- ==
-- input { [true] [2] }
-- output { 1i64 }

def main [n] (xs: ?[m].[m]bool) (ys: [n]i32) = length (zip xs ys)
