-- ==
-- input { [true] [2] }
-- output { 1i64 }

def main [n] (xs: ?[n].[n]bool) (ys: [n]i32) = length (zip xs ys)
