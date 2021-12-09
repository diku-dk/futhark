-- If a name is used as a size, then it's probably an i32!
-- ==
-- input { 3i64 [1,2,3] } output { [1,2,3] }

def main n (xs: [n]i32) = xs
