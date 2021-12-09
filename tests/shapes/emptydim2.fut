-- ==
-- input { 1i64 empty([0]i32) } output { empty([1][0]i32) }
-- input { 0i64 [1]           } output { empty([0][1]i32) }
-- input { 0i64 empty([0]i32) } output { empty([0][0]i32) }

def main (n: i64) (xs: []i32) = replicate n xs
