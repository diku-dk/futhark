-- ==
-- input { 0i64 32i64 empty([0]i32) }
-- output { empty([32][0]i32) }
-- input { 32i64 0i64 empty([0]i32) }
-- output { empty([0][32]i32) }

def main (n: i64) (m: i64) (xs: []i32) = transpose (unflatten n m xs)
