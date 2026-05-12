-- ==
-- input { 2i64 [1,2] }
-- output { [1,2] }
-- compiled input { 1i64 [1,2] }
-- error:

def f (n: i64) (xs: [n]i32) : [n]i32 = xs

def main (n: i64) (xs: []i32) = f n xs
