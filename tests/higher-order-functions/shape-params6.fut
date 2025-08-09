-- ==
-- input { [1,2] [3,4,5] }
-- output { 5i64 }

def f [n] [m] (f: [n + m]i32 -> i64) (a: [n]i32) (b: [m]i32) = f (a ++ b)

def g n m (_: [n + m]i32) = n + m

def main = f (g 2 3)
