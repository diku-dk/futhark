-- ==
-- input { [1,2,3] }
-- output { 3i64 }

type^ f = (k: i64) -> [k]i32 -> i64

def f : f = \n (xs: [n]i32) -> length xs

def main [K] (input: [K]i32) =
  f K input
