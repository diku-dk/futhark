-- ==
-- input { 4 }
-- output { [1i32, 1i32, 1i32] [4i32, 4i32, 4i32] }

type^ t [n] = ([n]i32, i32 -> [n]i32)

def v : t [] = let three = 3 in (replicate three 1, \i -> replicate three i)

def main x = (copy v.0, v.1 x)
