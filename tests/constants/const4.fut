-- You can use a constant as a shape declaration in another constant.
--
-- ==
-- input { } output { [0,0,0] }

def n : i64 = 3

def x : [n]i32 = replicate n 0

def main : []i32 = copy x
