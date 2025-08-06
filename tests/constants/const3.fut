--
-- ==
-- input { } output { [0,0,0] }

def n : i64 = 3

def f () : [n]i32 = replicate n 0

def main : []i32 = f ()
