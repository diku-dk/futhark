-- ==
-- input { 2 } output { 2 empty([0][1]i32) }

def empty (d: i64) (x: i32) : (i32, [0][d]i32) = (x, [])

def main (x: i32) : (i32, [][1]i32) = empty 1 x
