-- ==
-- error: Entry point

def empty 'a (x: i32) = (x, [] : [0]a)

def main x : (i32, [][]i32) = empty x
