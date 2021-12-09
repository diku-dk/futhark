-- Does it work to have a definition that only has a size parameter?
-- ==
-- input { 3i64 } output { [0i64,1i64,2i64] }

def iiota [n] : [n]i64 = iota n

def main x = copy iiota : [x]i64
