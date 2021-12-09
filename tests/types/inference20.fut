-- For-in loop variable inference.
-- ==
-- input { [1,2,3] } output { 9 }

def main xs = (loop y = 1 for x in xs do y * 2) + xs[0]
