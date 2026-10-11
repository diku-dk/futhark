-- A held value that aliases nothing does not prevent consumption.
-- ==
-- input { [1, 2, 3] } output { 1 [5, 2, 3] }

def main (xs: *[]i32) = (xs[0], xs with [0] = 5)
