-- An operator section may leave out a consuming parameter.
-- ==
-- input { [1, 2, 3] } output { [0, 2, 3] }

def (>->) (i: i32) (xs: *[]i32) : *[]i32 = xs with [0] = i

def main (ys: *[]i32) = (0 >->) ys
