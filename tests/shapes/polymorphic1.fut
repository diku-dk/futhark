-- Arrays passed for polymorphic parameters of the same type must have
-- the same size.
-- ==
-- input { [1] [2] } output { [1] [2] }
-- compiled input { [1] [2,3] } error:

def pair 't (x: t) (y: t) = (x, y)

def main (xs: []i32) (ys: []i32) = pair xs ys
