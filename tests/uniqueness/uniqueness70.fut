-- A consumed tuple parameter may be returned as fresh.
-- ==
-- input { [1] [2] }
-- output { [1] [2] }

def f (p: *([]i32, []i32)) : *([]i32, []i32) = p

def main (xs: *[]i32) (ys: *[]i32) = f (xs, ys)
