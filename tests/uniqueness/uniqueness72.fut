-- Swapping the components of a consumed tuple yields a fresh tuple.
-- ==
-- input { [1] [2] }
-- output { [2] [1] }

def swap (p: *([]i32, []i32)) : *([]i32, []i32) = (p.1, p.0)

def main (xs: *[]i32) (ys: *[]i32) = swap (xs, ys)
