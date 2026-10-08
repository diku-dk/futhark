-- A function application consumes its arguments after evaluating all of them,
-- so an argument evaluated after a consumed one may still use it.
-- ==
-- input { [1, 2, 3] } output { [2, 2, 3] }

def f (a: i32) (b: *[]i32) = b with [0] = a + 1

def main (xs: *[]i32) = f xs[0] xs
