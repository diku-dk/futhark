-- Distinct components of one variable as consumed and observed arguments.
-- ==
-- input { [0] [5] }
-- output { 15 }

def f (a: *[]i32) (b: []i32) : i32 = let a[0] = 10 in a[0] + b[0]

def g (p: *([]i32, []i32)) : i32 = f p.0 p.1

def main (xs: *[]i32) (ys: *[]i32) = g (xs, ys)
