-- Test that we can distinguish function application with literal
-- array argument from array indexing.
-- ==
-- input { 1 } output { 3 }

def f (xs: []i32) : i32 = xs[0]

def a : []i32 = [1, 2, 3]

def main (x: i32) : i32 =
  f [x] + a[x]
