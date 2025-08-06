-- Test tuple patterns.
-- ==
-- input { 2 } output { 3 1 }

def f (x: i32) = {a = x + 1, b = x - 1}

def main (x: i32) =
  let {a, b = c} = f x
  in (a, c)
