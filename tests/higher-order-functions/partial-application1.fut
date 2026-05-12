-- Binding a function to a local variable before applying it.
-- ==
-- input { 3 } output { 6 }
-- input { 11 } output { 22 }

def f (x: i32) : i32 = x + x

def main (x: i32) =
  let g = f
  in g x
