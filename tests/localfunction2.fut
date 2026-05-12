-- The same name for a local function in two places should not cause
-- trouble.
-- ==
-- input { 3 } output { 6 0 }

def f1 (x: i32) =
  let g (y: i32) = x + y
  in g x

def f2 (x: i32) =
  let g (y: i32) = x - y
  in g x

def main (x: i32) =
  (f1 x, f2 x)
