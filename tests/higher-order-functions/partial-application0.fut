-- Basic partial application.
-- ==
-- input { 3 7 2 } output { 12 }
-- input { -2 5 1 } output { 4 }

def f (x: i32) (y: i32) (z: i32) : i32 = x + y + z

def main (x: i32) (y: i32) (z: i32) =
  let g = f x y
  in g z
