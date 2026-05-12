-- Test of an infix operator that takes one zero order argument and
-- one functional argument, and returns a function.
-- ==
-- input { 7 5 } output { 19 }

def (**) (x: i32) (f: i32 -> i32) : i32 -> i32 =
  \(y: i32) -> f x + y

def main (x: i32) (y: i32) =
  (x ** (\(z: i32) -> z + z)) y
