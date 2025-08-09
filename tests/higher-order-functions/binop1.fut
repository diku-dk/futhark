-- Test of an infix operator that takes arguments of order 0, but
-- returns a function.
-- ==
-- input { 7 5 } output { 35 }

def (**) (x: i32) (y: i32) = \(f: i32 -> i32 -> i32) -> f x y

def main (x: i32) (y: i32) =
  (x ** y) (\(a: i32) (b: i32) -> a * b)
