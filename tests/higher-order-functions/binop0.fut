-- Test of a higher-order infix operator that takes two functions as
-- arguments and returns a function as result.
-- ==
-- input { 7 12 } output { 8 24 }

def (***) '^a '^b '^a' '^b' (f: a -> a') (g: b -> b') : (a, b) -> (a', b') =
  \(x: a, y: b) -> (f x, g y)

def main (x: i32) (y: i32) =
  ((\(x: i32) -> x + 1) *** (\(y: i32) -> y + y)) (x, y)
