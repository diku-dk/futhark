-- Defunctionalization should not leave the shape parameter on lifted
-- functions whose parameters do not refer to the shape, and it should
-- preserve (or introduce new shape parameters) on lifted functions
-- when necessary.
-- ==
-- input { [2,3,5,1] [6,5,2,6] } output { [8,8,7,7] }

def map2 'a 'b 'c [m] (f: a -> b -> c) (xs: [m]a) (ys: [m]b) : [m]c =
  map (\(x, y) -> f x y) (zip xs ys)

def add (x: i32) (y: i32) = x + y

def main (xs: []i32) (ys: []i32) =
  map2 add xs ys
