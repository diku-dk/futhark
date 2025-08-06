-- A higher-order function with a shape parameter that contains a
-- local dynamic function which is used as a first class value and
-- which refers to the outer shape parameter in its parameter type
-- and in its body.
-- ==
-- input { [2,3,5,1] [6,5,2,6] } output { [8,8,7,7] 4i64 }

def map2 [n] (f: i32 -> i32 -> i32) (xs: [n]i32) =
  let g (ys: [n]i32) = (map (\(x, y) -> f x y) (zip xs ys), n)
  in g

def add (x: i32) (y: i32) = x + y

def main (xs: []i32) (ys: []i32) =
  map2 add xs ys
