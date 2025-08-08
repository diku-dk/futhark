-- Defunctionalisation should also look at dimensions used in type
-- ascriptions.

def f (ys: []i32) = map (+ 1) ys

def main [n] (xs: [n]i32) =
  let g (h: []i32 -> []i32) = copy (h xs)
  let g' = \x -> g (\y -> (x y : [n]i32))
  in g' f
