def f xs ys =
  map (\(x,y) -> (x*y, x+y)) (zip xs ys)
