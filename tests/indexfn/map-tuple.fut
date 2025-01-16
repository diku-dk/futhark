def add (x, y) (z,a) = (x+z, y+a)

def f xs ys =
  map (\(x, y) -> add (x,y) (2,3)) (zip xs ys)
