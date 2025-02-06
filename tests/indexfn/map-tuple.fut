def add (x, y) (z,a) : {(i64,i64) | \_ -> true} = (x+z, y+a)

def f xs ys : {[](i64,i64) | \_ -> true} =
  map (\(x, y) -> add (x,y) (2,3)) (zip xs ys)
