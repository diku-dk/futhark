def add (x, y) z = (x+z, y+z)

entry f xs =
  map (\(x,y) -> add (x,y) 2) xs
