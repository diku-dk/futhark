def f =
  (\x ->
     x + 2)

def r =
  { x =
      \x -> x + 2
  }

def h =
  map3 (\x y z ->
          x + y + z)
       (iota 10)
       (iota 10)
       (iota 10)
