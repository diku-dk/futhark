-- ==
-- entry: fwd rev
-- input { 1 2 } output { 1 }

def d f x =
  jvp f x 1

def drev f x =
  vjp f x 1

entry fwd x y =
  d (\x' -> (d (x'*) y)) x

entry rev x y =
  drev (\x' -> (drev (x'*) y)) x
