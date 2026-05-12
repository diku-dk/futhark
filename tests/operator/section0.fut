-- We can use operator sections anywhere, just like other functions.
-- ==
-- input { 5 3 } output { 2 2 -2 }

def (-^) (x: i32) (y: i32) = x - y

def main (x: i32) (y: i32) =
  ( (-^) x y
  , (x -^) y
  , (-^ x) y
  )
