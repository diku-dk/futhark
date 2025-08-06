-- Can a constant be an array of tuples?
--
-- ==
-- input {} output { 3 }

def v : [](i32, i32) = [(1, 2)]

def main : i32 =
  let (x, y) = v[0]
  in x + y
