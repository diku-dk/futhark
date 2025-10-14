-- ==
-- input { [1,2] } output { [1,2,2,3] }

def flatmap [n] [m] 'a 'b (f: a -> [m]b) (as: [n]a) : []b =
  flatten (map f as)

def main (xs: []i32) = flatmap (\x -> [x, x + 1]) xs
