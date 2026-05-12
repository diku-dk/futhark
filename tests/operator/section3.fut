-- Operator section with interesting type to the left.
-- ==
-- input { [-1,1] } output { [1] }

def (<*>) 'a 'b (f: a -> b) (xs: a) =
  f xs

def main (xs: []i32) =
  (id <*>) (filter (> 0) xs)
