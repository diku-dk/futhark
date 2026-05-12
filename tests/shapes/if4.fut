-- Differing sizes, but the same across a single branch.
-- ==
-- input { false } output { [1,2] [3,4] }

def main (b: bool) =
  let (xs, ys) =
    if b
    then ([1, 2, 3], [4, 5, 6])
    else ([1, 2], [3, 4])
  in unzip (zip xs ys)
