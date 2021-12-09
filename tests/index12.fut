-- Simplifying away a slice of a rotate is not so simple.
-- ==
-- input { [1,2,3] }
-- output { [3,1] }

def main (xs: []i32) =
  let ys = rotate (-1) xs
  in ys[0:2]
