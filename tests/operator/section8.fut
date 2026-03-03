-- sections support mixed field+index access paths.
-- ==
-- input { [10,20] [1,2] } output { 10 2 }

def main (xs: []i32) (ys: []i32) =
  let r = {a = xs, b = ys}
  in ( (.a.[0]) r
     , (.b.[1]) r
     )
