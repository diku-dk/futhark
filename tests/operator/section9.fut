-- sections support field+slice access paths.
-- ==
-- input { [10,20,30,40] [1,2,3,4] } output { [20,30] [1,2] }

def main (xs: []i32) (ys: []i32) =
  let r = {a = xs, b = ys}
  in ( (.a.[1:3]) r
     , (.b.[0:2]) r
     )
