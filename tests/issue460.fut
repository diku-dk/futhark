-- ==
-- input { [1,2] [3,4] 2 }
-- output { [1, 2] }
-- input { [1,2] [3,4] 7 }
-- output { [3, 4] }

def main (xs: []i32) (ys: []i32) (n: i32) =
  map (\(x, y) -> (loop (x, y) for _i < n do (y, x)).0) (zip xs ys)
