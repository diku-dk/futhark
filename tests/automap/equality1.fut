-- ==
-- entry: bigger_to_smaller
-- compiled input { [[1,2],[3,4]] [1,2] }
-- output { [true, false] }

-- ==
-- entry: smaller_to_bigger
-- compiled input { [[1,2],[3,4]] [1,2] }
-- output { [true, false] }

-- ==
-- entry: smaller_to_bigger2
-- compiled input { [[1,2],[3,4]] 1 }
-- output { [[true,false],[false,false]]}

entry bigger_to_smaller [n] (xss : [n][n]i32) (ys: [n]i32) : [n]bool =
  xss == ys

entry smaller_to_bigger [n] (xss : [n][n]i32) (ys: [n]i32) : [n]bool =
  ys == xss

entry smaller_to_bigger2 [n] (xss : [n][n]i32) (z: i32) : [n][n]bool =
  z == xss
