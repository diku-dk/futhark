-- ==
-- entry: bigger_to_smaller
-- input { [[1,2],[3,4]] [1,2] }
-- output { [[true, true], [false, false]] }

-- ==
-- entry: smaller_to_bigger
-- input { [[1,2],[3,4]] [1,2] }
-- output {  [[true, true], [false, false]] }

-- ==
-- entry: smaller_to_bigger2
-- input { [[1,2],[3,4]] 1 }
-- output { [[true,false],[false,false]]}

entry bigger_to_smaller [n] (xss : [n][n]i32) (ys: [n]i32) : [n][n]bool =
  xss == ys

entry smaller_to_bigger [n] (xss : [n][n]i32) (ys: [n]i32) : [n][n]bool =
  ys == xss

entry smaller_to_bigger2 [n] (xss : [n][n]i32) (z: i32) : [n][n]bool =
  z == xss
