-- ==
-- entry: main
-- input { [[1,2],[3,4]] [1,1] }
-- output { [[2,3],[4,5]] }

entry main [n] (xss : [n][n]i32) (ys: [n]i32) : [n][n]i32 =
  ys + xss
  
