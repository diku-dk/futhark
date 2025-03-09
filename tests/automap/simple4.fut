-- ==
-- entry: main
-- input {  3 [1,1] [[1,2],[3,4]] }
-- output { [[5,6],[7,8]] }

entry main [n] (x : i32) (ys: [n]i32) (zss : [n][n]i32) : [n][n]i32 =
  x + ys + zss
  
