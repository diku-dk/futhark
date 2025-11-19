-- ==
-- entry: main
-- input {  [1,2,3] [4,5,6] }
-- output { [1,2,3,4,5,6] }

entry main [n] (xs: [n]i32) (ys: [n]i32) : []i32 =
  let xsys = zip xs ys
  in (.0) xsys ++ (.1) xsys
  
