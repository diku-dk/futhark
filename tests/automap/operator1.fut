-- ==
-- entry: main
-- input { [[1,2],[3,4]] [10,20] }
-- output { [[11, 22],[13, 24]] }

def (+^) [n] (xs: [n]i32) (ys: [n]i32) : [n]i32 = xs + ys

--entry main [n] (xss : [n][n]i32) (ys: [n]i32) : [n][n]i32 =
--  xss +^ ys
