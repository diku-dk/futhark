-- ==
-- tags { autodiff }
-- input { [[1,2],[3,4]] }
-- output { [[1, 3], [2, 4]] }

entry main [n] (xss: *[n][n]i32) = vjp transpose xss xss
