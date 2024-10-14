-- ==
-- entry: f_jvp
-- input { [[1,2],[3,4]] }
-- output { [[1,3],[2,4]] }

entry f_jvp (xss: [][]i32) =
 jvp transpose xss xss

-- ==
-- entry: f_vjp
-- input { [[1,2],[3,4]] }
-- output { [[1,3],[2,4]] }

entry f_vjp (xss: [][]i32) =
 vjp transpose xss xss
