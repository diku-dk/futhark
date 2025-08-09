-- ==
-- entry: f_jvp
-- input { 2i64 2i64 [1,2,3,4] }
-- output { [[1,2],[3,4]] }

entry f_jvp n m (xs: [n * m]i32) =
  jvp unflatten xs xs

-- ==
-- entry: f_vjp
-- input { 2i64 2i64 [1,2,3,4] }
-- output { [1,2,3,4] }

entry f_vjp n m (xs: [n * m]i32) =
  vjp unflatten xs (unflatten xs)
