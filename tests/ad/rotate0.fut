-- ==
-- entry: f_jvp
-- input { 1i64 [1,2,3,4] }
-- output { [2,3,4,1] }

entry f_jvp k (xs: []i32) =
  jvp (rotate k) xs xs

-- ==
-- entry: f_vjp
-- input { 1i64 [1,2,3,4] }
-- output { [1,2,3,4] }

entry f_vjp k (xs: []i32) =
  vjp (rotate k) xs (rotate k xs)
