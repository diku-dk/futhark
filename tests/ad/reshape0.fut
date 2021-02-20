-- ==
-- entry: f_jvp
-- compiled input { 2i64 2i64 [1,2,3,4] }
-- output { [[1,2],[3,4]] }

entry f_jvp n m (xs: []i32) =
 jvp (unflatten n m) xs xs

-- ==
-- entry: f_vjp
-- compiled input { 2i64 2i64 [1,2,3,4] }
-- output { [1,2,3,4] }

entry f_vjp n m (xs: []i32) =
 vjp (unflatten n m) xs (unflatten n m xs)
