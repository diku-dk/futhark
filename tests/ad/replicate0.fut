-- ==
-- entry: f_jvp
-- input { 3i64 2 }
-- output { [1,1,1] }

entry f_jvp n x : []i32 =
  jvp (replicate n) x 1

-- ==
-- entry: f_vjp
-- input { 3i64 2i64 }
-- output { 3i64 }

entry f_vjp n x =
  vjp (replicate n) x (iota n)
