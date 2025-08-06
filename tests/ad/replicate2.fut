-- ==
-- entry: f_jvp
-- input { 2i64 3i64 2 }
-- output { [[1,1,1],[1,1,1]] }

entry f_jvp n m x : [][]i32 =
  jvp (replicate m >-> replicate n) x 1

-- ==
-- entry: f_vjp
-- input { 2i64 3i64 2i64 }
-- output { 6i64 }

entry f_vjp n m x =
  vjp (replicate m >-> replicate n) x (replicate n (iota m))
