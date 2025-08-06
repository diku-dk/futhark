-- Differentiating with respect to 'n' does not make much sense, but
-- it should at least not crash.

-- ==
-- entry: f_jvp
-- input { 3i64 2i64 }
-- output { [0i64,0i64,0i64] }

entry f_jvp n x =
  jvp (\n' -> replicate n' x :> [n]i64) n 1

-- ==
-- entry: f_vjp
-- input { 3i64 2i64 }
-- output { 0i64 }

entry f_vjp n x =
  vjp (\n' -> replicate n' x :> [n]i64) n (iota n)
