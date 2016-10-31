-- Test comparison of i64 values.
--
-- ==
-- input {  0i64  0i64 } output { false true true }
-- input {  1i64  2i64 } output { true false true }
-- input { -1i64  1i64 } output { true false true }
-- input {  1i64 -1i64 } output { false false false }
-- input { -2i64 -1i64 } output { true false true }

fun main(x: i64, y: i64): (bool, bool, bool) =
  (x < y, x == y, x <= y)
