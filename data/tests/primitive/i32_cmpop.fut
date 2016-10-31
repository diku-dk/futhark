-- Test comparison of i32 values.
--
-- ==
-- input {  0i32  0i32 } output { false true true }
-- input {  1i32  2i32 } output { true false true }
-- input { -1i32  1i32 } output { true false true }
-- input {  1i32 -1i32 } output { false false false }
-- input { -2i32 -1i32 } output { true false true }

fun main(x: i32, y: i32): (bool, bool, bool) =
  (x < y, x == y, x <= y)
