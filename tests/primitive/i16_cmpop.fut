-- Test comparison of i16 values.
--
-- ==
-- input {  0i16  0i16 } output { false true true }
-- input {  1i16  2i16 } output { true false true }
-- input { -1i16  1i16 } output { true false true }
-- input {  1i16 -1i16 } output { false false false }
-- input { -2i16 -1i16 } output { true false true }

fun main(x: i16, y: i16): (bool, bool, bool) =
  (x < y, x == y, x <= y)
