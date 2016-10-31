-- Test comparison of u16 values.
--
-- ==
-- input {  0u16  0u16 } output { false true true }
-- input {  1u16  2u16 } output { true false true }
-- input { 65535u16 1u16 } output { false false false }
-- input {  1u16 65535u16 } output { true false true }

fun main(x: u16, y: u16): (bool, bool, bool) =
  (x < y, x == y, x <= y)
